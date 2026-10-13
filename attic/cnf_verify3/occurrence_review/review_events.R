# Independent observation of the unchanged simplifier. All input clauses and
# stored states retain occurrence positions. Run from the repository root.
suppressPackageStartupMessages(library(checkmate))
stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
map_chr = function(.x, .f, ...) vapply(.x, .f, character(1L), ...)
review_dir = "attic/cnf_verify3/occurrence_review"
cnf = new.env(parent = globalenv())
source_paths = file.path("R", paste0(c("CnfUniverse", "CnfSymbol", "CnfAtom",
  "CnfClause", "CnfFormula", "CnfFormula_simplify"), ".R"))
for (path in source_paths) sys.source(path, envir = cnf)
plain_simplify = cnf$simplify_cnf

review = new.env(parent = emptyenv())
review$counts = list()
review$examples = list()
count = function(key, amount = 1L) {
  value = review$counts[[key]]
  review$counts[[key]] = if (is.null(value)) amount else value + amount
}
snapshot = function(key, state = NULL, detail = NULL) {
  count(key)
  if (!is.null(review$examples[[key]])) return(invisible(NULL))
  review$examples[[key]] = list(words = review$words, label = review$label,
    detail = detail, entries = if (!is.null(state)) state$entries else NULL,
    eliminated = if (!is.null(state)) state$eliminated else NULL,
    registry = if (!is.null(state)) as.list(state$symbol_registry) else NULL)
  invisible(NULL)
}
require_review = function(ok, message, state = NULL, detail = NULL) {
  if (isTRUE(ok)) return(invisible(NULL))
  snapshot(paste0("FAIL: ", message), state, detail)
  saveRDS(list(counts = review$counts, examples = review$examples),
    file.path(review_dir, "failure.rds"))
  stop(message, call. = FALSE)
}

# Two independent evaluators: signed words and positional range lists.
truth_words = function(words, assignment) {
  if (!length(words)) return(rep(TRUE, nrow(assignment)))
  result = rep(TRUE, nrow(assignment))
  for (word in words) {
    satisfied = rep(FALSE, nrow(assignment))
    for (literal in word) {
      satisfied = satisfied | assignment[, abs(literal)] == (literal > 0L)
    }
    result = result & satisfied
  }
  result
}
truth_entries = function(entries, assignment) {
  if (is.logical(entries)) return(rep(as.vector(entries), nrow(assignment)))
  result = rep(TRUE, nrow(assignment))
  for (clause in entries) {
    satisfied = rep(FALSE, nrow(assignment))
    for (position in seq_along(clause)) {
      symbol_idx = as.integer(sub("^X", "", names(clause)[[position]]))
      value = ifelse(assignment[, symbol_idx], "yes", "no")
      satisfied = satisfied | value %in% clause[[position]]
    }
    result = result & satisfied
  }
  result
}
range_at = function(clause, symbol) {
  positions = which(names(clause) == symbol)
  unique(unlist(clause[positions], use.names = FALSE))
}

context = function(n) {
  universe = cnf$CnfUniverse()
  symbols = lapply(seq_len(n), function(i) {
    cnf$CnfSymbol(universe, paste0("X", i), c("no", "yes"))
  })
  assignment = as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), n)))
  list(universe = universe, symbols = symbols, assignment = assignment)
}
make_public = function(words, ctx, character_selector = FALSE) {
  lapply(words, function(word) {
    stopifnot(length(word) > 0L, !any(-word %in% word))
    unique_words = unique(word)
    base = cnf$CnfClause(lapply(unique_words, function(literal) {
      cnf$CnfAtom(ctx$symbols[[abs(literal)]], if (literal > 0L) "yes" else "no")
    }))
    selector = if (character_selector) paste0("X", abs(word)) else match(word, unique_words)
    result = cnf$`[.CnfClause`(base, matrix(selector, nrow = 1L))
    stopifnot(identical(names(result), paste0("X", abs(word))),
      identical(unname(c(result)), lapply(word, function(literal) if (literal > 0L) "yes" else "no")))
    result
  })
}

watch_semantics = function(state, label) {
  actual = truth_entries(state$entries[!state$eliminated], review$ctx$assignment)
  require_review(identical(actual, review$truth), paste("semantic boundary", label), state)
  count("semantic_boundaries")
}

watch_cache = function(state, label) {
  if (review$hla || is.null(state$is_not_subset_of)) return(invisible(NULL))
  live = which(!state$eliminated[state$available] & !state$is_unit[state$available] &
    lengths(state$entries[state$available]) > 1L)
  for (a in live) {
    matrix_a = state$is_not_subset_of[[a]]
    if (is.null(matrix_a)) next
    clause_a = state$entries[[state$available[[a]]]]
    for (symbol in unique(names(clause_a))) {
      require_review(length(range_at(clause_a, symbol)) == 1L,
        "stored occurrence homogeneity", state)
      if (!state$available[[a]] %in% state$symbol_registry[[symbol]]) {
        require_review(sum(colnames(matrix_a) == symbol) >= 2L,
          "orphan needs duplicate birth columns", state)
        snapshot("orphan", state, list(a = a, symbol = symbol, label = label))
      }
    }
    for (b in setdiff(live, a)) {
      if (is.na(state$not_subset_count[a, b])) next
      bits = matrix_a[b, ]
      require_review(sum(bits) == state$not_subset_count[a, b], "physical row count", state)
      require_review(all(bits[duplicated(colnames(matrix_a))]), "permanent trailing columns", state)
      clause_b = state$entries[[state$available[[b]]]]
      for (symbol in unique(colnames(matrix_a))) {
        group = bits[colnames(matrix_a) == symbol]
        if (!any(group)) require_review(all(range_at(clause_a, symbol) %in% range_at(clause_b, symbol)),
          "grouped raw containment", state, list(a = a, b = b, symbol = symbol, label = label))
        if (!symbol %in% names(clause_a)) require_review(!bits[[symbol]],
          "absent symbol has FALSE primary", state)
      }
      count("cached_pairs")
    }
  }
}

watch_helper = function(state, local, fn, leaving = FALSE, result = NULL) {
  if (leaving && identical(result, TRUE)) {
    require_review(!any(review$truth), paste("valid contradiction", fn), state)
  }
  watch_semantics(state, paste(if (leaving) "exit" else "enter", fn))
  watch_cache(state, fn)
  if (!leaving && fn == "eliminate_clause_update_sr") {
    orphan = any(vapply(unique(names(state$entries[[local$clause_idx]])), function(s) {
      !local$clause_idx %in% state$symbol_registry[[s]]
    }, logical(1L)))
    if (orphan) snapshot("orphan_clause_deleted", state, list(index = local$clause_idx, hla = review$hla))
    if (review$hla) snapshot("hla_clause_deleted", state, list(index = local$clause_idx, orphan = orphan))
  }
  if (!leaving && fn == "eliminate_symbol_from_clause") {
    occurrences = sum(names(state$entries[[local$clause_idx]]) == local$symbol)
    if (occurrences > 1L) snapshot("first_copy_removed", state,
      list(index = local$clause_idx, symbol = local$symbol, occurrences = occurrences))
  }
  if (!leaving && fn == "try_sse_2nd_order") {
    equal_names = identical(local$symbol_intersect, local$symbol_target)
    if (equal_names) {
      row = state$is_not_subset_of[[local$meta_idx_oneend]][local$meta_idx_target, ]
      snapshot("same_name_sse2", state, list(exceptions = names(row)[row]))
      if (any(names(row)[row] != local$symbol_intersect)) {
        snapshot("same_name_unrelated_exception", state, list(exceptions = names(row)[row]))
      }
    }
  }
}

watch_hla = function(state) {
  # This is deliberately a column for the CURRENT target. Earlier targets'
  # columns may describe their discarded virtual expansions.
  meta = state$meta_idx
  target = state$clause
  others = state$remaining_other_entries
  replacement = c(state$entries[others], list(target), state$entries[state$remaining_unit_entries])
  require_review(identical(truth_entries(replacement, review$ctx$assignment), review$truth),
    "virtual HLA preserves conjunction", state, list(target = state$clause_idx, virtual = target))
  count("hla_virtual_semantics")
  for (position in seq_along(others)) {
    donor = others[[position]]
    donor_meta = state$available_inverse[[donor]]
    matrix_donor = state$is_not_subset_of[[donor_meta]]
    row = matrix_donor[meta, ]
    require_review(sum(row) == state$not_subset_count_current[[position]], "HLA physical row count", state)
    require_review(all(row[duplicated(colnames(matrix_donor))]), "HLA trailing columns", state)
    donor_clause = state$entries[[donor]]
    for (symbol in unique(colnames(matrix_donor))) {
      if (!any(row[colnames(matrix_donor) == symbol])) {
        require_review(all(range_at(donor_clause, symbol) %in% range_at(target, symbol)),
          "HLA grouped containment", state, list(donor = donor, virtual = target, symbol = symbol))
        if (!all(range_at(donor_clause, symbol) %in% range_at(state$entries[[state$clause_idx]], symbol))) {
          snapshot("hla_containment_needs_virtual_target", state,
            list(target = state$clause_idx, donor = donor, symbol = symbol, virtual = target))
        }
      }
    }
    count("hla_cached_pairs")
  }
  chosen = which(state$not_subset_count_current == 1L & !state$was_used)
  if (length(chosen)) snapshot("hla_starting_donor", state,
    list(target = state$clause_idx, donor = others[[chosen[[1L]]]]))
}

watch_pre_hla = function(state) {
  watch_cache(state, "pre_hla")
  for (symbol in names(state$unit_domains)) {
    registered = state$symbol_registry[[symbol]]
    require_review(!any(!state$eliminated[registered] & !state$is_unit[registered]),
      "no registered live nonunit for a unit symbol", state)
  }
  review$hla = TRUE
}

watch_unit_hla = function(state) {
  require_review(all(state$not_subset_count >= 2L), "unit HLA cannot start", state)
  count("unit_hla_initializations")
}

install_observer = function() {
  code = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
  insert = function(anchor, replacement, all = FALSE) {
    stopifnot(grepl(anchor, code, fixed = TRUE))
    code <<- if (all) gsub(anchor, replacement, code, fixed = TRUE) else sub(anchor, replacement, code, fixed = TRUE)
  }
  for (fn in c("register_unit", "apply_domain_restriction", "eliminate_symbol_from_clause",
    "on_updated_subset_relations", "on_update_range", "handle_sse_2nd_order_oneend",
    "handle_sse_2nd_order_twoend", "try_sse_2nd_order", "eliminate_clause_update_sr")) {
    line = grep(paste0("^  ", fn, " = function"), strsplit(code, "\n", fixed = TRUE)[[1L]], value = TRUE)
    stopifnot(length(line) == 1L)
    insert(line, paste0(line, "\n    watch_helper(parent.env(environment()), environment(), \"", fn, "\")",
      "\n    on.exit(watch_helper(parent.env(environment()), environment(), \"", fn,
      "\", TRUE, returnValue()), add = TRUE)"))
  }
  anchor = "  # Now for the big one: Asymmetric Hidden Literal Addition (Marijn et al.)"
  insert(anchor, paste0("  watch_pre_hla(environment())\n", anchor))
  anchor = "      hla_clause_idx = match(TRUE, not_subset_count_current == 1L & !was_used)"
  insert(anchor, paste0("      watch_hla(environment())\n", anchor))
  anchor = "    not_subset_count = lengths(entries[remaining_nonunit_entries]) - (remaining_nonunit_entries %in% symbol_registry[[unitsymbol]])"
  insert(anchor, paste0(anchor, "\n    watch_unit_hla(environment())"))
  anchor = "if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) next"
  replacement = paste0("if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) {\n",
    "          require_review(!s_clause_idx %in% symbol_registry[[nu]], \"unit skip must be an old snapshot\", parent.env(environment()))\n",
    "          snapshot(\"unit_snapshot_skip\", parent.env(environment()), list(unit = unit_idx, target = s_clause_idx, symbol = nu))\n",
    "          next\n        }")
  insert(anchor, replacement)
  writeLines(code, file.path(review_dir, "observed_simplify.R"))
  eval(parse(text = code), envir = cnf)
}

check_case = function(words, label, character_selector = FALSE) {
  review$words = words
  review$label = label
  review$ctx = context(max(1L, abs(unlist(words))))
  review$truth = truth_words(words, review$ctx$assignment)
  clauses = make_public(words, review$ctx, character_selector)
  require_review(identical(truth_entries(lapply(clauses, c), review$ctx$assignment), review$truth), "public selector truth")
  watched = cnf$simplify_cnf
  cnf$simplify_cnf = plain_simplify
  ordinary = tryCatch(cnf$CnfFormula(clauses), error = identity)
  cnf$simplify_cnf = watched
  require_review(!inherits(ordinary, "error"), "unchanged simplifier runtime error",
    detail = if (inherits(ordinary, "error")) conditionMessage(ordinary))
  review$hla = FALSE
  result = cnf$CnfFormula(clauses)
  require_review(identical(result, ordinary), "observer preserves exact positional result")
  require_review(identical(truth_entries(c(result), review$ctx$assignment), review$truth), "final Boolean semantics")
  count("formulas")
  count("assignment_rows", nrow(review$ctx$assignment))
  invisible(result)
}

if (sys.nframe() == 0L) {
  install_observer()
  started = proc.time()[[3L]]
  directed = list(
    list(c(-2L, 3L), c(-1L, -3L), c(3L, 2L, 1L, 1L), c(2L, -1L)),
    list(c(1L, 2L), c(1L, 1L, 3L), c(-1L, -1L, 3L)),
    list(c(-1L, 2L), c(-2L, 3L), c(-3L, 4L), c(-4L, 1L), c(1L, 1L, 3L)),
    list(c(1L, 2L, 3L), c(-3L, 4L), c(-4L, 5L), c(-5L, 1L), c(-2L, -1L)),
    list(c(-1L, 2L), c(1L, 2L, 3L), c(-2L, 4L), c(-4L, -3L)),
    list(c(1L, 2L), c(1L, -2L), c(-1L, -1L, 3L, 3L), c(-3L, 4L)),
    list(c(1L, 2L), c(-1L, 3L), c(2L, -3L), c(-2L, 4L), c(-2L, -4L)))
  for (base_idx in seq_along(directed)) {
    base = directed[[base_idx]]
    for (clause_idx in seq_along(base)) for (literal_idx in seq_along(base[[clause_idx]])) {
      for (copies in c(0L, 1L, 2L, 5L)) for (reverse in c(FALSE, TRUE)) {
        words = base
        words[[clause_idx]] = append(words[[clause_idx]],
          rep(words[[clause_idx]][[literal_idx]], copies), after = literal_idx)
        if (reverse) words = lapply(rev(words), rev)
        check_case(words, paste("directed", base_idx, clause_idx, literal_idx, copies, reverse), reverse)
      }
    }
    cat("directed family", base_idx, "formulas", review$counts$formulas, "\n")
  }
  for (copies in c(2L, 3L, 12L, 48L, 96L)) {
    check_case(list(c(rep(1L, copies), rep(2L, copies)), -1L), "long initial propagation")
    check_case(list(c(1L, 2L), c(1L, -2L), c(rep(-1L, copies), rep(3L, copies)), c(-3L, 4L)), "long derived propagation")
    check_case(list(rep(1L, copies), rep(-1L, copies)), "opposite pseudounits")
  }
  set.seed(963207L)
  random_cases = as.integer(Sys.getenv("CNF_REVIEW_RANDOM", "1500"))
  for (index in seq_len(random_cases)) {
    n = sample.int(4L, 1L) + 2L
    clauses = sample.int(6L, 1L) + 2L
    words = lapply(seq_len(clauses), function(i) {
      width = sample.int(min(4L, n), 1L)
      symbols = sample.int(n, width)
      signed = symbols * ifelse(sample(c(FALSE, TRUE), width, replace = TRUE), 1L, -1L)
      word = rep(signed, times = sample.int(4L, width, replace = TRUE))
      word[sample.int(length(word))]
    })
    check_case(words, paste("random", index), index %% 2L == 0L)
    if (index %% 100L == 0L) {
      cat("random", index, "elapsed", proc.time()[[3L]] - started, "\n")
      flush.console()
    }
  }
  result = list(R = R.version.string, counts = review$counts, examples = review$examples,
    seconds = proc.time()[[3L]] - started, source_md5 = tools::md5sum(source_paths))
  suffix = Sys.getenv("CNF_REVIEW_SUFFIX")
  saveRDS(result, file.path(review_dir, paste0("events", suffix, ".rds")))
  capture.output(print(result[c("R", "counts", "seconds", "source_md5")]),
    file = file.path(review_dir, paste0("events", suffix, ".txt")))
  print(result[c("R", "counts", "seconds")])
}
