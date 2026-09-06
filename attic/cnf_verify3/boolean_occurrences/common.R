# Run from the repository root. Public constructors use real checkmate;
# signed occurrence words and this oracle are independent of the simplifier.
if (nzchar(Sys.getenv("CNF_REP_RLIB"))) .libPaths(c(Sys.getenv("CNF_REP_RLIB"), .libPaths()))
suppressPackageStartupMessages(library(checkmate))
stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
map_chr = function(.x, .f, ...) vapply(.x, .f, character(1), ...)
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}
production_simplify_cnf = simplify_cnf
audit_dir = "attic/cnf_verify3/boolean_occurrences"

boolean_context = function(n) {
  u = CnfUniverse()
  symbols = lapply(seq_len(n), function(i) CnfSymbol(u, paste0("S", i), c("0", "1")))
  grid = expand.grid(rep(list(0:1), n), KEEP.OUT.ATTRS = FALSE)
  names(grid) = paste0("S", seq_len(n))
  list(universe = u, symbols = symbols, assignments = grid)
}

word_truth = function(words, assignments) {
  result = rep(TRUE, nrow(assignments))
  for (word in words) {
    clause = rep(FALSE, nrow(assignments))
    for (literal in word) clause = clause | (assignments[[abs(literal)]] == as.integer(literal > 0))
    result = result & clause
  }
  result
}

occurrence_truth = function(entries, assignments) {
  if (is.logical(entries)) return(rep(as.vector(entries), nrow(assignments)))
  result = rep(TRUE, nrow(assignments))
  for (clause in entries) {
    if (is.logical(clause)) {
      value = rep(as.vector(clause), nrow(assignments))
    } else {
      value = rep(FALSE, nrow(assignments))
      for (position in seq_along(clause)) {
        value = value | as.character(assignments[[names(clause)[[position]]]]) %in% clause[[position]]
      }
    }
    result = result & value
  }
  result
}

public_clauses = function(words, context) {
  lapply(words, function(word) {
    stopifnot(length(word) > 0L, !anyNA(word), all(word != 0),
      all(abs(word) <= length(context$symbols)), !any(-word %in% word))
    literals = unique(word)
    base = CnfClause(lapply(literals, function(lit) {
      CnfAtom(context$symbols[[abs(lit)]], as.character(as.integer(lit > 0)))
    }))
    result = base[matrix(match(word, literals), nrow = 1L)]
    stopifnot(identical(names(result), paste0("S", abs(word))),
      identical(unname(c(result)), lapply(word, function(lit) as.character(as.integer(lit > 0)))))
    result
  })
}

audit = new.env(parent = emptyenv())
audit$totals = list()
audit$examples = list()
bump = function(name, amount = 1L) {
  before = audit$totals[[name]]
  if (is.null(before)) before = 0L
  audit$totals[[name]] = before + amount
}

remember = function(name, state = NULL, detail = NULL) {
  bump(name)
  if (!is.null(audit$examples[[name]])) return(invisible(NULL))
  audit$examples[[name]] = list(words = audit$words, label = audit$label, detail = detail,
    entries = if (!is.null(state)) state$entries else NULL,
    eliminated = if (!is.null(state)) state$eliminated else NULL,
    units = if (!is.null(state)) as.list(state$unit_domains) else NULL,
    registry = if (!is.null(state)) as.list(state$symbol_registry) else NULL)
}

audit_fail = function(message, state = NULL, detail = NULL) {
  remember(paste0("FAIL: ", message), state, detail)
  audit$failed = TRUE
  stop(message, call. = FALSE)
}

assert_semantics = function(state, event) {
  truth = occurrence_truth(state$entries[!state$eliminated], audit$context$assignments)
  if (!identical(truth, audit$expected)) audit_fail(paste("transient semantics", event), state)
  bump("semantic_boundary_checks")
}

check_boolean_state = function(state, event) {
  if (audit$in_hla || is.null(state$is_not_subset_of)) return(invisible(NULL))
  active = which(!state$eliminated[state$available] & !state$is_unit[state$available] &
    lengths(state$entries[state$available]) > 1L)
  for (i in active) {
    matrix_i = state$is_not_subset_of[[i]]
    if (is.null(matrix_i)) next
    clause_i = state$entries[[state$available[[i]]]]
    for (s in unique(names(clause_i))) {
      positions = which(names(clause_i) == s)
      if (!all(vapply(positions, function(k) identical(clause_i[[k]], clause_i[[positions[[1L]]]]), logical(1)))) {
        audit_fail("unequal surviving Boolean copies", state)
      }
      if (length(clause_i[[positions[[1L]]]]) != 1L) audit_fail("non-singleton stored Boolean range", state)
      if (!state$available[[i]] %in% state$symbol_registry[[s]]) {
        remember("orphan_occurrence", state, list(source = i, symbol = s, event = event))
        if (sum(colnames(matrix_i) == s) < 2L) audit_fail("orphan without a duplicated birth column", state)
      }
    }
    for (j in setdiff(active, i)) {
      if (is.na(state$not_subset_count[i, j])) next
      row = matrix_i[j, ]
      if (sum(row) != state$not_subset_count[i, j]) audit_fail("cache row sum mismatch", state)
      if (any(!row[duplicated(colnames(matrix_i))])) audit_fail("trailing duplicate column cleared", state)
      clause_j = state$entries[[state$available[[j]]]]
      for (s in unique(colnames(matrix_i))) {
        bits = row[colnames(matrix_i) == s]
        if (!any(bits) && !all(clause_i[[s]] %in% clause_j[[s]])) {
          audit_fail("all-FALSE symbol without raw containment", state,
            list(i = i, j = j, symbol = s, event = event))
        }
        if (!is.null(clause_i[[s]]) && identical(clause_i[[s]], clause_j[[s]]) &&
          row[[s]] && state$is_not_subset_of[[j]][i, s]) {
          audit_fail("equal Boolean literals with mutual primary TRUE", state,
            list(i = i, j = j, symbol = s, event = event))
        }
      }
      bump("cache_pair_checks")
    }
  }
}

helper_enter = function(state, local, fn) {
  if (audit$failed) return(invisible(NULL))
  bump(paste0("enter_", fn))
  assert_semantics(state, paste("enter", fn))
  # Unit registration itself has a deliberate moment before is_unit is set;
  # physical length above keeps those transition objects out of pair checks.
  check_boolean_state(state, fn)
  if (fn == "eliminate_symbol_from_clause") {
    count = sum(names(state$entries[[local$clause_idx]]) == local$symbol)
    if (count > 1L) remember("delete_first_with_copy_left", state,
      list(clause_idx = local$clause_idx, symbol = local$symbol, copies = count))
  }
  if (fn == "try_sse_2nd_order" && identical(local$symbol_intersect, local$symbol_target)) {
    remember("same_name_twoend_try", state,
      list(oneend = local$meta_idx_oneend, twoend = local$meta_idx_twoends,
        target = local$meta_idx_target, symbol = local$symbol_target))
    row = state$is_not_subset_of[[local$meta_idx_oneend]][local$meta_idx_target, ]
    exceptional = names(row)[row]
    if (any(exceptional != local$symbol_target)) {
      remember("same_name_extra_exception", state,
        list(oneend = local$meta_idx_oneend, twoend = local$meta_idx_twoends,
          target = local$meta_idx_target, symbol = local$symbol_target, exceptional = exceptional))
    }
  }
  if (fn == "eliminate_clause_update_sr") {
    symbols = names(state$entries[[local$clause_idx]])
    if (any(vapply(symbols, function(s) !local$clause_idx %in% state$symbol_registry[[s]], logical(1)))) {
      remember("delete_orphan_clause", state, list(clause_idx = local$clause_idx, in_hla = audit$in_hla))
    }
    if (audit$in_hla) remember("nonunit_hla_deletion", state, list(clause_idx = local$clause_idx))
  }
}

helper_exit = function(state, local, fn, result) {
  if (audit$failed) return(invisible(NULL))
  if (identical(result, TRUE) && any(audit$expected)) audit_fail(paste("false contradiction from", fn), state)
  assert_semantics(state, paste("exit", fn))
  check_boolean_state(state, paste("exit", fn))
}

install_instrumentation = function() {
  src = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
  helpers = c("register_unit", "apply_domain_restriction", "eliminate_symbol_from_clause",
    "on_updated_subset_relations", "on_update_range", "handle_sse_2nd_order_oneend",
    "handle_sse_2nd_order_twoend", "try_sse_2nd_order", "eliminate_clause_update_sr")
  for (fn in helpers) {
    lines = strsplit(src, "\n", fixed = TRUE)[[1L]]
    found = lines[startsWith(lines, paste0("  ", fn, " = function("))]
    stopifnot(length(found) == 1L)
    replacement = paste0(found,
      "\n    helper_enter(parent.env(environment()), environment(), \"", fn, "\")",
      "\n    on.exit(helper_exit(parent.env(environment()), environment(), \"", fn, "\", returnValue()), add = TRUE)")
    src = sub(found, replacement, src, fixed = TRUE)
  }
  anchor = "  # Now for the big one: Asymmetric Hidden Literal Addition (Marijn et al.)"
  stopifnot(grepl(anchor, src, fixed = TRUE))
  src = sub(anchor, paste0("  check_boolean_state(environment(), \"pre_hla\")\n",
    "  for (s in names(unit_domains)) {\n",
    "    if (any(!eliminated[symbol_registry[[s]]] & !is_unit[symbol_registry[[s]]])) audit_fail(\"registered live nonunit has a unit symbol at pre-HLA\", environment())\n",
    "  }\n  audit$in_hla = TRUE\n", anchor), src, fixed = TRUE)
  anchor = "      symbol = names(is_not_subset_entry)[is_not_subset_entry]"
  stopifnot(grepl(anchor, src, fixed = TRUE))
  src = sub(anchor, paste0(anchor,
    "\n      remember(\"unit_hla_donor\", environment(), list(unit = clause_idx, donor = clause_idx_other, symbol = symbol))",
    "\n      if (length(symbol) != 1L) remember(\"unit_hla_bad_exception_count\", environment(), list(symbol = symbol, mask = is_not_subset_entry))"), src, fixed = TRUE)
  skip = "if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) next"
  stopifnot(grepl(skip, src, fixed = TRUE))
  src = sub(skip, sub(" next$", paste0(" {\n",
    "          remember(\"unit_cache_skip\", parent.env(environment()), list(clause_idx = s_clause_idx, unit_idx = unit_idx, symbol = nu, registered_now = s_clause_idx %in% symbol_registry[[nu]]))\n",
    "          if (s_clause_idx %in% symbol_registry[[nu]]) audit_fail(\"Boolean cache skip while still registered\", parent.env(environment()))\n",
    "          next\n        }"), skip), src, fixed = TRUE)
  anchor = "      range_new = c(range_old, char_setdiff(universe[[symbol]], c(range_old, entries[[clause_idx_other]][[symbol]])))"
  stopifnot(grepl(anchor, src, fixed = TRUE))
  src = sub(anchor, paste0("      remember(\"nonunit_hla_donor\", environment(), list(target = clause_idx, donor = clause_idx_other, symbol = symbol))\n", anchor), src, fixed = TRUE)
  eval(parse(text = src), envir = globalenv())
}

one_case = function(words, context = NULL, label = "", instrument = TRUE) {
  if (is.null(context)) context = boolean_context(max(abs(unlist(words))))
  clauses = public_clauses(words, context)
  audit$words = words
  audit$label = label
  audit$context = context
  audit$expected = word_truth(words, context$assignments)
  audit$failed = FALSE
  audit$in_hla = FALSE
  stopifnot(identical(audit$expected, occurrence_truth(lapply(clauses, c), context$assignments)))
  # Always run the unchanged source separately; instrumented output must be identical.
  simplifier = simplify_cnf
  simplify_cnf <<- production_simplify_cnf
  expected_output = tryCatch(CnfFormula(clauses), error = identity)
  simplify_cnf <<- simplifier
  result = if (instrument) tryCatch(CnfFormula(clauses), error = identity) else expected_output
  if (audit$failed) {
    saveRDS(list(examples = audit$examples, totals = audit$totals), file.path(audit_dir, "failure.rds"))
    stop(conditionMessage(result), call. = FALSE)
  }
  if (inherits(result, "error")) {
    stopifnot(inherits(expected_output, "error"), identical(conditionMessage(result), conditionMessage(expected_output)))
    remember("runtime_error", detail = conditionMessage(result))
    return(invisible("error"))
  }
  stopifnot(!inherits(expected_output, "error"), identical(result, expected_output))
  if (!identical(audit$expected, occurrence_truth(c(result), context$assignments))) {
    remember("semantic_error", detail = c(result))
    saveRDS(list(examples = audit$examples, totals = audit$totals), file.path(audit_dir, "failure.rds"))
    stop("Wrong final positional truth table", call. = FALSE)
  }
  bump("correct_formulas")
  bump("assignment_rows", nrow(context$assignments))
  invisible(result)
}
