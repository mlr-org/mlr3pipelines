# Standalone audit helpers. Run from the repository root.
# No evaluator, constructor, or simplifier is imported from older audit code.
suppressPackageStartupMessages(library(checkmate))
stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
map_chr = function(.x, .f, ...) vapply(.x, .f, character(1), ...)
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}

audit_dir = "attic/cnf_verify3/small_selectors"

# Evaluate the occurrence at integer position j. Looking up a range by symbol
# name would discard every occurrence after the first one with that name.
eval_clause = function(clause, assignments, first_only = FALSE) {
  if (is.logical(clause)) return(rep(c(clause), nrow(assignments)))
  answer = rep(FALSE, nrow(assignments))
  positions = seq_along(clause)
  if (first_only) positions = positions[!duplicated(names(clause))]
  for (j in positions) {
    symbol = names(clause)[[j]]
    stopifnot(!is.na(symbol), symbol %in% names(assignments))
    answer = answer | assignments[[symbol]] %in% clause[[j]]
  }
  answer
}

eval_formula = function(formula, assignments, first_only = FALSE) {
  if (is.logical(formula)) return(rep(c(formula), nrow(assignments)))
  answer = rep(TRUE, nrow(assignments))
  for (clause in formula) answer = answer & eval_clause(clause, assignments, first_only)
  answer
}

is_canonical = function(formula, domains) {
  if (is.logical(formula)) return(length(formula) == 1L && !anyNA(formula))
  all(vapply(formula, function(clause) {
    if (!is.list(clause) || !length(clause) || is.null(names(clause)) ||
      anyNA(names(clause)) || anyDuplicated(names(clause))) return(FALSE)
    all(vapply(seq_along(clause), function(j) {
      values = clause[[j]]
      domain = domains[[names(clause)[[j]]]]
      is.character(values) && length(values) > 0L && is.null(attributes(values)) &&
        !anyNA(values) && !anyDuplicated(values) && all(values %in% domain) &&
        !all(domain %in% values)
    }, logical(1)))
  }, logical(1)))
}

make_context = function(domains) {
  universe = CnfUniverse()
  symbols = lapply(names(domains), function(s) CnfSymbol(universe, s, domains[[s]]))
  names(symbols) = names(domains)
  list(domains = domains, universe = universe, symbols = symbols,
    assignments = expand.grid(domains, stringsAsFactors = FALSE))
}

make_selected = function(raw, selector, context) {
  base = CnfClause(lapply(seq_along(raw), function(j) {
    CnfAtom(context$symbols[[names(raw)[[j]]]], raw[[j]])
  }))
  stopifnot(!is.logical(base), identical(names(base), names(raw)))
  clause = base[matrix(selector, nrow = 1L)]
  expected = eval_clause(raw[selector], context$assignments)
  stopifnot(identical(expected, eval_clause(c(clause), context$assignments)))
  list(raw = raw, selector = selector, clause = clause, expected = expected)
}

classify = function(selected, context, keep_details = FALSE) {
  expected = Reduce(`&`, lapply(selected, `[[`, "expected"))
  output = tryCatch(CnfFormula(lapply(selected, `[[`, "clause")), error = identity)
  if (inherits(output, "error")) {
    return(list(category = "error", error = conditionMessage(output)))
  }
  positional = eval_formula(c(output), context$assignments)
  named = eval_formula(c(output), context$assignments, first_only = TRUE)
  canonical = is_canonical(c(output), context$domains)
  wrong = any(expected != positional)
  named_wrong = any(expected != named)
  category = if (canonical) {
    if (wrong) "canonical_wrong" else "canonical_correct"
  } else if (!wrong && !named_wrong) {
    "noncanonical_correct"
  } else if (wrong && !named_wrong) {
    "noncanonical_stale"
  } else if (!wrong && named_wrong) {
    "noncanonical_named_wrong"
  } else {
    "noncanonical_both_wrong"
  }
  if (!keep_details) return(list(category = category))
  # RDS/dput keep names and values positionally; no symbol-keyed JSON map is used.
  list(category = category, domains = context$domains,
    raw = lapply(selected, `[[`, "raw"), selectors = lapply(selected, `[[`, "selector"),
    selected = lapply(selected, function(s) c(s$clause)), output = c(output),
    truth = data.frame(context$assignments, expected, positional, named))
}

selector_words = function(support, max_occurrences) {
  result = list()
  for (n in seq.int(support, max_occurrences)) {
    grid = expand.grid(rep(list(seq_len(support)), n))
    for (i in seq_len(nrow(grid))) {
      word = as.integer(grid[i, ])
      if (setequal(word, seq_len(support))) result[[length(result) + 1L]] = word
    }
  }
  result
}

proper_ranges = function(domain) {
  result = list()
  for (n in seq_len(length(domain) - 1L)) {
    result = c(result, combn(domain, n, simplify = FALSE))
  }
  result
}

make_bank = function(context, max_occurrences = 3L) {
  choices = lapply(context$domains, function(d) c(list(character()), proper_ranges(d)))
  grid = expand.grid(lapply(choices, seq_along))
  bank = list()
  for (i in seq_len(nrow(grid))) {
    raw = Map(function(values, j) values[[j]], choices, as.integer(grid[i, ]))
    raw = raw[lengths(raw) > 0L]
    if (!length(raw) || length(raw) > max_occurrences) next
    for (selector in selector_words(length(raw), max_occurrences)) {
      bank[[length(bank) + 1L]] = make_selected(raw, selector, context)
    }
  }
  bank
}

categories = c("canonical_correct", "noncanonical_correct", "noncanonical_stale",
  "noncanonical_named_wrong", "noncanonical_both_wrong", "canonical_wrong", "error")

new_recorder = function(label) {
  label = paste0(label, Sys.getenv("CNF_SMALL_SUFFIX"))
  counts = setNames(integer(length(categories)), categories)
  examples = list()
  function(selected = NULL, context = NULL, finish = FALSE, extra = NULL) {
    if (finish) {
      result = list(label = label, counts = counts, examples = examples, extra = extra,
        R = R.version.string, checkmate = as.character(packageVersion("checkmate")))
      saveRDS(result, file.path(audit_dir, paste0(label, ".rds")))
      write.table(data.frame(category = names(counts), count = unname(counts)),
        file.path(audit_dir, paste0(label, ".tsv")), sep = "\t", quote = FALSE, row.names = FALSE)
      print(counts)
      return(invisible(result))
    }
    out = classify(selected, context)
    cat = out$category
    counts[[cat]] <<- counts[[cat]] + 1L
    if (is.null(examples[[cat]]) && cat != "canonical_correct") {
      examples[[cat]] <<- classify(selected, context, keep_details = TRUE)
      if (cat == "error") {
        examples[[cat]] <<- c(examples[[cat]], list(domains = context$domains,
          raw = lapply(selected, `[[`, "raw"), selectors = lapply(selected, `[[`, "selector")))
      }
      cat("FIRST", label, cat, "at", sum(counts), "\n")
      dput(examples[[cat]])
      flush.console()
    }
    invisible(cat)
  }
}
