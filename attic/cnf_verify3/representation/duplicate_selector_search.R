# Canonical clauses are subset with ordinary one-row matrices containing
# repeated numeric indices. Each selector retains every original literal,
# so it must preserve the original clause's disjunction.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
for (nm in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(nm, ".R")))
}
truth = function(formula, assignments, by_name = FALSE) {
  if (is.logical(formula)) return(rep(c(formula), nrow(assignments)))
  result = rep(TRUE, nrow(assignments))
  for (cl in formula) {
    value = rep(FALSE, nrow(assignments))
    for (i in seq_along(cl)) {
      symbol = names(cl)[[i]]
      range = if (by_name) cl[[symbol]] else cl[[i]]
      value = value | assignments[[symbol]] %in% range
    }
    result = result & value
  }
  result
}
set.seed(as.integer(Sys.getenv("CNF_REP_SEED", "619031")))
n_trials = as.integer(Sys.getenv("CNF_REP_TRIALS", "10000"))
error_counts = integer()
first_errors = list()
semantic = list()
n_positional = n_named = 0L
for (trial in seq_len(n_trials)) {
  domains = lapply(seq_len(sample(2:4, 1L)), function(i) paste0("v", seq_len(sample(2:5, 1L))))
  names(domains) = paste0("X", seq_along(domains))
  u = CnfUniverse()
  syms = lapply(names(domains), function(s) CnfSymbol(u, s, domains[[s]]))
  names(syms) = names(domains)
  raw = lapply(seq_len(sample(2:9, 1L)), function(i) {
    symbols = sample(names(domains), sample.int(length(domains), 1L))
    out = lapply(symbols, function(s) sample(domains[[s]], sample.int(length(domains[[s]]) - 1L, 1L)))
    names(out) = symbols
    out
  })
  selectors = lapply(raw, function(cl) {
    indices = seq_along(cl)
    if (runif(1) < .75) indices = c(indices, sample(indices, sample(1:4, 1L), replace = TRUE))
    sample(indices)
  })
  objects = lapply(seq_along(raw), function(ci) {
    cl = CnfClause(lapply(names(raw[[ci]]), function(s) CnfAtom(syms[[s]], raw[[ci]][[s]])))
    cl[matrix(selectors[[ci]], nrow = 1L)]
  })
  # Check generator semantics before invoking the simplifier.
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  expected = truth(raw, assignments)
  stopifnot(identical(expected, truth(lapply(objects, unclass), assignments)))
  actual = tryCatch(CnfFormula(objects), error = function(e) e)
  if (inherits(actual, "error")) {
    message = conditionMessage(actual)
    if (!message %in% names(error_counts)) {
      error_counts[[message]] = 0L
      first_errors[[message]] = list(trial = trial, domains = domains, raw = raw,
        selectors = selectors, error = message)
    }
    error_counts[[message]] = error_counts[[message]] + 1L
  } else {
    actual = c(actual)
    positional = truth(actual, assignments)
    named = truth(actual, assignments, by_name = TRUE)
    if (!identical(expected, positional) || !identical(expected, named)) {
      n_positional = n_positional + as.integer(!identical(expected, positional))
      n_named = n_named + as.integer(!identical(expected, named))
      if (length(semantic) < 5L || !identical(expected, named)) semantic[[length(semantic) + 1L]] = list(trial = trial, domains = domains, raw = raw,
        selectors = selectors, actual = actual, positional_mismatch = which(expected != positional),
        named_mismatch = which(expected != named), assignments = assignments,
        expected = expected, positional = positional, named = named)
      saveRDS(semantic, "attic/cnf_verify3/representation/duplicate_selector_semantic.rds")
      if (n_positional <= 5L || !identical(expected, named)) cat("SEMANTIC trial=", trial, " positional=", sum(expected != positional), " named=", sum(expected != named), "\n", sep = "")
      if (n_named >= 5L) break
    }
  }
  if (trial %% 250L == 0L) cat("trials=", trial, " errors=", sum(error_counts), " error_types=", length(error_counts), " positional_cases=", n_positional, " named_cases=", n_named, "\n", sep = "")
}
saveRDS(list(trials = trial, errors = error_counts, first_errors = first_errors, semantic = semantic, n_positional = n_positional, n_named = n_named,
  version = R.version.string), "attic/cnf_verify3/representation/duplicate_selector_results.rds")
cat("FINAL trials=", trial, " errors=", sum(error_counts), " positional_cases=", n_positional, " named_cases=", n_named, "\n", sep = "")
print(error_counts)
