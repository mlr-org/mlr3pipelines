#!/usr/bin/env Rscript
# Wide clause stress testing:
# Clauses with many symbols (5-8) that exercise the is_not_subset_of matrix
# with many columns. This stresses the matrix column handling, symbol elimination
# from wide clauses, and cascading when wide clauses interact.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

evaluate_raw_clauses = function(clauses, universe) {
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }
  raw_truth
}

# === Pattern 1: 8 binary vars, clauses spanning 5-7 vars ===
cat("=== 8 binary wide clauses ===\n")
set.seed(217001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("T", "F")
  sym_names = paste0("V", 1:8)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(4:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(5:7, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [8bwide-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [8bwide-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  8 binary wide: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: 6 ternary vars, clauses spanning 4-6 vars ===
cat("\n=== 6 ternary wide clauses ===\n")
set.seed(217002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("X", 1:6)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(5:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(4:6, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [6twide-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [6twide-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  6 ternary wide: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Wide clauses with units to force massive elimination ===
cat("\n=== Wide clauses + units ===\n")
set.seed(217003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:7)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Some units
  n_units = sample(1:3, 1)
  unit_syms = sample(sym_names, n_units)
  clauses = lapply(unit_syms, function(s) {
    as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
  })

  # Wide clauses
  for (j in 1:sample(4:8, 1)) {
    n_sym = sample(4:7, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [wideunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [wideunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Wide + units: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: All clauses span ALL variables ===
cat("\n=== Full-width clauses ===\n")
set.seed(217004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(4:6, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(4:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    atoms = lapply(sym_names, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [fullw-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [fullw-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Full-width: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
