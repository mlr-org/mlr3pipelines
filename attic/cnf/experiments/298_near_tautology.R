#!/usr/bin/env Rscript
# Test with clauses that are near-tautological (almost cover the full domain).
# These exercise the HLA tautology detection path.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Near-tautological clauses ===\n")
set.seed(298001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create clauses where each symbol covers most of the domain
  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    # Each atom covers 3-4 out of 5 values
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(3:4, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neartaut-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [neartaut-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Near-tautological clauses: %d tests, %d failures\n", n_tests, n_failures))

# === Mix of near-tautological and very restrictive ===
cat("\n=== Mixed restrictiveness ===\n")
set.seed(298002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  clauses = list()
  # Some very restrictive (1 value per symbol)
  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # Some near-tautological (3 values per symbol)
  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 3))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[sample(length(clauses))]
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mixrest-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixrest-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Mixed restrictiveness: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
