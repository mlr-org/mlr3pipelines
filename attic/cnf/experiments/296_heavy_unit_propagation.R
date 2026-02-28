#!/usr/bin/env Rscript
# Stress test unit propagation with many units.
# Create formulas where many variables have units, causing widespread restriction.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Many units ===\n")
set.seed(296001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:6)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  clauses = list()

  # Create 3-5 units
  n_units = sample(3:5, 1)
  unit_syms = sample(sym_names, n_units)
  for (s in unit_syms) {
    r = sample(dom, sample(1:2, 1))
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% r)
  }

  # Create 3-6 multi-symbol clauses that overlap with units
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[sample(length(clauses))]
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [manyunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [manyunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Many units: %d tests, %d failures\n", n_tests, n_failures))

# === All units (only single-symbol clauses) ===
cat("\n=== All units ===\n")
set.seed(296002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # All clauses are units (single symbol)
  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    s = sample(sym_names, 1)
    as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [allunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [allunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  All units: %d tests, %d failures\n", n_tests, n_failures))

# === Units with conflicting ranges ===
cat("\n=== Units with varying overlaps ===\n")
set.seed(296003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  V = CnfSymbol(u, "V", dom)

  # Multiple units for the same symbol with varying overlap
  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    as.CnfClause(V %among% sample(dom, sample(1:4, 1)))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [unitoverlap-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unitoverlap-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Units with varying overlaps: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
