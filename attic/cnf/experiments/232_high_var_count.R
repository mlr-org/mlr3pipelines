#!/usr/bin/env Rscript
# High variable count testing:
# 10-15 binary variables with many clauses.
# This tests scalability and correctness with large is_not_subset_of matrices.
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

# === Pattern 1: 10 binary vars, 3-SAT ===
cat("=== 10 binary vars 3-SAT ===\n")
set.seed(232001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1")
  n_vars = 10
  sym_names = paste0("X", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(10:25, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(sym_names, 3)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 8) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [10b3sat-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [10b3sat-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  10 binary 3-SAT: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: 12 binary vars, 2-SAT + units ===
cat("\n=== 12 binary vars 2-SAT + units ===\n")
set.seed(232002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("T", "F")
  n_vars = 12
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Some units
  n_units = sample(1:3, 1)
  unit_syms = sample(sym_names, n_units)
  clauses = lapply(unit_syms, function(s) as.CnfClause(syms[[s]] %among% sample(dom, 1)))

  # 2-SAT clauses
  n_cl = sample(8:20, 1)
  for (j in 1:n_cl) {
    chosen = sample(sym_names, 2)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 6) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [12b2sat-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [12b2sat-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  12 binary 2-SAT+units: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: 8 ternary vars ===
cat("\n=== 8 ternary vars ===\n")
set.seed(232003)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = 8
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(8:20, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 6) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [8t-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [8t-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  8 ternary: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: 15 binary vars, very sparse ===
cat("\n=== 15 binary vars sparse ===\n")
set.seed(232004)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("0", "1")
  n_vars = 15
  sym_names = paste0("B", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Very sparse: 2-3 vars per clause
  n_cl = sample(10:20, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 8) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [15sparse-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [15sparse-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  15 binary sparse: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
