#!/usr/bin/env Rscript
# Domain size 1 edge case testing:
# When a symbol has only one possible value, every mention of it is a unit.
# This creates degenerate cases for the simplifier.
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

# === Pattern 1: All domain-1 symbols ===
cat("=== All domain-1 symbols ===\n")
set.seed(239001)

for (trial in 1:500) {
  u = CnfUniverse()
  n_vars = sample(3:6, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, "only")

  # All clauses mention single-valued symbols
  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:min(3, n_vars), 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% "only")
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [d1all-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [d1all-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  All domain-1: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Mixed domain-1 and domain-2+ symbols ===
cat("\n=== Mixed domain-1 and larger ===\n")
set.seed(239002)

for (trial in 1:500) {
  u = CnfUniverse()
  # Some symbols have domain 1, others have larger domains
  sym_names = paste0("V", 1:5)
  doms = list()
  syms = list()
  for (s in sym_names) {
    d_size = sample(c(1, 1, 2, 3, 4), 1)  # bias towards domain-1
    d = paste0("v", 1:d_size)
    doms[[s]] = d
    syms[[s]] = CnfSymbol(u, s, d)
  }

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:length(d), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [d1mix-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [d1mix-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Mixed domain-1: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Domain-1 with operations ===
cat("\n=== Domain-1 with operations ===\n")
set.seed(239003)

for (trial in 1:300) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", "x")
  B = CnfSymbol(u, "B", c("y", "z"))
  C = CnfSymbol(u, "C", c("p", "q", "r"))

  # Create formulas with domain-1 symbol
  f1_clauses = list(as.CnfClause(A %among% "x" | B %among% sample(c("y", "z"), 1)))
  f1_clauses = f1_clauses[!sapply(f1_clauses, function(x) isTRUE(unclass(x)))]
  if (length(f1_clauses) < 1) next
  f1 = CnfFormula(f1_clauses)

  f2_clauses = list(as.CnfClause(B %among% sample(c("y", "z"), 1) | C %among% sample(c("p", "q", "r"), sample(1:2, 1))))
  f2_clauses = f2_clauses[!sapply(f2_clauses, function(x) isTRUE(unclass(x)))]
  if (length(f2_clauses) < 1) next
  f2 = CnfFormula(f2_clauses)

  n_tests = n_tests + 1

  # Test AND
  result_and = tryCatch(f1 & f2, error = function(e) e)
  if (inherits(result_and, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [d1op-and-%d]: %s\n", trial, result_and$message)); next
  }
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected_and = t1 & t2
  actual_and = evaluate_formula(result_and, u)
  if (!all(actual_and == expected_and)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [d1op-and-%d]: AND mismatch\n", trial)); next
  }

  # Test OR
  result_or = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result_or, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [d1op-or-%d]: %s\n", trial, result_or$message)); next
  }
  expected_or = t1 | t2
  actual_or = evaluate_formula(result_or, u)
  if (!all(actual_or == expected_or)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [d1op-or-%d]: OR mismatch\n", trial)); next
  }

  # Test NOT
  result_not = tryCatch(!f1, error = function(e) e)
  if (inherits(result_not, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [d1op-not-%d]: %s\n", trial, result_not$message)); next
  }
  expected_not = !t1
  actual_not = evaluate_formula(result_not, u)
  if (!all(actual_not == expected_not)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [d1op-not-%d]: NOT mismatch\n", trial))
  }
}
cat(sprintf("  Domain-1 ops: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Domain-1 symbol in clauses that become FALSE ===
cat("\n=== Domain-1 contradictions ===\n")
set.seed(239004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom_sizes = sample(c(1, 1, 2, 3), 4, replace = TRUE)
  sym_names = paste0("V", 1:4)
  doms = list()
  syms = list()
  for (i in 1:4) {
    d = paste0("v", 1:dom_sizes[i])
    doms[[sym_names[i]]] = d
    syms[[sym_names[i]]] = CnfSymbol(u, sym_names[i], d)
  }

  # Create potentially contradictory clauses
  n_cl = sample(4:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:length(d), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [d1contr-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [d1contr-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Domain-1 contradictions: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
