#!/usr/bin/env Rscript
# Exhaustive test: 2 variables with domain size 5.
# Each clause uses subsets of size 1-4 for each of 1-2 variables.
# Generate all possible clauses, then test all 2-clause combinations
# and random 3-clause combinations.
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

cat("=== Exhaustive 2-var 5-domain ===\n")

u = CnfUniverse()
dom = c("a", "b", "c", "d", "e")
A = CnfSymbol(u, "A", dom)
B = CnfSymbol(u, "B", dom)

# Generate all possible non-trivial subsets
all_subsets = function(d) {
  n = length(d)
  result = list()
  for (k in 1:(n-1)) {
    combos = combn(d, k, simplify = FALSE)
    result = c(result, combos)
  }
  result
}

subsets = all_subsets(dom)
cat(sprintf("  Subsets per symbol: %d\n", length(subsets)))

# Generate all non-tautological clauses
# 1-symbol clauses: A in subset or B in subset
# 2-symbol clauses: A in subset | B in subset (skip if tautological)
clause_pool = list()

# A-only clauses
for (s in subsets) {
  clause_pool[[length(clause_pool) + 1]] = as.CnfClause(A %among% s)
}
# B-only clauses
for (s in subsets) {
  clause_pool[[length(clause_pool) + 1]] = as.CnfClause(B %among% s)
}
# A|B clauses
for (sa in subsets) {
  for (sb in subsets) {
    cl = as.CnfClause(A %among% sa | B %among% sb)
    if (!isTRUE(unclass(cl))) {
      clause_pool[[length(clause_pool) + 1]] = cl
    }
  }
}

n_pool = length(clause_pool)
cat(sprintf("  Total clause pool: %d\n", n_pool))

# Test all 2-clause combinations
cat("  Testing all 2-clause combos...\n")
for (i in 1:n_pool) {
  if (i %% 100 == 0) cat(sprintf("    Progress: %d/%d\n", i, n_pool))
  for (j in (i):n_pool) {  # include i=j (same clause twice)
    clauses = list(clause_pool[[i]], clause_pool[[j]])

    n_tests = n_tests + 1
    result = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [2cl-%d-%d]: %s\n", i, j, result$message)); next
    }
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [2cl-%d-%d]: semantic mismatch\n", i, j))
    }
  }
}
cat(sprintf("  2-clause combos: %d tests, %d failures\n", n_tests, n_failures))

# Test random 3-clause combinations
cat("  Testing random 3-clause combos...\n")
set.seed(133001)
for (trial in 1:5000) {
  idx = sample(n_pool, 3, replace = TRUE)
  clauses = list(clause_pool[[idx[1]]], clause_pool[[idx[2]]], clause_pool[[idx[3]]])

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [3cl-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3cl-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  3-clause combos: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
