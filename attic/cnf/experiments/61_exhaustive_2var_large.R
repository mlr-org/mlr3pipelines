#!/usr/bin/env Rscript
# Exhaustive test: 2 variables with domain size 4, all possible clause combinations
# Total clause pool: all non-trivial clauses on 2 variables with domain {a,b,c,d}
# Each variable can have range 1-3 (not 0 or 4, which give FALSE/tautology)
# Single-symbol clauses: 2 vars * C(4,1)+C(4,2)+C(4,3) = 2 * 14 = 28
# Two-symbol clauses: C(4,1..3) * C(4,1..3) = 14 * 14 = 196
# Total pool: 28 + 196 = 224
# We test all 2-clause combinations and random 3-4 clause combinations
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

check_fc = function(formula, raw_clauses, universe, label) {
  n_tests <<- n_tests + 1
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in raw_clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }

  simplified_truth = evaluate_formula(formula, universe)
  mismatches = which(raw_truth != simplified_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d (raw=%s, simp=%s)\n",
      label, idx, raw_truth[idx], simplified_truth[idx]))
    return(FALSE)
  }
  TRUE
}

u = CnfUniverse()
dom = c("a", "b", "c", "d")
A = CnfSymbol(u, "A", dom)
B = CnfSymbol(u, "B", dom)

# Generate all non-trivial subsets of size 1-3
subsets = list()
for (k in 1:3) {
  combos = combn(dom, k, simplify = FALSE)
  subsets = c(subsets, combos)
}

# Build clause pool
clause_pool = list()

# Single-symbol clauses
for (ss in subsets) {
  clause_pool[[length(clause_pool) + 1]] = as.CnfClause(A %among% ss)
  clause_pool[[length(clause_pool) + 1]] = as.CnfClause(B %among% ss)
}

# Two-symbol clauses
for (ss_a in subsets) {
  for (ss_b in subsets) {
    clause_pool[[length(clause_pool) + 1]] = as.CnfClause(A %among% ss_a | B %among% ss_b)
  }
}

cat(sprintf("Clause pool size: %d\n", length(clause_pool)))

# === All 2-clause combinations ===
cat("=== All 2-clause combinations ===\n")
n_pairs = length(clause_pool) * (length(clause_pool) - 1) / 2
cat(sprintf("Testing %d pairs...\n", n_pairs))

for (i in 1:(length(clause_pool)-1)) {
  for (j in (i+1):length(clause_pool)) {
    clauses = list(clause_pool[[i]], clause_pool[[j]])
    f = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(f, "error")) {
      n_tests = n_tests + 1; n_failures = n_failures + 1
      cat(sprintf("ERROR [2cl %d,%d]: %s\n", i, j, f$message)); next
    }
    check_fc(f, clauses, u, sprintf("2cl-%d-%d", i, j))
  }
}
cat(sprintf("  2-clause: %d tests, %d failures\n", n_tests, n_failures))

# === Random 3-clause combinations ===
cat("\n=== Random 3-clause combinations ===\n")
set.seed(61001)

for (trial in 1:5000) {
  indices = sample(length(clause_pool), 3)
  clauses = clause_pool[indices]
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [3cl %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("3cl-%d", trial))
}
cat(sprintf("  3-clause: %d tests, %d failures\n", n_tests, n_failures))

# === Random 4-clause combinations ===
cat("\n=== Random 4-clause combinations ===\n")
set.seed(61002)

for (trial in 1:3000) {
  indices = sample(length(clause_pool), 4)
  clauses = clause_pool[indices]
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [4cl %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("4cl-%d", trial))
}
cat(sprintf("  4-clause: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
