#!/usr/bin/env Rscript
# Exhaustive test: 3 variables with domain size 3
# There are 7 possible non-tautological, non-empty atoms per variable (2^3 - 2 subsets)
# And 7^3 = 343 possible 3-symbol clauses (plus smaller clauses)
# Test all 2-clause combinations from a subset of these
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
    cat(sprintf("  Assignment: %s\n",
      paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
    return(FALSE)
  }
  TRUE
}

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
B = CnfSymbol(u, "B", c("b1", "b2", "b3"))
C = CnfSymbol(u, "C", c("c1", "c2", "c3"))

# Generate all non-trivial subsets of each domain
all_subsets = function(dom) {
  n = length(dom)
  subs = list()
  for (i in 1:(2^n - 2)) {  # skip empty set and full set
    bits = as.logical(intToBits(i)[1:n])
    subs[[length(subs) + 1]] = dom[bits]
  }
  subs
}

dom_a = c("a1", "a2", "a3")
dom_b = c("b1", "b2", "b3")
dom_c = c("c1", "c2", "c3")

subs_a = all_subsets(dom_a)  # 6 subsets
subs_b = all_subsets(dom_b)
subs_c = all_subsets(dom_c)

# Generate all possible clauses with 1, 2, or 3 symbols
# This is a lot, so let's be systematic but sample for 3-symbol clauses
cat("=== Generating clause pool ===\n")
clause_pool = list()

# 1-symbol clauses (units)
for (s in subs_a) clause_pool[[length(clause_pool) + 1]] = as.CnfClause(A %among% s)
for (s in subs_b) clause_pool[[length(clause_pool) + 1]] = as.CnfClause(B %among% s)
for (s in subs_c) clause_pool[[length(clause_pool) + 1]] = as.CnfClause(C %among% s)

# 2-symbol clauses (AB, AC, BC)
for (sa in subs_a) for (sb in subs_b) {
  clause_pool[[length(clause_pool) + 1]] = as.CnfClause(A %among% sa | B %among% sb)
}
for (sa in subs_a) for (sc in subs_c) {
  clause_pool[[length(clause_pool) + 1]] = as.CnfClause(A %among% sa | C %among% sc)
}
for (sb in subs_b) for (sc in subs_c) {
  clause_pool[[length(clause_pool) + 1]] = as.CnfClause(B %among% sb | C %among% sc)
}

# 3-symbol clauses: too many (6*6*6=216), add a sample
set.seed(43001)
for (i in 1:100) {
  clause_pool[[length(clause_pool) + 1]] = as.CnfClause(
    A %among% sample(subs_a, 1)[[1]] |
    B %among% sample(subs_b, 1)[[1]] |
    C %among% sample(subs_c, 1)[[1]]
  )
}

cat(sprintf("  Clause pool: %d clauses\n", length(clause_pool)))

# === All 2-clause combinations from a subset ===
cat("\n=== All 2-clause combinations (sampled) ===\n")
set.seed(43002)
# Sample pairs from the clause pool
n_pairs = 3000
for (trial in 1:n_pairs) {
  idx = sample(length(clause_pool), 2, replace = TRUE)
  clauses = clause_pool[idx]
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [2-clause trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("2cl-%d", trial))
}
cat(sprintf("  2-clause combos: %d tests, %d failures\n", n_tests, n_failures))

# === 3-clause combinations ===
cat("\n=== 3-clause combinations (sampled) ===\n")
for (trial in 1:2000) {
  idx = sample(length(clause_pool), 3, replace = TRUE)
  clauses = clause_pool[idx]
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [3-clause trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("3cl-%d", trial))
}
cat(sprintf("  3-clause combos: %d tests, %d failures\n", n_tests, n_failures))

# === 4-5 clause combinations ===
cat("\n=== 4-5 clause combinations (sampled) ===\n")
for (trial in 1:1000) {
  n_cl = sample(4:5, 1)
  idx = sample(length(clause_pool), n_cl, replace = TRUE)
  clauses = clause_pool[idx]
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [4-5-clause trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("45cl-%d", trial))
}
cat(sprintf("  4-5 clause combos: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
