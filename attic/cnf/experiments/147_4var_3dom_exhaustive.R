#!/usr/bin/env Rscript
# Semi-exhaustive testing with 4 variables, domain 3.
# With ranges of size 1-2 (not full domain), there are
# C(3,1) + C(3,2) = 6 range choices per symbol.
# Each clause uses 2-3 out of 4 symbols.
# We generate a large pool and test random 2-4 clause combinations.
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

cat("=== 4 vars domain 3, semi-exhaustive ===\n")

u = CnfUniverse()
dom = c("a", "b", "c")
A = CnfSymbol(u, "A", dom)
B = CnfSymbol(u, "B", dom)
C = CnfSymbol(u, "C", dom)
D = CnfSymbol(u, "D", dom)
syms = list(A = A, B = B, C = C, D = D)

# Generate all possible ranges (subsets of size 1-2)
all_ranges = list()
for (r in 1:2) {
  combos = combn(dom, r, simplify = FALSE)
  all_ranges = c(all_ranges, combos)
}

# Generate a diverse clause pool
cat("Generating clause pool...\n")
clause_pool = list()
sym_names = names(syms)

# All 2-symbol clauses with all range combinations
sym_pairs = combn(sym_names, 2, simplify = FALSE)
for (sp in sym_pairs) {
  for (r1 in all_ranges) {
    for (r2 in all_ranges) {
      cl = as.CnfClause(syms[[sp[1]]] %among% r1 | syms[[sp[2]]] %among% r2)
      if (!isTRUE(unclass(cl))) {
        clause_pool[[length(clause_pool) + 1]] = cl
      }
    }
  }
}

# Sample of 3-symbol clauses (all combinations too large)
sym_triples = combn(sym_names, 3, simplify = FALSE)
set.seed(147001)
for (sp in sym_triples) {
  for (i in 1:20) {
    r1 = sample(all_ranges, 1)[[1]]
    r2 = sample(all_ranges, 1)[[1]]
    r3 = sample(all_ranges, 1)[[1]]
    cl = as.CnfClause(syms[[sp[1]]] %among% r1 | syms[[sp[2]]] %among% r2 | syms[[sp[3]]] %among% r3)
    if (!isTRUE(unclass(cl))) {
      clause_pool[[length(clause_pool) + 1]] = cl
    }
  }
}

cat(sprintf("Pool size: %d clauses\n", length(clause_pool)))

# Test all 2-clause combinations from a random subset
set.seed(147002)
pool_subset = sample(seq_along(clause_pool), min(200, length(clause_pool)))

cat("\n=== All 2-clause combos from subset ===\n")
for (i in seq_along(pool_subset)) {
  if (i %% 50 == 0) cat(sprintf("  Progress: %d/%d, %d tests, %d failures\n", i, length(pool_subset), n_tests, n_failures))
  for (j in seq_along(pool_subset)) {
    if (i >= j) next
    ci = pool_subset[i]
    cj = pool_subset[j]

    clauses = list(clause_pool[[ci]], clause_pool[[cj]])

    n_tests = n_tests + 1
    result = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [2cl-%d-%d]: %s\n", ci, cj, result$message)); next
    }
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [2cl-%d-%d]: semantic mismatch\n", ci, cj))
    }
  }
}
cat(sprintf("  2-clause combos: %d tests, %d failures\n", n_tests, n_failures))

# Test random 3-4 clause combinations
cat("\n=== Random 3-4 clause combos ===\n")
set.seed(147003)
for (trial in 1:5000) {
  if (trial %% 1000 == 0) cat(sprintf("  Progress: %d/5000, %d tests, %d failures\n", trial, n_tests, n_failures))
  n_cl = sample(3:4, 1)
  chosen = sample(seq_along(clause_pool), n_cl)
  clauses = clause_pool[chosen]

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multi-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multi-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Random multi-clause: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
