#!/usr/bin/env Rscript
# Semi-exhaustive testing: 3 variables, domain 5, 2-clause combinations
# This generates a large pool of clauses and tests all 2-clause combinations
# This is a new exhaustive benchmark with a different parameter space.
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

u = CnfUniverse()
dom = c("a", "b", "c", "d", "e")
A = CnfSymbol(u, "A", dom)
B = CnfSymbol(u, "B", dom)
C = CnfSymbol(u, "C", dom)

# Generate clause pool: all non-tautological clauses with 1-2 symbols,
# and ranges of size 1-2 (to keep the pool manageable)
cat("=== Generating clause pool ===\n")

clause_pool = list()
sym_list = list(A = A, B = B, C = C)
sym_names = names(sym_list)

# Single-symbol clauses (units) with ranges 1-2
for (s in sym_names) {
  for (k in 1:2) {
    combos = combn(dom, k, simplify = FALSE)
    for (vals in combos) {
      cl = as.CnfClause(sym_list[[s]] %among% vals)
      if (!isTRUE(unclass(cl))) clause_pool[[length(clause_pool) + 1]] = cl
    }
  }
}

# Two-symbol clauses with ranges 1-2 each
for (i in 1:(length(sym_names)-1)) {
  for (j in (i+1):length(sym_names)) {
    s1 = sym_names[i]; s2 = sym_names[j]
    for (k1 in 1:2) {
      for (k2 in 1:2) {
        combos1 = combn(dom, k1, simplify = FALSE)
        combos2 = combn(dom, k2, simplify = FALSE)
        for (v1 in combos1) {
          for (v2 in combos2) {
            cl = as.CnfClause(sym_list[[s1]] %among% v1 | sym_list[[s2]] %among% v2)
            if (!isTRUE(unclass(cl))) clause_pool[[length(clause_pool) + 1]] = cl
          }
        }
      }
    }
  }
}

cat(sprintf("  Clause pool size: %d\n", length(clause_pool)))

# Test all 2-clause combinations
cat("\n=== All 2-clause combinations ===\n")
n_combos = choose(length(clause_pool), 2)
cat(sprintf("  Total combinations: %d\n", n_combos))

for (i in 1:(length(clause_pool)-1)) {
  for (j in (i+1):length(clause_pool)) {
    clauses = list(clause_pool[[i]], clause_pool[[j]])

    n_tests = n_tests + 1
    result = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [exh-%d-%d]: %s\n", i, j, result$message)); next
    }
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [exh-%d-%d]: semantic mismatch\n", i, j))
    }
  }
  if (i %% 50 == 0) cat(sprintf("  Progress: %d/%d outer iterations, %d tests, %d failures\n",
                                  i, length(clause_pool)-1, n_tests, n_failures))
}

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
