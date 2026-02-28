#!/usr/bin/env Rscript
# Test that the simplifier produces semantically correct results regardless
# of the order clauses are fed in. While the simplifier is non-confluent
# (different orders may produce structurally different results), the
# semantic truth must be identical.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Permutation invariance stress ===\n")
set.seed(65001)

for (trial in 1:2000) {
  u = CnfUniverse()
  n_vars = sample(2:4, 1)
  dom_size = sample(2:4, 1)
  dom = paste0("d", 1:dom_size)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(3:7, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:min(n_vars, 3), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:max(1, dom_size-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  # Build formula from original order
  f1 = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f1)) next

  # Build from reversed order
  f2 = tryCatch(CnfFormula(rev(clauses)), error = function(e) NULL)
  if (is.null(f2)) next

  # Build from random shuffled order
  f3 = tryCatch(CnfFormula(clauses[sample(n_cl)]), error = function(e) NULL)
  if (is.null(f3)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)

  n_tests = n_tests + 1
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [perm-%d]: original != reversed\n", trial))
  }
  n_tests = n_tests + 1
  if (!all(t1 == t3)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [perm-%d]: original != shuffled\n", trial))
  }
}

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
