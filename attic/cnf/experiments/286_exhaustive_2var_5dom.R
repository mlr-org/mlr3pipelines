#!/usr/bin/env Rscript
# Exhaustive test: 2 variables, domain size 5, all possible 2-4 clause formulas.
# With 2 vars and dom=5, each atom has C(5,k) possible ranges for k=1..4.
# A clause with both vars has many possible combinations.
# We sample heavily from this space.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== 2-var 5-dom exhaustive ===\n")
set.seed(286001)

u = CnfUniverse()
dom = c("a", "b", "c", "d", "e")
V1 = CnfSymbol(u, "V1", dom)
V2 = CnfSymbol(u, "V2", dom)

for (trial in 1:100000) {
  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    if (n_sym == 1) {
      sym = sample(c("V1", "V2"), 1)
      if (sym == "V1") {
        as.CnfClause(V1 %among% sample(dom, sample(1:4, 1)))
      } else {
        as.CnfClause(V2 %among% sample(dom, sample(1:4, 1)))
      }
    } else {
      r1 = sample(dom, sample(1:4, 1))
      r2 = sample(dom, sample(1:4, 1))
      as.CnfClause(V1 %among% r1 | V2 %among% r2)
    }
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [2v5d-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [2v5d-%d]: semantic mismatch\n", trial))
    if (n_failures >= 5) break
  }
}
cat(sprintf("  2-var 5-dom exhaustive: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
