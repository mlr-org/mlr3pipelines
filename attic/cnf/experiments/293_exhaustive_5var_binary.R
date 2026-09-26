#!/usr/bin/env Rscript
# Exhaustive test: 5 variables, binary domain (0/1), 3-clause formulas.
# 2^5 = 32 assignments. This is manageable and covers many interaction patterns.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== 5-var binary 3-clause ===\n")
set.seed(293001)

u = CnfUniverse()
dom = c("0", "1")
sym_names = paste0("V", 1:5)
syms = list()
for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

for (trial in 1:100000) {
  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [5vb-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [5vb-%d]: semantic mismatch\n", trial))
    if (n_failures >= 5) break
  }
}
cat(sprintf("  5-var binary 3-clause: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
