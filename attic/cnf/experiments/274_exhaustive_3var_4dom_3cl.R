#!/usr/bin/env Rscript
# Semi-exhaustive test: 3 variables, domain size 4, sampling 3-clause formulas.
# Each atom uses a random 1-2 value range from a 4-value domain.
# This is a larger space than 3-var 3-dom, testing more diverse patterns.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== 3-var 4-dom 3-clause ===\n")
set.seed(274001)

u = CnfUniverse()
dom = c("a", "b", "c", "d")
sym_names = paste0("V", 1:3)
syms = list()
for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

# Generate clause pool: for each subset of symbols (1-3), for each range combo
# This pool is large but we sample from it
n_total = 100000

for (trial in 1:n_total) {
  # Generate 3 random clauses
  clauses = lapply(1:3, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [exh-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [exh-%d]: semantic mismatch\n", trial))
    if (n_failures >= 5) break
  }
}
cat(sprintf("  3-var 4-dom 3-clause: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
