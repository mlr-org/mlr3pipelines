#!/usr/bin/env Rscript
# All clauses use the same single symbol. This exercises pure unit propagation
# and subsumption paths without any cross-symbol interactions.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Single symbol formulas ===\n")
set.seed(279001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom_size = sample(3:6, 1)
  dom = letters[1:dom_size]
  s = CnfSymbol(u, "V1", dom)

  n_cl = sample(2:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    range_size = sample(1:(dom_size - 1), 1)
    as.CnfClause(s %among% sample(dom, range_size))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [single-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [single-%d]: semantic mismatch\n", trial))
  }

  # The simplified formula should be at most 1 clause (intersection of all ranges)
  result_bare = unclass(result)
  if (!isTRUE(result_bare) && !isFALSE(result_bare) && length(result_bare) > 1) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [single-%d]: single-symbol formula not fully simplified (has %d clauses)\n", trial, length(result_bare)))
  }
}
cat(sprintf("  Single symbol formulas: %d tests, %d failures\n", n_tests, n_failures))

# === Two symbols, all clauses have both ===
cat("\n=== Two symbol saturation ===\n")
set.seed(279002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  V1 = CnfSymbol(u, "V1", dom)
  V2 = CnfSymbol(u, "V2", dom)

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    r1 = sample(dom, sample(1:3, 1))
    r2 = sample(dom, sample(1:3, 1))
    as.CnfClause(V1 %among% r1 | V2 %among% r2)
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [twosat-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [twosat-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Two symbol saturation: %d tests, %d failures\n", n_tests, n_failures))

# === Duplicate clauses ===
cat("\n=== Duplicate clauses ===\n")
set.seed(279003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(2:4, 1)
  base_clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  base_clauses = base_clauses[!sapply(base_clauses, function(x) isTRUE(unclass(x)))]
  if (length(base_clauses) < 2) next

  # Duplicate some clauses
  n_dups = sample(1:3, 1)
  dup_idx = sample(length(base_clauses), n_dups, replace = TRUE)
  clauses = c(base_clauses, base_clauses[dup_idx])
  clauses = clauses[sample(length(clauses))]

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dup-%d]: %s\n", trial, result$message)); next
  }
  result_nodup = tryCatch(CnfFormula(base_clauses), error = function(e) e)
  if (inherits(result_nodup, "error")) next

  truth = evaluate_formula(result, u)
  truth_nodup = evaluate_formula(result_nodup, u)
  if (!all(truth == truth_nodup)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dup-%d]: duplicates change semantics\n", trial))
  }
}
cat(sprintf("  Duplicate clauses: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
