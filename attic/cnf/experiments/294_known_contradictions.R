#!/usr/bin/env Rscript
# Test with formulas that are known to be contradictions (unsatisfiable).
# The simplifier should detect some of these.
# Verify that the truth table is all FALSE.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Simple pairwise contradictions ===
cat("=== Pairwise contradictions ===\n")
set.seed(294001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  V1 = CnfSymbol(u, "V1", dom)
  V2 = CnfSymbol(u, "V2", dom)

  # For each value of V1, the required V2 value is different and conflicts
  # V1=a -> V2=b, V1=b -> V2=c, V1=c -> V2=d, V1=d -> V2=a
  # But then V2=b -> V1=a, V1=a -> V2=b (consistent so far)
  # The trick is to also have: V2=b -> V1 != a (via another clause)

  # Simple contradiction: V1 in {a,b} and V1 in {c,d}
  r1 = sample(dom, 2)
  r2 = setdiff(dom, r1)
  clauses = list(
    as.CnfClause(V1 %among% r1),
    as.CnfClause(V1 %among% r2)
  )

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [pair-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  if (any(truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [pair-%d]: contradiction not detected\n", trial))
  }
}
cat(sprintf("  Pairwise contradictions: %d tests, %d failures\n", n_tests, n_failures))

# === Three-way contradictions ===
cat("\n=== Three-way contradictions ===\n")
set.seed(294002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  V1 = CnfSymbol(u, "V1", dom)

  # V1 in {a,b} and V1 in {b,c} and V1 in {a,c}
  # intersection = empty
  clauses = list(
    as.CnfClause(V1 %among% sample(dom, 2)),
    as.CnfClause(V1 %among% sample(dom, 2)),
    as.CnfClause(V1 %among% sample(dom, 2))
  )

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [three-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [three-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Three-way contradictions: %d tests, %d failures\n", n_tests, n_failures))

# === Cascade contradictions ===
cat("\n=== Cascade contradictions ===\n")
set.seed(294003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  V1 = CnfSymbol(u, "V1", dom)
  V2 = CnfSymbol(u, "V2", dom)
  V3 = CnfSymbol(u, "V3", dom)

  # V1 = a (unit)
  # V1 in {b,c} | V2 in {a} -> SSE: V2 = a
  # V2 in {b,c} | V3 in {a} -> SSE: V3 = a
  # V3 in {b,c} (contradicts V3 = a)
  clauses = list(
    as.CnfClause(V1 %among% "a"),
    as.CnfClause(V1 %among% c("b", "c") | V2 %among% "a"),
    as.CnfClause(V2 %among% c("b", "c") | V3 %among% "a"),
    as.CnfClause(V3 %among% c("b", "c"))
  )

  clauses = clauses[sample(length(clauses))]

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cascade-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  if (any(truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cascade-%d]: contradiction not detected\n", trial))
  }
}
cat(sprintf("  Cascade contradictions: %d tests, %d failures\n", n_tests, n_failures))

# === Random formulas that happen to be contradictions ===
cat("\n=== Random contradictions ===\n")
set.seed(294004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Many unit-like clauses in binary domain -> high chance of contradiction
  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [rand-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [rand-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Random contradictions: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
