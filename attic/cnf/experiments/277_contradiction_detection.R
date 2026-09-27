#!/usr/bin/env Rscript
# Test contradiction detection: formulas that should simplify to FALSE.
# Focus on cases where unit propagation creates empty domains.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Direct contradictions from conflicting units ===
cat("=== Conflicting units ===\n")
set.seed(277001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  s = CnfSymbol(u, "V1", dom)

  # Two units with disjoint ranges for the same symbol -> contradiction
  r1 = sample(dom, sample(1:2, 1))
  r2 = sample(setdiff(dom, r1), sample(1:min(2, length(setdiff(dom, r1))), 1))

  cl1 = as.CnfClause(s %among% r1)
  cl2 = as.CnfClause(s %among% r2)

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(list(cl1, cl2)), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [conflict-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  if (any(truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [conflict-%d]: should be all FALSE but got some TRUE\n", trial))
  }
}
cat(sprintf("  Conflicting units: %d tests, %d failures\n", n_tests, n_failures))

# === Contradiction from SSE cascade ===
cat("\n=== SSE cascade to contradiction ===\n")
set.seed(277002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  V1 = CnfSymbol(u, "V1", dom)
  V2 = CnfSymbol(u, "V2", dom)

  # Create clauses whose SSE leads to empty ranges:
  # V1 in {a} | V2 in {b}
  # V1 in {a} | V2 in {c}
  # These SSE on V2: restrict to intersection {b} ∩ {c} = empty
  # But only if V1 ranges are identical (so V1 is subset)
  v1_range = sample(dom, 1)
  v2_r1 = sample(dom, sample(1:2, 1))
  v2_r2 = sample(dom, sample(1:2, 1))

  # Only expect contradiction if V2 ranges are disjoint
  expect_contradiction = length(intersect(v2_r1, v2_r2)) == 0

  cl1 = as.CnfClause(V1 %among% v1_range | V2 %among% v2_r1)
  cl2 = as.CnfClause(V1 %among% v1_range | V2 %among% v2_r2)

  if (isTRUE(unclass(cl1)) || isTRUE(unclass(cl2))) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(list(cl1, cl2)), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ssecasc-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(list(unclass(cl1), unclass(cl2)), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ssecasc-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE cascade to contradiction: %d tests, %d failures\n", n_tests, n_failures))

# === Multi-step contradiction from unit propagation ===
cat("\n=== Multi-step unit propagation contradiction ===\n")
set.seed(277003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Build a chain: unit on V1 restricts V2 range in clause, making V2 a unit,
  # which restricts V3, etc.
  clauses = list()

  # Unit for V1
  v1_val = sample(dom, 1)
  clauses[[1]] = as.CnfClause(syms[["V1"]] %among% v1_val)

  # V1 in {something disjoint from v1_val} | V2 in {v2_val}
  v1_other = sample(setdiff(dom, v1_val), 1)
  v2_val = sample(dom, 1)
  clauses[[2]] = as.CnfClause(syms[["V1"]] %among% v1_other | syms[["V2"]] %among% v2_val)

  # V2 in {something disjoint from v2_val} | V3 in {v3_val}
  v2_other = sample(setdiff(dom, v2_val), 1)
  v3_val = sample(dom, 1)
  clauses[[3]] = as.CnfClause(syms[["V2"]] %among% v2_other | syms[["V3"]] %among% v3_val)

  # V3 in {something disjoint from v3_val} - this should cause contradiction
  v3_other = sample(setdiff(dom, v3_val), 1)
  clauses[[4]] = as.CnfClause(syms[["V3"]] %among% v3_other)

  # Add some random extra clauses
  for (j in 1:sample(0:3, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[sample(length(clauses))]
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multiunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multiunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multi-step unit propagation: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
