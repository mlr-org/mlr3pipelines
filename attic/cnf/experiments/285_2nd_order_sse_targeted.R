#!/usr/bin/env Rscript
# Targeted testing of 2nd-order SSE (lines 330-461).
# 2nd-order SSE happens when two clauses together can restrict a third clause.
# Clause A and Clause B each differ from Clause C on some symbols.
# If A's non-subset symbol on C is the same as B's, they can jointly restrict C.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Oneend 2nd-order SSE ===
cat("=== Oneend 2nd-order SSE ===\n")
set.seed(285001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  V1 = CnfSymbol(u, "V1", dom)
  V2 = CnfSymbol(u, "V2", dom)
  V3 = CnfSymbol(u, "V3", dom)

  # Create a scenario for oneend 2nd-order:
  # Target C: V1 in {x1} | V2 in {x2} | V3 in {x3}
  # Clause A: V1 in {subset of x1} | V2 in {subset of x2} | V3 in {y3} (differs on V3 only)
  #   -> A is subset of C on V1 and V2, not on V3. not_subset_count[A, C] = 1.
  # Clause B: V1 in {z1} | V2 in {subset of x2} | V3 in {w3} (differs on V1 and V3)
  #   -> B is subset of C on V2, not on V1 and V3. not_subset_count[B, C] = 2.
  #   -> Both A and B have V3 as a non-subset symbol. If their V3 ranges are disjoint
  #      outside C's V3 range, then 2nd-order SSE can restrict V3 in C.

  x1 = sample(dom, sample(1:3, 1))
  x2 = sample(dom, sample(1:3, 1))
  x3 = sample(dom, sample(1:3, 1))

  y3 = sample(dom, sample(1:2, 1))
  z1 = sample(dom, sample(1:2, 1))
  w3 = sample(dom, sample(1:2, 1))

  clauses = list(
    as.CnfClause(V1 %among% x1 | V2 %among% x2 | V3 %among% x3),
    as.CnfClause(V1 %among% sample(x1, min(length(x1), sample(1:2, 1))) |
                  V2 %among% sample(x2, min(length(x2), sample(1:2, 1))) |
                  V3 %among% y3),
    as.CnfClause(V1 %among% z1 |
                  V2 %among% sample(x2, min(length(x2), sample(1:2, 1))) |
                  V3 %among% w3)
  )

  # Add more clauses
  for (j in 1:sample(1:3, 1)) {
    syms = list(V1, V2, V3)
    sym_names = c("V1", "V2", "V3")
    n_sym = sample(2:3, 1)
    chosen = sample(3, n_sym)
    atoms = lapply(chosen, function(i) syms[[i]] %among% sample(dom, sample(1:3, 1)))
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
    cat(sprintf("ERROR [oneend-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [oneend-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Oneend 2nd-order SSE: %d tests, %d failures\n", n_tests, n_failures))

# === Twoend 2nd-order SSE ===
cat("\n=== Twoend 2nd-order SSE ===\n")
set.seed(285002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  V1 = CnfSymbol(u, "V1", dom)
  V2 = CnfSymbol(u, "V2", dom)
  V3 = CnfSymbol(u, "V3", dom)

  # For twoend: clause A has not_subset_count[A, C] = 2
  # Another clause B has not_subset_count[B, C] <= 2 and shares one of the non-subset symbols with A
  x1 = sample(dom, sample(2:3, 1))
  x2 = sample(dom, sample(2:3, 1))
  x3 = sample(dom, sample(2:3, 1))

  # Clause C (target)
  clauses = list(
    as.CnfClause(V1 %among% x1 | V2 %among% x2 | V3 %among% x3)
  )

  # Clause A: differs on V1 and V3 from C (twoend)
  a_v1 = sample(dom, sample(1:2, 1))
  a_v2 = sample(x2, min(length(x2), sample(1:2, 1)))
  a_v3 = sample(dom, sample(1:2, 1))
  cl = as.CnfClause(V1 %among% a_v1 | V2 %among% a_v2 | V3 %among% a_v3)
  if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl

  # Clause B: shares non-subset on V1 or V3 with A
  b_v1 = sample(dom, sample(1:2, 1))
  b_v2 = sample(x2, min(length(x2), sample(1:2, 1)))
  b_v3 = sample(dom, sample(1:2, 1))
  cl = as.CnfClause(V1 %among% b_v1 | V2 %among% b_v2 | V3 %among% b_v3)
  if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl

  # Add more random clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(c("V1", "V2", "V3"), n_sym)
    syms_map = list(V1 = V1, V2 = V2, V3 = V3)
    atoms = lapply(chosen, function(s) syms_map[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[sample(length(clauses))]
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [twoend-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [twoend-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Twoend 2nd-order SSE: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
