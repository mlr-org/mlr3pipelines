#!/usr/bin/env Rscript
# Focus on SSE that creates new unit clauses, which then cascade.
# This exercises the eliminate_symbol_from_clause -> register_unit path.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== SSE creating units ===\n")
set.seed(282001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create a 2-symbol clause that SSE will reduce to 1-symbol (unit)
  # Clause A: V1 in {a,b} | V2 in {c}
  # Clause B: V1 in {a,b,c} | V2 in {c,d}
  # B is a superset of A on V1. SSE restricts V2 in B to intersect with V2 in A: {c} ∩ {c,d} = {c}
  # Now B has V1 in {a,b,c} | V2 in {c}, still 2 symbols
  # But if: Clause A: V1 in {a,b} | V2 in {c}
  #         Clause B: V1 in {a,b} | V2 in {c,d} | V3 in {x}
  # Then B is superset on V1 and V3 (V3 not in A, so A is trivially subset on V3)
  # SSE restricts V2 in B: {c,d} ∩ {c} = {c}. B is still 3 symbols, not a unit.
  # For SSE to create a unit, we need: clause has 2 symbols, SSE eliminates one of them
  # e.g. Clause A: V1 in {a} | V2 in {c}
  #      Clause B: V1 in {a,b} | V2 in {d}
  # A subset of B on V1 (a ⊆ a,b). SSE: V2 in B ∩ V2 in A = {d} ∩ {c} = {} → V2 eliminated from B
  # B becomes: V1 in {a,b} (a unit!)

  # Generate patterns that are likely to create this scenario
  s1 = sample(sym_names, 1)
  s2 = sample(setdiff(sym_names, s1), 1)

  # Clause A: s1 in {subset} | s2 in {r1}
  s1_range_a = sample(dom, sample(1:2, 1))
  s2_range_a = sample(dom, sample(1:2, 1))

  # Clause B: s1 in {superset of A} | s2 in {disjoint from A on s2}
  s1_range_b = unique(c(s1_range_a, sample(dom, sample(0:2, 1))))
  s2_range_b = sample(setdiff(dom, s2_range_a), min(2, length(setdiff(dom, s2_range_a))))
  if (length(s2_range_b) == 0) next

  clauses = list(
    as.CnfClause(syms[[s1]] %among% s1_range_a | syms[[s2]] %among% s2_range_a),
    as.CnfClause(syms[[s1]] %among% s1_range_b | syms[[s2]] %among% s2_range_b)
  )

  # Add more clauses that interact with the potential new unit
  for (j in 1:sample(2:4, 1)) {
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
    cat(sprintf("ERROR [sseunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sseunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE creating units: %d tests, %d failures\n", n_tests, n_failures))

# === Chain reactions: SSE -> unit -> more SSE ===
cat("\n=== Chain reactions ===\n")
set.seed(282002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create a chain where SSE creates a unit, which propagates to affect other clauses
  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Chain reactions: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
