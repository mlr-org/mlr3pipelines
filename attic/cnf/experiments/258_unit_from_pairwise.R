#!/usr/bin/env Rscript
# Test formulas designed to create new units during the pairwise SSE loop.
# This triggers the code path where `meta_idx > meta_idx_outer` in
# apply_domain_restriction, and the `use_inso` optimization in register_unit.
# Key insight: SSE during pairwise comparison can reduce a clause to a unit,
# which then triggers unit propagation on remaining unprocessed clauses.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === SSE creating units ===
cat("=== SSE creates units ===\n")
set.seed(258001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create clauses where SSE should reduce some to units:
  # Clause 1: V1 in {a,b} | V2 in {a}       (2 symbols)
  # Clause 2: V1 in {a,b} | V2 in {b,c,d}   (SSE on V2 -> V2 restricted to intersection empty -> removes V2 -> V1 in {a,b} becomes unit)
  # But we randomize this pattern
  r1 = sample(dom, 2)
  r2_a = sample(dom, 1)
  r2_b = setdiff(dom, r2_a)

  clauses = list()
  s1 = sample(sym_names, 1); s2 = sample(setdiff(sym_names, s1), 1)
  cl1 = as.CnfClause(syms[[s1]] %among% r1 | syms[[s2]] %among% r2_a)
  cl2 = as.CnfClause(syms[[s1]] %among% r1 | syms[[s2]] %among% r2_b)
  clauses = list(cl1, cl2)

  # Add more clauses that will be affected by the unit propagation
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

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
cat(sprintf("  SSE creates units: %d tests, %d failures\n", n_tests, n_failures))

# === Chain: SSE -> unit -> propagation -> another unit ===
cat("\n=== Unit chain creation ===\n")
set.seed(258002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Design: V1 unit restricts V2, which creates another unit,
  # which then propagates to remaining clauses
  v1_val = sample(dom, 1)
  v2_val = sample(dom, 1)
  v3_range = sample(dom, 2)

  clauses = list(
    as.CnfClause(syms[["V1"]] %among% v1_val),              # Unit for V1
    as.CnfClause(syms[["V1"]] %among% v1_val | syms[["V2"]] %among% v2_val),  # After UP: becomes unit for V2
    as.CnfClause(syms[["V2"]] %among% v2_val | syms[["V3"]] %among% v3_range) # After chain: affected by V2 unit
  )

  # Add random clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # Shuffle clause order to test different processing sequences
  clauses = clauses[sample(length(clauses))]
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

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
cat(sprintf("  Unit chain creation: %d tests, %d failures\n", n_tests, n_failures))

# === Many concurrent units from pairwise ===
cat("\n=== Concurrent units ===\n")
set.seed(258003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create pairs that could produce units via SSE for different symbols
  clauses = list()
  for (si in 1:min(3, length(sym_names))) {
    s1 = sym_names[si]
    s2 = sym_names[((si) %% length(sym_names)) + 1]
    r = sample(dom, 2)
    r_a = sample(dom, 1)
    r_b = sample(setdiff(dom, r_a), 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s1]] %among% r | syms[[s2]] %among% r_a)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s1]] %among% r | syms[[s2]] %among% r_b)
  }

  # More random clauses
  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [concurrent-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [concurrent-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Concurrent units: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
