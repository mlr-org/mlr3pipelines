#!/usr/bin/env Rscript
# Target specific code paths that might not be exercised by random testing:
# 1. Line 606: break inner loop when ousr is NULL (outer clause eliminated by inner's SSE)
# 2. Line 615: outer clause eliminated/unitized during inner's processing
# 3. Line 692: HLA hidden tautology via multi-step range accumulation
# 4. Line 770: Unit HLA hidden subsumption
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Patterns designed for pairwise loop interactions ===
cat("=== Pairwise loop interactions ===\n")
set.seed(276001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create clauses where processing pair (i,j) could cascade to affect clause k
  # and then processing pair (i,k) or (j,k) encounters the modified clause
  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    # Use overlapping but not identical ranges
    atoms = lapply(chosen, function(s) {
      start = sample(1:(length(dom)-1), 1)
      end = min(start + sample(1:2, 1), length(dom))
      syms[[s]] %among% dom[start:end]
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [pairwise-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [pairwise-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Pairwise loop interactions: %d tests, %d failures\n", n_tests, n_failures))

# === HLA multi-step range accumulation ===
cat("\n=== HLA multi-step ===\n")
set.seed(276002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # For HLA multi-step: need a clause C and multiple helper clauses H1, H2, ...
  # Each Hi is a subset of C on all but one symbol Si.
  # HLA adds complement of Hi's Si range to C's Si range.
  # After multiple steps, C might become a tautology (hidden tautology elimination).
  target_sym = "V1"
  # Target clause: V1 in {a,b} | V2 in {x} | V3 in {y}
  target_v1_range = sample(dom, 2)
  target_v2_range = sample(dom, sample(1:2, 1))
  target_v3_range = sample(dom, sample(1:2, 1))

  clauses = list(
    as.CnfClause(syms[["V1"]] %among% target_v1_range |
                  syms[["V2"]] %among% target_v2_range |
                  syms[["V3"]] %among% target_v3_range)
  )

  # Helper clauses that are subsets on V2 and V3 but differ on V1
  # Each helper adds complement of its V1 range to target's V1
  for (j in 1:sample(2:4, 1)) {
    h_v1 = sample(dom, sample(1:3, 1))
    h_v2 = sample(target_v2_range, min(length(target_v2_range), sample(1:2, 1)))
    h_v3 = sample(target_v3_range, min(length(target_v3_range), sample(1:2, 1)))
    cl = as.CnfClause(syms[["V1"]] %among% h_v1 |
                       syms[["V2"]] %among% h_v2 |
                       syms[["V3"]] %among% h_v3)
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # More random clauses
  for (j in 1:sample(1:3, 1)) {
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
    cat(sprintf("ERROR [hlamulti-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hlamulti-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA multi-step: %d tests, %d failures\n", n_tests, n_failures))

# === Unit HLA with many non-unit candidates ===
cat("\n=== Unit HLA stress ===\n")
set.seed(276003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create a unit, then many non-unit clauses that interact with unit HLA
  unit_sym = sample(sym_names, 1)
  unit_range = sample(dom, sample(1:2, 1))
  clauses = list(as.CnfClause(syms[[unit_sym]] %among% unit_range))

  # Many 3-4 symbol clauses that include the unit symbol
  for (j in 1:sample(4:8, 1)) {
    n_sym = sample(3:4, 1)
    chosen = unique(c(unit_sym, sample(setdiff(sym_names, unit_sym), n_sym - 1)))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
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
    cat(sprintf("ERROR [unithla-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unithla-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Unit HLA stress: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
