#!/usr/bin/env Rscript
# Target specific structural patterns in the simplification engine:
# 1. Formulas that trigger subsumption (not_subset_count == 0) during pairwise
# 2. Formulas that trigger SSE (not_subset_count == 1) during pairwise
# 3. Formulas that trigger 2nd-order SSE after the pairwise loop
# 4. Formulas that trigger HLA hidden subsumption and hidden tautology
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Subsumption patterns ===
cat("=== Subsumption patterns ===\n")
set.seed(268001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create clause C1, then create C2 which is a superset of C1 (C1 subsumes C2)
  n_sym = sample(2:3, 1)
  chosen = sample(sym_names, n_sym)
  c1_ranges = lapply(chosen, function(s) sample(dom, sample(1:2, 1)))
  names(c1_ranges) = chosen

  # C2: same or wider ranges, possibly more symbols
  c2_chosen = unique(c(chosen, sample(sym_names, sample(0:1, 1))))
  c2_ranges = lapply(c2_chosen, function(s) {
    if (s %in% chosen) {
      r = c1_ranges[[s]]
      extra = sample(setdiff(dom, r), min(sample(0:2, 1), length(setdiff(dom, r))))
      unique(c(r, extra))
    } else {
      sample(dom, sample(1:3, 1))
    }
  })
  names(c2_ranges) = c2_chosen

  c1_atoms = lapply(chosen, function(s) syms[[s]] %among% c1_ranges[[s]])
  cl1 = as.CnfClause(Reduce(`|`, c1_atoms))

  c2_atoms = lapply(c2_chosen, function(s) syms[[s]] %among% c2_ranges[[s]])
  cl2 = as.CnfClause(Reduce(`|`, c2_atoms))

  if (isTRUE(unclass(cl1)) || isTRUE(unclass(cl2))) next

  # Add more random clauses
  clauses = list(cl1, cl2)
  for (j in 1:sample(1:4, 1)) {
    n_s = sample(1:3, 1)
    ch = sample(sym_names, n_s)
    atoms = lapply(ch, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[sample(length(clauses))]

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [subs-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [subs-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Subsumption patterns: %d tests, %d failures\n", n_tests, n_failures))

# === SSE patterns (not_subset_count == 1) ===
cat("\n=== SSE patterns ===\n")
set.seed(268002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create two clauses with same symbols, all ranges equal except one
  n_sym = sample(2:3, 1)
  chosen = sample(sym_names, n_sym)
  ranges = lapply(chosen, function(s) sample(dom, sample(2:3, 1)))
  names(ranges) = chosen

  # SSE symbol: one symbol differs
  sse_sym = sample(chosen, 1)
  ranges_2 = ranges
  ranges_2[[sse_sym]] = sample(dom, sample(1:3, 1))

  c1_atoms = lapply(chosen, function(s) syms[[s]] %among% ranges[[s]])
  c2_atoms = lapply(chosen, function(s) syms[[s]] %among% ranges_2[[s]])
  cl1 = as.CnfClause(Reduce(`|`, c1_atoms))
  cl2 = as.CnfClause(Reduce(`|`, c2_atoms))

  if (isTRUE(unclass(cl1)) || isTRUE(unclass(cl2))) next

  clauses = list(cl1, cl2)
  for (j in 1:sample(1:3, 1)) {
    n_s = sample(1:3, 1)
    ch = sample(sym_names, n_s)
    atoms = lapply(ch, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[sample(length(clauses))]

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sse-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sse-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE patterns: %d tests, %d failures\n", n_tests, n_failures))

# === HLA-triggering patterns ===
cat("\n=== HLA patterns ===\n")
set.seed(268003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # For HLA: need clause C where there exists another clause C' such that
  # C' is a subset of C for all but one symbol, and adding the complement
  # of that symbol in C' to C creates a tautology or reveals subsumption.
  # Create such patterns:
  n_sym = sample(2:3, 1)
  chosen = sample(sym_names, n_sym)

  # Base clause with narrow ranges
  base_ranges = lapply(chosen, function(s) sample(dom, 1))
  names(base_ranges) = chosen

  # Target clause: broader, with one symbol whose complement union covers domain
  hla_sym = sample(chosen, 1)
  target_ranges = lapply(chosen, function(s) {
    if (s == hla_sym) {
      # Leave room for complement addition
      sample(dom, sample(1:2, 1))
    } else {
      # Wider than base (so base is subset)
      r = base_ranges[[s]]
      extra = sample(setdiff(dom, r), min(1, length(setdiff(dom, r))))
      c(r, extra)
    }
  })
  names(target_ranges) = chosen

  cl_base = as.CnfClause(Reduce(`|`, lapply(chosen, function(s) syms[[s]] %among% base_ranges[[s]])))
  cl_target = as.CnfClause(Reduce(`|`, lapply(chosen, function(s) syms[[s]] %among% target_ranges[[s]])))

  if (isTRUE(unclass(cl_base)) || isTRUE(unclass(cl_target))) next

  clauses = list(cl_base, cl_target)
  for (j in 1:sample(2:4, 1)) {
    n_s = sample(1:3, 1)
    ch = sample(sym_names, n_s)
    atoms = lapply(ch, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[sample(length(clauses))]
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [hla-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hla-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA patterns: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
