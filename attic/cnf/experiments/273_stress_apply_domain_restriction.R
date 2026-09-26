#!/usr/bin/env Rscript
# Stress test apply_domain_restriction specifically:
# Focus on the reverse direction update (lines 171-185) and the
# on_update_range cascading (line 224).
# Create formulas where domain restriction causes NOT-subset relationships
# to change in the reverse direction (other clauses' subsets of us change).
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Reverse direction updates ===
cat("=== Reverse direction updates ===\n")
set.seed(273001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create clauses where one clause has a wide range and others are narrower
  # SSE on the wide clause will shrink it, changing reverse subset relations
  wide_range = sample(dom, 3)
  narrow_range = sample(dom, 1)

  s1 = "V1"; s2 = "V2"; s3 = "V3"
  # Clause 1: V1 in wide | V2 in {x}
  # Clause 2: V1 in {subset of wide} | V2 in {y}  (where y != x)
  # SSE between them restricts V2 in clause 2 to intersection, possibly eliminating it
  clauses = list(
    as.CnfClause(syms[[s1]] %among% wide_range | syms[[s2]] %among% sample(dom, 1)),
    as.CnfClause(syms[[s1]] %among% sample(wide_range, min(2, length(wide_range))) | syms[[s2]] %among% sample(dom, sample(1:2, 1)))
  )
  # Add clauses with V3 that depend on V1's final range
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
    cat(sprintf("ERROR [rev-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [rev-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Reverse direction updates: %d tests, %d failures\n", n_tests, n_failures))

# === Domain restriction with 2nd-order cascade ===
cat("\n=== 2nd-order cascade from restriction ===\n")
set.seed(273002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create a formula with many 2-symbol clauses that could interact via 2nd-order SSE
  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = 2  # All 2-symbol clauses
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [2ndcasc-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [2ndcasc-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2nd-order cascade from restriction: %d tests, %d failures\n", n_tests, n_failures))

# === Multiple restrictions on same clause ===
cat("\n=== Multiple restrictions same clause ===\n")
set.seed(273003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create multiple units and a wide clause that will be restricted by all of them
  clauses = list()
  # 2-3 units for different symbols
  unit_syms = sample(sym_names, sample(2:3, 1))
  for (s in unit_syms) {
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
  }
  # Wide clause using all symbols
  wide_atoms = lapply(sym_names, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
  clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, wide_atoms))

  # More clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
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
    cat(sprintf("ERROR [multrestr-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multrestr-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multiple restrictions same clause: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
