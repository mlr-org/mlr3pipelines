#!/usr/bin/env Rscript
# Unit conflict and intersection stress testing:
# When two units for the same symbol are created, they are intersected.
# This tests formulas where this intersection:
# 1. Produces a strict subset (narrowing)
# 2. Produces an empty set (contradiction)
# 3. Cascading: narrowed unit affects other clauses
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

evaluate_raw_clauses = function(clauses, universe) {
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }
  raw_truth
}

# === Pattern 1: Two overlapping units for same symbol ===
cat("=== Overlapping units same symbol ===\n")
set.seed(236001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Two units for A with overlapping ranges
  a_range1 = sample(dom, sample(2:3, 1))
  a_range2 = sample(dom, sample(2:3, 1))
  # Ensure overlap
  common = intersect(a_range1, a_range2)
  if (length(common) == 0) {
    shared = sample(dom, 1)
    a_range1 = unique(c(a_range1, shared))
    a_range2 = unique(c(a_range2, shared))
  }

  clauses = list(
    as.CnfClause(A %among% a_range1),
    as.CnfClause(A %among% a_range2)
  )

  # More clauses
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [overlap-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [overlap-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Overlapping units: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Three units for same symbol ===
cat("\n=== Three units same symbol ===\n")
set.seed(236002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Three units for A
  common_val = sample(dom, 1)
  a1 = unique(c(common_val, sample(dom, sample(1:2, 1))))
  a2 = unique(c(common_val, sample(dom, sample(1:2, 1))))
  a3 = unique(c(common_val, sample(dom, sample(1:2, 1))))

  clauses = list(
    as.CnfClause(A %among% a1),
    as.CnfClause(A %among% a2),
    as.CnfClause(A %among% a3)
  )

  # More clauses
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [three-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [three-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Three units: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Units created during pairwise that conflict with existing units ===
cat("\n=== Pairwise-created units conflicting ===\n")
set.seed(236003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Initial unit for V1
  v1_range = sample(dom, sample(1:2, 1))
  clauses = list(as.CnfClause(syms[["V1"]] %among% v1_range))

  # 2-symbol clauses involving V1 and V2 that could create a unit for V2 via SSE
  # If V2 becomes a unit, it might conflict with another V2 unit
  v1_vals = sample(dom, 2)
  v2_small = sample(dom, 1)
  v2_larger = unique(c(v2_small, sample(dom, 1)))

  clauses[[length(clauses) + 1]] = as.CnfClause(syms[["V1"]] %among% v1_vals | syms[["V2"]] %among% v2_small)
  clauses[[length(clauses) + 1]] = as.CnfClause(syms[["V1"]] %among% v1_vals | syms[["V2"]] %among% v2_larger)

  # Another unit for V2 (might conflict with SSE-created unit)
  v2_other = sample(dom, sample(1:2, 1))
  clauses[[length(clauses) + 1]] = as.CnfClause(syms[["V2"]] %among% v2_other)

  # More random clauses
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [pwconflict-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [pwconflict-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Pairwise-created conflicts: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Multiple symbols with unit conflicts ===
cat("\n=== Multi-symbol unit conflicts ===\n")
set.seed(236004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  clauses = list()
  # Multiple pairs of conflicting/overlapping units
  n_unit_pairs = sample(2:3, 1)
  for (k in 1:n_unit_pairs) {
    s = sample(sym_names, 1)
    r1 = sample(dom, sample(1:3, 1))
    r2 = sample(dom, sample(1:3, 1))
    # Ensure some overlap
    if (length(intersect(r1, r2)) == 0) {
      shared = sample(dom, 1)
      r1 = unique(c(r1, shared))
      r2 = unique(c(r2, shared))
    }
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% r1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% r2)
  }

  # Random clauses
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multiconf-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multiconf-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multi-symbol conflicts: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
