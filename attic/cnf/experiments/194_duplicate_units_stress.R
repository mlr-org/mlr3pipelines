#!/usr/bin/env Rscript
# Duplicate unit stress test:
# Formulas designed to create multiple units for the same symbol during SSE.
# This exercises the unit intersection path in register_unit (lines 96-104)
# and the propagation with potentially tighter domains.
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

# === Pattern 1: Two explicit units for same symbol ===
cat("=== Two units same symbol ===\n")
set.seed(194001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Two units for the same symbol with overlapping domains
  sym_name = sample(names(syms), 1)
  range1 = sample(dom, sample(1:3, 1))
  range2 = sample(dom, sample(1:3, 1))
  isct = intersect(range1, range2)

  unit1 = as.CnfClause(syms[[sym_name]] %among% range1)
  unit2 = as.CnfClause(syms[[sym_name]] %among% range2)

  # Add some multi-symbol clauses
  clauses = list(unit1, unit2)
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [2unit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [2unit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Two units same symbol: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Three+ units same symbol (cascading intersection) ===
cat("\n=== Three+ units same symbol ===\n")
set.seed(194002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  sym_name = sample(names(syms), 1)
  # Create 3-4 units for same symbol
  n_units = sample(3:4, 1)
  clauses = list()
  for (j in 1:n_units) {
    range_j = sample(dom, sample(2:4, 1))
    clauses[[j]] = as.CnfClause(syms[[sym_name]] %among% range_j)
  }

  # Add non-unit clauses
  for (j in 1:sample(3:5, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [3unit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3unit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Three+ units same symbol: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Units for ALL symbols ===
cat("\n=== Units for all symbols ===\n")
set.seed(194003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  clauses = list()
  # One unit per symbol
  for (s in names(syms)) {
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
  }
  # Maybe duplicate units
  if (sample(c(TRUE, FALSE), 1)) {
    dup_sym = sample(names(syms), 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[dup_sym]] %among% sample(dom, sample(1:3, 1)))
  }
  # Add multi-symbol clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [allunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [allunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Units for all symbols: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Contradicting units (empty intersection) ===
cat("\n=== Contradicting units ===\n")
set.seed(194004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  sym_name = sample(names(syms), 1)
  # Partition domain into disjoint parts
  perm = sample(dom)
  range1 = perm[1:2]
  range2 = perm[3:4]

  clauses = list(
    as.CnfClause(syms[[sym_name]] %among% range1),
    as.CnfClause(syms[[sym_name]] %among% range2)
  )
  # Add random clauses (shouldn't matter since formula is FALSE)
  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [contra-%d]: %s\n", trial, result$message)); next
  }
  # Should be FALSE
  if (!isFALSE(unclass(result))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [contra-%d]: should be FALSE\n", trial)); next
  }
}
cat(sprintf("  Contradicting units: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 5: SSE creating new unit (narrow clauses) ===
cat("\n=== SSE creating units ===\n")
set.seed(194005)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create clauses where SSE can turn multi-symbol clauses into units
  # E.g., (A in {a,b} | B in {c}) and (A in {a,b} | B in {c,d}) -> SSE can restrict second clause
  a_range = sample(dom, sample(1:2, 1))
  b_range1 = sample(dom, sample(1:2, 1))
  b_range2 = unique(c(b_range1, sample(dom, sample(0:2, 1))))

  clauses = list(
    as.CnfClause(A %among% a_range | B %among% b_range1)
  )
  if (!isTRUE(unclass(clauses[[1]]))) {
    cl2 = as.CnfClause(A %among% a_range | B %among% b_range2)
    if (!isTRUE(unclass(cl2))) clauses[[2]] = cl2
  }

  # Add more clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sseunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sseunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE creating units: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
