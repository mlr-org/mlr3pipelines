#!/usr/bin/env Rscript
# Register unit with use_inso optimization:
# When a new unit is created during SSE (inside the pairwise loop),
# the is_not_subset_of matrix can be used to skip some propagation.
# This test creates formulas designed to trigger this optimization path.
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

# === Pattern 1: Unit + many clauses where unit covers most ===
cat("=== Unit covers most clause ranges ===\n")
set.seed(200001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Unit for A with wide range
  a_unit = sample(dom, sample(3:4, 1))
  clauses = list(as.CnfClause(A %among% a_unit))

  # Clauses where A's range is mostly within the unit
  for (j in 1:sample(5:8, 1)) {
    a_range = sample(a_unit, sample(1:length(a_unit), 1))
    b_range = sample(dom, sample(1:4, 1))
    c_range = sample(dom, sample(1:4, 1))
    n_sym = sample(2:3, 1)
    if (n_sym == 2) {
      cl = as.CnfClause(A %among% a_range | B %among% b_range)
    } else {
      cl = as.CnfClause(A %among% a_range | B %among% b_range | C %among% c_range)
    }
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  # Some clauses where A's range extends beyond unit (will be restricted)
  for (j in 1:sample(1:3, 1)) {
    a_range = sample(dom, sample(2:4, 1))
    b_range = sample(dom, sample(1:3, 1))
    cl = as.CnfClause(A %among% a_range | B %among% b_range)
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ucover-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ucover-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Unit covers most: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: SSE creates unit for symbol that already has unit ===
cat("\n=== SSE creates duplicate unit ===\n")
set.seed(200002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Have a unit for A
  a_unit = sample(dom, sample(2:3, 1))
  clauses = list(as.CnfClause(A %among% a_unit))

  # Create two clauses where SSE can reduce one to a unit for B
  # e.g., (B in {a,b} | C in {c}) and (B in {a,b} | C in {c,d})
  # SSE: the second clause's C range gets restricted to {c}, then C is eliminated -> unit for B
  b_range = sample(dom, sample(1:2, 1))
  c_range1 = sample(dom, sample(1:2, 1))
  c_extra = sample(setdiff(dom, c_range1), min(2, length(setdiff(dom, c_range1))))
  c_range2 = c(c_range1, c_extra)

  cl1 = as.CnfClause(B %among% b_range | C %among% c_range1)
  cl2 = as.CnfClause(B %among% b_range | C %among% c_range2)
  if (!isTRUE(unclass(cl1))) clauses[[length(clauses) + 1]] = cl1
  if (!isTRUE(unclass(cl2))) clauses[[length(clauses) + 1]] = cl2

  # Add more clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ssedup-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ssedup-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE duplicate unit: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Multiple rounds of unit creation ===
cat("\n=== Multiple unit creation rounds ===\n")
set.seed(200003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Create a formula where unit propagation triggers SSE, which creates new units,
  # which trigger more propagation
  a_val = sample(dom, 1)
  b_val = sample(dom, 1)
  c_val = sample(dom, 1)

  a_comp = setdiff(dom, a_val)
  b_comp = setdiff(dom, b_val)

  clauses = list(
    # Unit for A
    as.CnfClause(A %among% a_val),
    # After A-propagation: becomes unit for B
    as.CnfClause(A %among% a_comp | B %among% b_val),
    # After B-propagation: becomes unit for C
    as.CnfClause(B %among% b_comp | C %among% c_val)
  )

  # More clauses that interact
  for (j in 1:sample(3:5, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multi-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multi-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multiple rounds: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Unit with tight domain forcing many eliminations ===
cat("\n=== Tight unit many eliminations ===\n")
set.seed(200004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Very tight unit (1 value)
  a_val = sample(dom, 1)
  clauses = list(as.CnfClause(A %among% a_val))

  # Many clauses with A ranges that include the unit value -> most get subsumed
  for (j in 1:sample(8:12, 1)) {
    # Include a_val in the range most of the time
    if (sample(c(TRUE, FALSE), 1, prob = c(0.7, 0.3))) {
      a_range = unique(c(a_val, sample(dom, sample(1:3, 1))))
    } else {
      a_range = sample(dom, sample(1:3, 1))
    }
    b_range = sample(dom, sample(1:4, 1))
    n_sym = sample(2:3, 1)
    if (n_sym == 2) {
      cl = as.CnfClause(A %among% a_range | B %among% b_range)
    } else {
      c_range = sample(dom, sample(1:4, 1))
      cl = as.CnfClause(A %among% a_range | B %among% b_range | C %among% c_range)
    }
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 6) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [tight-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [tight-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Tight unit: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
