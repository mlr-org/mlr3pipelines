#!/usr/bin/env Rscript
# HLA unit phase with roe_inverse edge cases:
# - non-unit HLA eliminates clauses, then unit HLA uses stale roe_inverse
# - unit HLA with symbol_registry pointing to eliminated entries
# - Multiple units competing for HLA
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

# === Pattern 1: Non-unit HLA eliminates, then unit HLA runs ===
cat("=== Non-unit HLA then unit HLA ===\n")
set.seed(203001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Unit for A
  a_val = sample(dom, 1)
  a_comp = setdiff(dom, a_val)

  # Non-unit clause that might be eliminated by HLA
  # Make clauses where HLA could trigger hidden tautology
  b_range1 = sample(dom, 2)
  b_comp1 = setdiff(dom, b_range1)
  c_range1 = sample(dom, sample(1:2, 1))

  clauses = list(
    as.CnfClause(A %among% a_val),
    # Clause pair designed for HLA interaction
    as.CnfClause(B %among% b_range1 | C %among% c_range1),
    as.CnfClause(B %among% b_comp1 | C %among% c_range1)
  )

  # More clauses for interaction
  for (j in 1:sample(3:6, 1)) {
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
    cat(sprintf("ERROR [hlauroe-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hlauroe-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Non-unit then unit HLA: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Multiple units, each unit does HLA ===
cat("\n=== Multiple units HLA ===\n")
set.seed(203002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Two units
  a_val = sample(dom, sample(1:2, 1))
  b_val = sample(dom, sample(1:2, 1))

  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(B %among% b_val)
  )

  # Multi-symbol clauses involving A, B, C, D
  for (j in 1:sample(4:8, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
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
    cat(sprintf("ERROR [multiuhla-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multiuhla-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multiple units HLA: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Cascading HLA + SSE interaction ===
cat("\n=== Cascading HLA + SSE ===\n")
set.seed(203003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create clauses designed for SSE during pairwise phase,
  # followed by HLA during HLA phase
  a1 = sample(dom, sample(2:3, 1))
  a2 = sample(dom, sample(2:3, 1))
  b1 = sample(dom, sample(2:3, 1))
  b2 = sample(dom, sample(2:3, 1))
  c1 = sample(dom, sample(2:3, 1))
  c2 = sample(dom, sample(2:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a2 | B %among% b2),
    as.CnfClause(A %among% a1 | C %among% c1),
    as.CnfClause(A %among% a2 | C %among% c2),
    as.CnfClause(B %among% b1 | C %among% c1),
    as.CnfClause(B %among% b2 | C %among% c2)
  )

  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
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
    cat(sprintf("ERROR [hlasse-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hlasse-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Cascading HLA + SSE: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Unit HLA where unit symbol is absent from some clauses ===
cat("\n=== Unit symbol absent ===\n")
set.seed(203004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Unit for A
  a_val = sample(dom, sample(1:2, 1))
  clauses = list(as.CnfClause(A %among% a_val))

  # Clauses that DON'T have A (so unit HLA treats them differently)
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(c("B", "C", "D"), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # Some clauses WITH A
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    if (!"A" %in% chosen) chosen[1] = "A"
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
    cat(sprintf("ERROR [uabs-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [uabs-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Unit symbol absent: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
