#!/usr/bin/env Rscript
# Tests the use_inso optimization in register_unit:
# When a unit is created during the pairwise phase (not initially),
# the code can use the is_not_subset_of matrix to skip unnecessary
# apply_domain_restriction calls. This tests that the optimization
# correctly skips only non-necessary calls and doesn't miss any.
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

# === Pattern 1: SSE creates units from 2-symbol clauses during pairwise ===
cat("=== SSE creates units during pairwise ===\n")
set.seed(220001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  E = CnfSymbol(u, "E", dom)
  syms = list(A = A, B = B, C = C, D = D, E = E)

  # Pairs of 2-symbol clauses that will trigger SSE, turning one into a unit
  a_vals = sample(dom, 2)
  b_vals1 = sample(dom, sample(1:2, 1))
  b_vals2 = unique(c(b_vals1, sample(dom, 1)))

  clauses = list(
    as.CnfClause(A %among% a_vals | B %among% b_vals1),
    as.CnfClause(A %among% a_vals | B %among% b_vals2)
  )

  # More clauses that will be affected by the unit creation
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
    cat(sprintf("ERROR [sseunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sseunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE unit during pairwise: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Multiple units created during pairwise, overlapping symbols ===
cat("\n=== Multiple pairwise units overlapping ===\n")
set.seed(220002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:6)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create several pairs that will generate units during pairwise
  clauses = list()
  for (j in 1:sample(3:5, 1)) {
    s1 = sample(sym_names, 1)
    s2 = sample(setdiff(sym_names, s1), 1)
    v1 = sample(dom, sample(1:2, 1))
    v2a = sample(dom, 1)
    v2b = unique(c(v2a, sample(dom, 1)))
    cl1 = as.CnfClause(syms[[s1]] %among% v1 | syms[[s2]] %among% v2a)
    cl2 = as.CnfClause(syms[[s1]] %among% v1 | syms[[s2]] %among% v2b)
    if (!isTRUE(unclass(cl1))) clauses[[length(clauses) + 1]] = cl1
    if (!isTRUE(unclass(cl2))) clauses[[length(clauses) + 1]] = cl2
  }

  # Additional random clauses
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 6) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multiunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multiunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multiple pairwise units: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Unit created early in pairwise, affects many later clauses ===
cat("\n=== Early unit, late effects ===\n")
set.seed(220003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # The first pair of clauses (shortest, will be processed first) creates a unit
  v1 = sample(dom, 2)
  v2a = sample(dom, 1)
  v2b = sample(dom, 2)  # Note: different sizes to ensure SSE potential
  if (!v2a %in% v2b) v2b = c(v2a, sample(setdiff(dom, v2a), 1))

  clauses = list(
    as.CnfClause(syms[[sym_names[1]]] %among% v1 | syms[[sym_names[2]]] %among% v2a),
    as.CnfClause(syms[[sym_names[1]]] %among% v1 | syms[[sym_names[2]]] %among% v2b)
  )

  # Many longer clauses that reference sym_names[2] (the unit's symbol after SSE)
  for (j in 1:sample(5:10, 1)) {
    n_sym = sample(3:5, 1)
    chosen = sample(sym_names, n_sym)
    # Ensure sym_names[2] is included often
    if (!sym_names[2] %in% chosen && runif(1) < 0.7) {
      chosen[sample(length(chosen), 1)] = sym_names[2]
    }
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
    cat(sprintf("ERROR [early-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [early-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Early unit late effects: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Contradiction through cascading pairwise units ===
cat("\n=== Pairwise unit contradiction ===\n")
set.seed(220004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create formulas that might simplify to FALSE through cascading
  clauses = list()
  for (j in 1:sample(6:12, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [contra-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [contra-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Pairwise contradiction: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
