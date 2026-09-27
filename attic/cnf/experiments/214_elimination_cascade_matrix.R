#!/usr/bin/env Rscript
# Elimination cascade and matrix update stress:
# Tests that the is_not_subset_of matrix stays consistent through
# cascading eliminations, unit creations, and symbol removals.
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

# === Pattern 1: Deep cascade through 6 symbols ===
cat("=== Deep cascade 6 symbols ===\n")
set.seed(214001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  E = CnfSymbol(u, "E", dom)
  F = CnfSymbol(u, "F", dom)
  syms = list(A = A, B = B, C = C, D = D, E = E, F = F)

  # Chain: A -> B -> C -> D -> E -> F
  vals = lapply(1:6, function(i) sample(dom, 1))
  comps = lapply(vals, function(v) setdiff(dom, v))

  clauses = list(
    as.CnfClause(A %among% vals[[1]]),
    as.CnfClause(A %among% comps[[1]] | B %among% vals[[2]]),
    as.CnfClause(B %among% comps[[2]] | C %among% vals[[3]]),
    as.CnfClause(C %among% comps[[3]] | D %among% vals[[4]]),
    as.CnfClause(D %among% comps[[4]] | E %among% vals[[5]]),
    as.CnfClause(E %among% comps[[5]] | F %among% vals[[6]])
  )

  # Add interaction clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
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
    cat(sprintf("ERROR [deep6-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [deep6-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Deep cascade 6: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Diamond-shaped cascade ===
cat("\n=== Diamond cascade ===\n")
set.seed(214002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Diamond: A -> B, A -> C, B -> D, C -> D
  a_val = sample(dom, 1); a_comp = setdiff(dom, a_val)
  b_val = sample(dom, 1); b_comp = setdiff(dom, b_val)
  c_val = sample(dom, 1); c_comp = setdiff(dom, c_val)
  d_val = sample(dom, sample(1:2, 1))

  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(A %among% a_comp | B %among% b_val),
    as.CnfClause(A %among% a_comp | C %among% c_val),
    as.CnfClause(B %among% b_comp | D %among% d_val),
    as.CnfClause(C %among% c_comp | D %among% d_val)
  )

  for (j in 1:sample(1:3, 1)) {
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
    cat(sprintf("ERROR [diamond-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [diamond-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Diamond cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Cross-referencing clauses ===
cat("\n=== Cross-referencing clauses ===\n")
set.seed(214003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Many clauses referencing same symbols in different combos
  sym_pairs = list(c("A","B"), c("A","C"), c("A","D"), c("B","C"), c("B","D"), c("C","D"))
  n_cl = sample(6:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    pair = sym_pairs[[sample(length(sym_pairs), 1)]]
    atoms = lapply(pair, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [xref-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [xref-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Cross-referencing: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Rapid unit creation/elimination ===
cat("\n=== Rapid unit creation ===\n")
set.seed(214004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Multiple units for different symbols
  a_val = sample(dom, 1)
  b_val = sample(dom, 1)

  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(B %among% b_val)
  )

  # Clauses that become units after propagation
  a_comp = setdiff(dom, a_val)
  b_comp = setdiff(dom, b_val)
  c_val = sample(dom, 1)
  d_val = sample(dom, 1)

  clauses[[length(clauses) + 1]] = as.CnfClause(A %among% a_comp | C %among% c_val)
  clauses[[length(clauses) + 1]] = as.CnfClause(B %among% b_comp | D %among% d_val)

  # Cross-interaction clauses
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [rapid-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [rapid-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Rapid unit creation: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
