#!/usr/bin/env Rscript
# CnfClause constructor edge cases:
# - Clause from multiple CnfClause objects (merging)
# - Clause with atoms that overlap in symbol
# - Empty atom lists
# - Mixing atoms and clauses in constructor
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: Clause merging ===
cat("=== Clause merging ===\n")
set.seed(191001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create two clauses and merge them
  cl1_syms = sample(names(syms), sample(1:3, 1))
  cl1_atoms = lapply(cl1_syms, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  cl1 = as.CnfClause(Reduce(`|`, cl1_atoms))

  cl2_syms = sample(names(syms), sample(1:3, 1))
  cl2_atoms = lapply(cl2_syms, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  cl2 = as.CnfClause(Reduce(`|`, cl2_atoms))

  if (isTRUE(unclass(cl1)) || isTRUE(unclass(cl2))) next

  # Merge via CnfClause constructor
  n_tests = n_tests + 1
  merged = tryCatch(CnfClause(list(cl1, cl2)), error = function(e) e)
  if (inherits(merged, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [merge-%d]: %s\n", trial, merged$message)); next
  }

  # Merge via | operator
  ored = tryCatch(cl1 | cl2, error = function(e) e)
  if (inherits(ored, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [or-%d]: %s\n", trial, ored$message)); next
  }

  # Both should be semantically equivalent (as single clauses)
  f_merged = as.CnfFormula(merged)
  f_ored = as.CnfFormula(ored)
  t_merged = evaluate_formula(f_merged, u)
  t_ored = evaluate_formula(f_ored, u)
  if (!all(t_merged == t_ored)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mergeVsOr-%d]: CnfClause(list(cl1,cl2)) != cl1|cl2\n", trial))
  }
}
cat(sprintf("  Clause merging: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Overlapping atoms in constructor ===
cat("\n=== Overlapping atoms ===\n")
set.seed(191002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  # Multiple atoms for same symbol
  a1 = A %among% sample(dom, sample(1:3, 1))
  a2 = A %among% sample(dom, sample(1:3, 1))
  b1 = B %among% sample(dom, sample(1:4, 1))

  n_tests = n_tests + 1
  cl = tryCatch(CnfClause(list(a1, a2, b1)), error = function(e) e)
  if (inherits(cl, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [overlap-%d]: %s\n", trial, cl$message)); next
  }
  if (isTRUE(unclass(cl))) next

  # Compare: (A in a1_vals | A in a2_vals | B in b1_vals) should equal
  # (A in union(a1_vals, a2_vals) | B in b1_vals)
  f = as.CnfFormula(cl)
  tf = evaluate_formula(f, u)

  # Manual: for each assignment, check if A in union or B in b1_vals
  varnames = ls(u)
  domains2 = lapply(varnames, function(v) get(v, u))
  names(domains2) = varnames
  assignments = expand.grid(domains2, stringsAsFactors = FALSE)
  a_vals = unique(c(unclass(a1)$values, unclass(a2)$values))
  b_vals = unclass(b1)$values
  expected = (assignments[["A"]] %in% a_vals) | (assignments[["B"]] %in% b_vals)

  if (!all(tf == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [overlap-%d]: overlapping atoms mismatch\n", trial))
  }
}
cat(sprintf("  Overlapping atoms: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Clauses with FALSE atoms ===
cat("\n=== FALSE atoms in clause ===\n")
set.seed(191003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  # FALSE atom = X %among% character(0)
  false_atom = A %among% character(0)
  real_atom = B %among% sample(dom, sample(1:2, 1))

  n_tests = n_tests + 1
  cl = CnfClause(list(false_atom, real_atom))
  # FALSE atom should be ignored
  if (isTRUE(unclass(cl))) next

  f = as.CnfFormula(cl)
  tf = evaluate_formula(f, u)

  # Should be equivalent to just B %among% vals
  varnames = ls(u)
  domains2 = lapply(varnames, function(v) get(v, u))
  names(domains2) = varnames
  assignments = expand.grid(domains2, stringsAsFactors = FALSE)
  expected = assignments[["B"]] %in% unclass(real_atom)$values

  if (!all(tf == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [false-%d]: FALSE atom not ignored\n", trial))
  }
}
cat(sprintf("  FALSE atoms: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Mixing atoms and clauses ===
cat("\n=== Mixed atoms and clauses ===\n")
set.seed(191004)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create clause from mix of atoms and clauses
  atom1 = syms[[sample(names(syms), 1)]] %among% sample(dom, sample(1:3, 1))
  atom2 = syms[[sample(names(syms), 1)]] %among% sample(dom, sample(1:3, 1))

  sub_syms = sample(names(syms), 2)
  sub_atoms = lapply(sub_syms, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  sub_clause = as.CnfClause(Reduce(`|`, sub_atoms))

  n_tests = n_tests + 1
  if (isTRUE(unclass(sub_clause))) next
  cl = tryCatch(CnfClause(list(atom1, sub_clause, atom2)), error = function(e) e)
  if (inherits(cl, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mix-%d]: %s\n", trial, cl$message)); next
  }
  if (isTRUE(unclass(cl))) next

  # Compare with manually computing the OR
  f = as.CnfFormula(cl)
  tf = evaluate_formula(f, u)

  varnames = ls(u)
  domains2 = lapply(varnames, function(v) get(v, u))
  names(domains2) = varnames
  assignments = expand.grid(domains2, stringsAsFactors = FALSE)

  # All values for each symbol in the combined clause
  expected = rep(FALSE, nrow(assignments))
  cl_bare = unclass(cl)
  for (sym in names(cl_bare)) {
    expected = expected | (assignments[[sym]] %in% cl_bare[[sym]])
  }

  if (!all(tf == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mix-%d]: mixed atom/clause mismatch\n", trial))
  }
}
cat(sprintf("  Mixed atoms/clauses: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
