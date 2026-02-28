#!/usr/bin/env Rscript
# Test cross-type operations thoroughly:
# - atom | clause (dispatch to |.CnfAtom with CnfClause e2)
# - clause | atom
# - atom & clause, clause & atom
# - atom | formula, formula | atom
# - clause & formula, formula & clause
# - Chained: atom | atom | atom (left-to-right: (atom | atom) | atom = clause | atom)
# - Mixed chains: atom | clause | atom
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === atom | atom | atom (creates clause | atom internally) ===
cat("=== Three-way atom OR ===\n")
set.seed(112001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  a_vals = sample(dom, sample(1:3, 1))
  b_vals = sample(dom, sample(1:3, 1))
  c_vals = sample(dom, sample(1:3, 1))

  atom_a = A %among% a_vals
  atom_b = B %among% b_vals
  atom_c = C %among% c_vals

  n_tests = n_tests + 1
  # This evaluates as (atom_a | atom_b) | atom_c
  result = tryCatch(atom_a | atom_b | atom_c, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [3atom-%d]: %s\n", trial, result$message)); next
  }

  # Should be a CnfClause
  if (!inherits(result, "CnfClause")) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3atom-%d]: result not CnfClause, is %s\n", trial, class(result)[1]))
    next
  }

  # Evaluate
  truth = evaluate_clause(result, u)

  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  expected = (assignments[["A"]] %in% a_vals) | (assignments[["B"]] %in% b_vals) | (assignments[["C"]] %in% c_vals)

  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3atom-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Three-way atom OR: %d tests, %d failures\n", n_tests, n_failures))

# === clause | atom (explicit) ===
cat("\n=== clause | atom ===\n")
set.seed(112002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  clause = as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | B %among% sample(dom, sample(1:2, 1)))
  if (isTRUE(unclass(clause))) next
  atom = C %among% sample(dom, sample(1:2, 1))

  n_tests = n_tests + 1
  result = tryCatch(clause | atom, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cl-atom-%d]: %s\n", trial, result$message)); next
  }

  truth = evaluate_clause(result, u)

  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  cl_bare = unclass(clause)
  cl_truth = rep(FALSE, nrow(assignments))
  for (s in names(cl_bare)) cl_truth = cl_truth | (assignments[[s]] %in% cl_bare[[s]])
  a_truth = assignments[["C"]] %in% atom$values
  expected = cl_truth | a_truth

  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cl-atom-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  clause | atom: %d tests, %d failures\n", n_tests, n_failures))

# === atom & clause (should create a CnfFormula) ===
cat("\n=== atom & clause ===\n")
set.seed(112003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("x", "y", "z")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  atom = A %among% sample(dom, sample(1:2, 1))
  clause = as.CnfClause(B %among% sample(dom, sample(1:2, 1)) | C %among% sample(dom, sample(1:2, 1)))
  if (isTRUE(unclass(clause))) next

  n_tests = n_tests + 1
  result = tryCatch(atom & clause, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [atom-and-cl-%d]: %s\n", trial, result$message)); next
  }

  truth = evaluate_formula(result, u)

  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  a_truth = assignments[["A"]] %in% atom$values
  cl_bare = unclass(clause)
  cl_truth = rep(FALSE, nrow(assignments))
  for (s in names(cl_bare)) cl_truth = cl_truth | (assignments[[s]] %in% cl_bare[[s]])
  expected = a_truth & cl_truth

  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [atom-and-cl-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  atom & clause: %d tests, %d failures\n", n_tests, n_failures))

# === formula | atom (should work via |.CnfFormula) ===
cat("\n=== formula | atom ===\n")
set.seed(112004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  f = tryCatch(CnfFormula(list(
    as.CnfClause(A %among% sample(dom, sample(1:2, 1))),
    as.CnfClause(B %among% sample(dom, sample(1:2, 1)))
  )), error = function(e) NULL)
  if (is.null(f)) next

  atom = A %among% sample(dom, sample(1:2, 1))

  n_tests = n_tests + 1
  result = tryCatch(f | atom, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [f-or-atom-%d]: %s\n", trial, result$message)); next
  }

  t_f = evaluate_formula(f, u)

  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  a_truth = assignments[["A"]] %in% atom$values
  expected = t_f | a_truth

  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [f-or-atom-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  formula | atom: %d tests, %d failures\n", n_tests, n_failures))

# === Chained: atom | clause | atom (four atoms total) ===
cat("\n=== Long chain atom | clause | atom ===\n")
set.seed(112005)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  a1 = A %among% sample(dom, sample(1:2, 1))
  b1 = B %among% sample(dom, sample(1:2, 1))
  c1 = C %among% sample(dom, sample(1:2, 1))
  a2 = A %among% sample(dom, sample(1:2, 1))

  n_tests = n_tests + 1
  # (a1 | b1) is clause, then clause | c1 is clause, then clause | a2 is clause
  result = tryCatch(a1 | b1 | c1 | a2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d]: %s\n", trial, result$message)); next
  }

  if (!inherits(result, "CnfClause")) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d]: result not CnfClause\n", trial)); next
  }

  truth = evaluate_clause(result, u)
  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  expected = (assignments[["A"]] %in% a1$values) | (assignments[["B"]] %in% b1$values) |
             (assignments[["C"]] %in% c1$values) | (assignments[["A"]] %in% a2$values)

  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Long chain: %d tests, %d failures\n", n_tests, n_failures))

# === Negation of atoms/clauses used in formulas ===
cat("\n=== Negated atoms in formulas ===\n")
set.seed(112006)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  atom = A %among% sample(dom, sample(1:2, 1))
  neg_atom = !atom
  clause = as.CnfClause(B %among% sample(dom, sample(1:2, 1)))
  if (isTRUE(unclass(clause))) next

  # !atom & clause should be a formula
  n_tests = n_tests + 1
  result = tryCatch(neg_atom & clause, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [negform-%d]: %s\n", trial, result$message)); next
  }

  truth = evaluate_formula(result, u)
  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  neg_truth = assignments[["A"]] %in% neg_atom$values
  cl_bare = unclass(clause)
  cl_truth = assignments[["B"]] %in% cl_bare[["B"]]
  expected = neg_truth & cl_truth

  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [negform-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Negated atoms: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
