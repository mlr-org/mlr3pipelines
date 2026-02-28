#!/usr/bin/env Rscript
# Test operator dispatch edge cases:
# - atom & formula, formula & atom
# - atom | clause, clause | atom
# - mixed TRUE/FALSE in complex expressions
# - chooseOpsMethod dispatch with all combinations
# - Operations producing tautologies or contradictions
# - Very complex expression trees
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Cross-type binary ops ===
cat("=== Cross-type binary ops ===\n")
set.seed(123001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  a1 = A %among% sample(dom, sample(1:2, 1))
  b1 = B %among% sample(dom, sample(1:2, 1))
  c1 = C %among% sample(dom, sample(1:2, 1))

  # atom | atom -> clause
  n_tests = n_tests + 1
  cl = tryCatch(a1 | b1, error = function(e) e)
  if (inherits(cl, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [a|a-%d]: %s\n", trial, cl$message)); next
  }
  if (!inherits(cl, "CnfClause")) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [a|a-%d]: expected CnfClause, got %s\n", trial, class(cl)))
  }

  # clause & atom -> formula
  n_tests = n_tests + 1
  f1 = tryCatch(cl & c1, error = function(e) e)
  if (inherits(f1, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cl&a-%d]: %s\n", trial, f1$message)); next
  }
  if (!inherits(f1, "CnfFormula")) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cl&a-%d]: expected CnfFormula, got %s\n", trial, class(f1)))
  }

  # atom & clause -> formula
  n_tests = n_tests + 1
  f2 = tryCatch(c1 & cl, error = function(e) e)
  if (inherits(f2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [a&cl-%d]: %s\n", trial, f2$message)); next
  }

  # f1 and f2 should be semantically identical
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sym-%d]: cl&a != a&cl\n", trial))
  }

  # formula | atom -> formula
  n_tests = n_tests + 1
  f3 = tryCatch(f1 | a1, error = function(e) e)
  if (inherits(f3, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [f|a-%d]: %s\n", trial, f3$message)); next
  }

  # atom | formula -> formula
  n_tests = n_tests + 1
  f4 = tryCatch(a1 | f1, error = function(e) e)
  if (inherits(f4, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [a|f-%d]: %s\n", trial, f4$message)); next
  }

  # f3 and f4 should be semantically identical
  t3 = evaluate_formula(f3, u)
  t4 = evaluate_formula(f4, u)
  if (!all(t3 == t4)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [or-sym-%d]: f|a != a|f\n", trial))
  }

  # formula & formula -> formula
  n_tests = n_tests + 1
  f5 = tryCatch(f1 & f2, error = function(e) e)
  if (inherits(f5, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [f&f-%d]: %s\n", trial, f5$message)); next
  }

  # formula | formula -> formula
  n_tests = n_tests + 1
  f6 = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(f6, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [f|f-%d]: %s\n", trial, f6$message)); next
  }
}
cat(sprintf("  Cross-type ops: %d tests, %d failures\n", n_tests, n_failures))

# === Complex expression trees with all types ===
cat("\n=== Complex expression trees ===\n")
set.seed(123002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  a_vals = sample(dom, sample(1:2, 1))
  b_vals = sample(dom, sample(1:2, 1))
  c_vals = sample(dom, sample(1:2, 1))

  # Build a complex expression tree and verify semantics
  # (A %among% a) & ((B %among% b) | (C %among% c))
  n_tests = n_tests + 1
  expr = tryCatch({
    part1 = A %among% a_vals
    part2 = B %among% b_vals | C %among% c_vals
    part1 & part2
  }, error = function(e) e)
  if (inherits(expr, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [tree-%d]: %s\n", trial, expr$message)); next
  }

  # Evaluate manually
  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  expected = (assignments[["A"]] %in% a_vals) &
    ((assignments[["B"]] %in% b_vals) | (assignments[["C"]] %in% c_vals))
  truth = evaluate_formula(expr, u)
  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [tree-%d]: semantic error\n", trial))
  }

  # More complex: !((A %among% a) | (B %among% b)) & (C %among% c)
  n_tests = n_tests + 1
  expr2 = tryCatch({
    part = as.CnfClause(A %among% a_vals | B %among% b_vals)
    neg_part = !CnfFormula(list(part))
    neg_part & (C %among% c_vals)
  }, error = function(e) e)
  if (inherits(expr2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [tree2-%d]: %s\n", trial, expr2$message)); next
  }
  expected2 = !((assignments[["A"]] %in% a_vals) | (assignments[["B"]] %in% b_vals)) &
    (assignments[["C"]] %in% c_vals)
  truth2 = evaluate_formula(expr2, u)
  if (!all(truth2 == expected2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [tree2-%d]: semantic error\n", trial))
  }
}
cat(sprintf("  Expression trees: %d tests, %d failures\n", n_tests, n_failures))

# === Tautology/contradiction through operations ===
cat("\n=== Tautology/contradiction ops ===\n")
set.seed(123003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)

  vals = sample(dom, sample(1:3, 1))
  comp = setdiff(dom, vals)

  # A in vals | A in comp = tautology
  n_tests = n_tests + 1
  taut_clause = tryCatch(as.CnfClause(A %among% vals | A %among% comp), error = function(e) e)
  if (inherits(taut_clause, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [taut-%d]: %s\n", trial, taut_clause$message)); next
  }
  if (!isTRUE(unclass(taut_clause))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [taut-%d]: expected TRUE clause\n", trial))
  }

  # (A in vals) & (A in comp) = contradiction (if vals and comp are disjoint)
  n_tests = n_tests + 1
  contr = tryCatch({
    f = CnfFormula(list(
      as.CnfClause(A %among% vals),
      as.CnfClause(A %among% comp)
    ))
    f
  }, error = function(e) e)
  if (inherits(contr, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [contr-%d]: %s\n", trial, contr$message)); next
  }
  truth = evaluate_formula(contr, u)
  if (any(truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [contr-%d]: expected FALSE formula\n", trial))
  }

  # f & !f = FALSE
  n_tests = n_tests + 1
  f = CnfFormula(list(as.CnfClause(A %among% vals)))
  f_and_not_f = tryCatch(f & (!f), error = function(e) e)
  if (inherits(f_and_not_f, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [comp-%d]: %s\n", trial, f_and_not_f$message)); next
  }
  truth_comp = evaluate_formula(f_and_not_f, u)
  if (any(truth_comp)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [comp-%d]: f & !f not FALSE\n", trial))
  }

  # f | !f = TRUE
  n_tests = n_tests + 1
  f_or_not_f = tryCatch(f | (!f), error = function(e) e)
  if (inherits(f_or_not_f, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [comp-or-%d]: %s\n", trial, f_or_not_f$message)); next
  }
  truth_comp_or = evaluate_formula(f_or_not_f, u)
  if (!all(truth_comp_or)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [comp-or-%d]: f | !f not TRUE\n", trial))
  }
}
cat(sprintf("  Taut/contr: %d tests, %d failures\n", n_tests, n_failures))

# === Negation of multi-clause formulas ===
cat("\n=== Multi-clause negation ===\n")
set.seed(123004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("x", "y", "z")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  a1 = sample(dom, sample(1:2, 1))
  a2 = sample(dom, sample(1:2, 1))
  b1 = sample(dom, sample(1:2, 1))
  b2 = sample(dom, sample(1:2, 1))

  # f = (A in a1 | B in b1) & (A in a2 | B in b2)
  # !f = !(A in a1 | B in b1) | !(A in a2 | B in b2)
  #    = (A not in a1 & B not in b1) | (A not in a2 & B not in b2)
  n_tests = n_tests + 1
  f = tryCatch(CnfFormula(list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a2 | B %among% b2)
  )), error = function(e) e)
  if (inherits(f, "error")) { next }

  neg_f = tryCatch(!f, error = function(e) e)
  if (inherits(neg_f, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neg-%d]: %s\n", trial, neg_f$message)); next
  }

  # Check: f & !f should be FALSE
  f_and_neg = tryCatch(f & neg_f, error = function(e) e)
  if (!inherits(f_and_neg, "error")) {
    truth = evaluate_formula(f_and_neg, u)
    if (any(truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [neg-contr-%d]: f & !f not FALSE\n", trial))
    }
  }

  # Check: f | !f should be TRUE
  n_tests = n_tests + 1
  f_or_neg = tryCatch(f | neg_f, error = function(e) e)
  if (!inherits(f_or_neg, "error")) {
    truth = evaluate_formula(f_or_neg, u)
    if (!all(truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [neg-taut-%d]: f | !f not TRUE\n", trial))
    }
  }
}
cat(sprintf("  Multi-clause neg: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
