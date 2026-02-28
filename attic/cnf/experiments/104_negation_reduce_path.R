#!/usr/bin/env Rscript
# Test !.CnfFormula with multi-clause formulas:
# - The Reduce(function(x,y) x | y, negated_formulae) path
# - Negation of formulas with many clauses (creates OR of AND expressions)
# - Negation followed by further operations
# - Negation of results from negation (double negation of complex formulas)
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Negation of multi-clause formulas ===
cat("=== Negation of multi-clause formulas ===\n")
set.seed(104001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom_size = sample(2:4, 1)
  d = paste0("v", 1:dom_size)
  n_vars = sample(2:4, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  # Create a multi-clause formula (2-4 clauses)
  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:min(2, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(d, sample(1:(dom_size - 1), 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  neg_f = tryCatch(!f, error = function(e) e)
  if (inherits(neg_f, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neg-%d]: %s\n", trial, neg_f$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_neg = evaluate_formula(neg_f, u)
  if (!all(t_neg == !t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [neg-%d]: !f semantics wrong\n", trial))
  }
}
cat(sprintf("  Multi-clause negation: %d tests, %d failures\n", n_tests, n_failures))

# === Double negation of complex formulas ===
cat("\n=== Double negation ===\n")
set.seed(104002)

for (trial in 1:300) {
  u = CnfUniverse()
  d = c("a", "b", "c")
  A = CnfSymbol(u, "A", d)
  B = CnfSymbol(u, "B", d)
  C = CnfSymbol(u, "C", d)

  n_cl = sample(1:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(c("A", "B", "C"), n_sym)
    atoms = lapply(chosen, function(s) {
      sym = switch(s, A = A, B = B, C = C)
      sym %among% sample(d, sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  dbl_neg = tryCatch(!!f, error = function(e) e)
  if (inherits(dbl_neg, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dblneg-%d]: %s\n", trial, dbl_neg$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_dbl = evaluate_formula(dbl_neg, u)
  if (!all(t_dbl == t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dblneg-%d]: !!f != f\n", trial))
  }
}
cat(sprintf("  Double negation: %d tests, %d failures\n", n_tests, n_failures))

# === Negation then AND: !(f1) & !(f2) should equal !(f1 | f2) ===
cat("\n=== De Morgan with complex formulas ===\n")
set.seed(104003)

for (trial in 1:300) {
  u = CnfUniverse()
  d = c("0", "1", "2")
  n_vars = sample(2:3, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  make_formula = function() {
    n_cl = sample(1:2, 1)
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:min(2, n_vars), 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(d, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula(); f2 = make_formula()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  # De Morgan: !(f1 | f2) == !f1 & !f2
  lhs = tryCatch(!(f1 | f2), error = function(e) NULL)
  rhs = tryCatch((!f1) & (!f2), error = function(e) NULL)
  if (is.null(lhs) || is.null(rhs)) next

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan-%d]: !(f1|f2) != !f1 & !f2\n", trial))
  }
}
cat(sprintf("  De Morgan: %d tests, %d failures\n", n_tests, n_failures))

# === Negation creates large intermediate formula via Reduce ===
cat("\n=== Large Reduce path ===\n")
set.seed(104004)

for (trial in 1:200) {
  u = CnfUniverse()
  d = c("0", "1")
  n_vars = sample(3:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("B", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  # Create formula with many clauses -> negation Reduce path
  n_cl = sample(3:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:min(3, n_vars), 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(d, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  neg_f = tryCatch(!f, error = function(e) e)
  if (inherits(neg_f, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [reduce-%d]: %s\n", trial, neg_f$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_neg = evaluate_formula(neg_f, u)
  if (!all(t_neg == !t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [reduce-%d]: negation via Reduce wrong\n", trial))
  }
}
cat(sprintf("  Large Reduce: %d tests, %d failures\n", n_tests, n_failures))

# === Negation of single-clause formula (no Reduce needed) vs multi ===
cat("\n=== Single vs multi clause negation consistency ===\n")
set.seed(104005)

for (trial in 1:200) {
  u = CnfUniverse()
  d = c("a", "b", "c")
  A = CnfSymbol(u, "A", d)
  B = CnfSymbol(u, "B", d)

  # Single clause formula
  a_vals = sample(d, sample(1:2, 1))
  b_vals = sample(d, sample(1:2, 1))
  clause = as.CnfClause(A %among% a_vals | B %among% b_vals)
  if (isTRUE(unclass(clause))) next

  f = CnfFormula(list(clause))

  n_tests = n_tests + 1
  neg = tryCatch(!f, error = function(e) e)
  if (inherits(neg, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [single-%d]: %s\n", trial, neg$message)); next
  }

  # Verify: negation of (A in a_vals OR B in b_vals) = (A not in a_vals) AND (B not in b_vals)
  t_f = evaluate_formula(f, u)
  t_neg = evaluate_formula(neg, u)
  if (!all(t_neg == !t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [single-%d]: single clause negation wrong\n", trial))
  }
}
cat(sprintf("  Single clause negation: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
