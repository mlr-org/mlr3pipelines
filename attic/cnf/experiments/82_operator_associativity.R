#!/usr/bin/env Rscript
# Test associativity and commutativity of &, |, ! more thoroughly.
# Also test that formula-level algebra works correctly.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Associativity and commutativity ===\n")
set.seed(82001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function() {
    n_cl = sample(1:3, 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula()
  f2 = make_formula()
  f3 = make_formula()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)

  # AND commutativity: f1 & f2 == f2 & f1
  r1 = tryCatch(f1 & f2, error = function(e) NULL)
  r2 = tryCatch(f2 & f1, error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    n_tests = n_tests + 1
    if (!all(evaluate_formula(r1, u) == evaluate_formula(r2, u))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [and-comm-%d]\n", trial))
    }
  }

  # AND associativity: (f1 & f2) & f3 == f1 & (f2 & f3)
  r1 = tryCatch((f1 & f2) & f3, error = function(e) NULL)
  r2 = tryCatch(f1 & (f2 & f3), error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    n_tests = n_tests + 1
    if (!all(evaluate_formula(r1, u) == evaluate_formula(r2, u))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [and-assoc-%d]\n", trial))
    }
  }

  # OR commutativity: f1 | f2 == f2 | f1
  r1 = tryCatch(f1 | f2, error = function(e) NULL)
  r2 = tryCatch(f2 | f1, error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    n_tests = n_tests + 1
    if (!all(evaluate_formula(r1, u) == evaluate_formula(r2, u))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [or-comm-%d]\n", trial))
    }
  }

  # OR associativity: (f1 | f2) | f3 == f1 | (f2 | f3)
  r1 = tryCatch((f1 | f2) | f3, error = function(e) NULL)
  r2 = tryCatch(f1 | (f2 | f3), error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    n_tests = n_tests + 1
    if (!all(evaluate_formula(r1, u) == evaluate_formula(r2, u))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [or-assoc-%d]\n", trial))
    }
  }

  # Distributivity: f1 & (f2 | f3) == (f1 & f2) | (f1 & f3)
  lhs = tryCatch(f1 & (f2 | f3), error = function(e) NULL)
  rhs = tryCatch((f1 & f2) | (f1 & f3), error = function(e) NULL)
  if (!is.null(lhs) && !is.null(rhs)) {
    n_tests = n_tests + 1
    if (!all(evaluate_formula(lhs, u) == evaluate_formula(rhs, u))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [distrib-and-%d]\n", trial))
    }
  }

  # Distributivity: f1 | (f2 & f3) == (f1 | f2) & (f1 | f3)
  lhs = tryCatch(f1 | (f2 & f3), error = function(e) NULL)
  rhs = tryCatch((f1 | f2) & (f1 | f3), error = function(e) NULL)
  if (!is.null(lhs) && !is.null(rhs)) {
    n_tests = n_tests + 1
    if (!all(evaluate_formula(lhs, u) == evaluate_formula(rhs, u))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [distrib-or-%d]\n", trial))
    }
  }
}
cat(sprintf("  Algebraic laws: %d tests, %d failures\n", n_tests, n_failures))

# === Complement laws ===
cat("\n=== Complement laws ===\n")
set.seed(82002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function() {
    n_cl = sample(1:3, 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f = make_formula()
  if (is.null(f)) next

  t_f = evaluate_formula(f, u)

  # f & !f == FALSE
  f_not = tryCatch(!f, error = function(e) NULL)
  if (is.null(f_not)) next
  f_and_not = tryCatch(f & f_not, error = function(e) NULL)
  if (!is.null(f_and_not)) {
    n_tests = n_tests + 1
    t_result = evaluate_formula(f_and_not, u)
    if (any(t_result)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [complement-and-%d]: f & !f should be all FALSE\n", trial))
    }
  }

  # f | !f == TRUE
  f_or_not = tryCatch(f | f_not, error = function(e) NULL)
  if (!is.null(f_or_not)) {
    n_tests = n_tests + 1
    t_result = evaluate_formula(f_or_not, u)
    if (!all(t_result)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [complement-or-%d]: f | !f should be all TRUE\n", trial))
    }
  }
}
cat(sprintf("  Complement laws: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
