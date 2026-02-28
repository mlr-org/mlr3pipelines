#!/usr/bin/env Rscript
# Test deeply nested operator combinations: ((A & B) | (C & D)) & ((E | F) & ...)
# This exercises the |.CnfFormula distribution and !.CnfFormula De Morgan paths
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Deep AND/OR/NOT nesting ===\n")
set.seed(70001)

for (trial in 1:500) {
  u = CnfUniverse()
  n_vars = sample(2:4, 1)
  dom_size = sample(2:3, 1)
  dom = paste0("d", 1:dom_size)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Build random expression tree
  make_atom = function() {
    s = sample(names(syms), 1)
    syms[[s]] %among% sample(dom, sample(1:max(1, dom_size - 1), 1))
  }

  # Build expression tree of depth 2-4
  depth = sample(2:4, 1)
  build_expr = function(d) {
    if (d <= 0 || (d <= 1 && sample(c(TRUE, FALSE), 1))) {
      return(as.CnfFormula(as.CnfClause(make_atom())))
    }
    op = sample(c("&", "|", "!"), 1)
    if (op == "!") {
      return(!build_expr(d - 1))
    }
    left = build_expr(d - 1)
    right = build_expr(d - 1)
    if (op == "&") left & right else left | right
  }

  f = tryCatch(build_expr(depth), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [deep-%d]: %s\n", trial, f$message)); next
  }

  # Evaluate
  n_tests = n_tests + 1
  result = tryCatch(evaluate_formula(f, u), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR-EVAL [deep-%d]: %s\n", trial, result$message)); next
  }

  # Verify it's a valid logical vector
  if (!is.logical(result) || any(is.na(result))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [deep-%d]: result contains NA or is not logical\n", trial))
  }
}
cat(sprintf("  Deep nesting: %d tests, %d failures\n", n_tests, n_failures))

# === Verify De Morgan's laws ===
cat("\n=== De Morgan verification ===\n")
set.seed(70002)

for (trial in 1:500) {
  u = CnfUniverse()
  n_vars = sample(2:3, 1)
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create two formulas
  make_formula = function() {
    n_cl = sample(1:3, 1)
    clauses = lapply(1:n_cl, function(j) {
      n_atoms = sample(1:min(n_vars, 2), 1)
      chosen = sample(names(syms), n_atoms)
      atoms = lapply(chosen, function(s) {
        syms[[s]] %among% sample(dom, sample(1:2, 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula()
  f2 = make_formula()
  if (is.null(f1) || is.null(f2)) next

  # De Morgan: !(f1 & f2) == (!f1) | (!f2)
  lhs = tryCatch(!(f1 & f2), error = function(e) NULL)
  rhs = tryCatch((!f1) | (!f2), error = function(e) NULL)
  if (is.null(lhs) || is.null(rhs)) next

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)

  n_tests = n_tests + 1
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan-and-%d]: !(f1 & f2) != (!f1) | (!f2)\n", trial))
  }

  # De Morgan: !(f1 | f2) == (!f1) & (!f2)
  lhs2 = tryCatch(!(f1 | f2), error = function(e) NULL)
  rhs2 = tryCatch((!f1) & (!f2), error = function(e) NULL)
  if (is.null(lhs2) || is.null(rhs2)) next

  t_lhs2 = evaluate_formula(lhs2, u)
  t_rhs2 = evaluate_formula(rhs2, u)

  n_tests = n_tests + 1
  if (!all(t_lhs2 == t_rhs2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan-or-%d]: !(f1 | f2) != (!f1) & (!f2)\n", trial))
  }
}
cat(sprintf("  De Morgan: %d tests, %d failures\n", n_tests, n_failures))

# === Double negation ===
cat("\n=== Double negation ===\n")
set.seed(70003)

for (trial in 1:500) {
  u = CnfUniverse()
  n_vars = sample(2:3, 1)
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(1:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:min(n_vars, 2), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  f_dbl_neg = tryCatch(!!f, error = function(e) NULL)
  if (is.null(f_dbl_neg)) next

  t1 = evaluate_formula(f, u)
  t2 = evaluate_formula(f_dbl_neg, u)

  n_tests = n_tests + 1
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dbl-neg-%d]: f != !!f\n", trial))
  }
}
cat(sprintf("  Double negation: %d tests, %d failures\n", n_tests, n_failures))

# === Absorption laws: f & (f | g) == f, f | (f & g) == f ===
cat("\n=== Absorption laws ===\n")
set.seed(70004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function() {
    n_cl = sample(1:2, 1)
    clauses = lapply(1:n_cl, function(j) {
      n_atoms = sample(1:2, 1)
      chosen = sample(names(syms), n_atoms)
      atoms = lapply(chosen, function(s) {
        syms[[s]] %among% sample(dom, sample(1:2, 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f = make_formula()
  g = make_formula()
  if (is.null(f) || is.null(g)) next

  # f & (f | g) should == f
  fg_or = tryCatch(f | g, error = function(e) NULL)
  if (is.null(fg_or)) next
  absorption1 = tryCatch(f & fg_or, error = function(e) NULL)
  if (is.null(absorption1)) next

  t_f = evaluate_formula(f, u)
  t_abs1 = evaluate_formula(absorption1, u)

  n_tests = n_tests + 1
  if (!all(t_f == t_abs1)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [absorb-and-%d]: f & (f | g) != f\n", trial))
  }

  # f | (f & g) should == f
  fg_and = tryCatch(f & g, error = function(e) NULL)
  if (is.null(fg_and)) next
  absorption2 = tryCatch(f | fg_and, error = function(e) NULL)
  if (is.null(absorption2)) next

  t_abs2 = evaluate_formula(absorption2, u)

  n_tests = n_tests + 1
  if (!all(t_f == t_abs2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [absorb-or-%d]: f | (f & g) != f\n", trial))
  }
}
cat(sprintf("  Absorption laws: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
