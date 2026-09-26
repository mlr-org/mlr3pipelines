#!/usr/bin/env Rscript
# Stress test negation operator and its interaction with other operations
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

set.seed(77777)

cat("=== Double negation ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e"))
  syms = list(X = X, Y = Y)

  n_clauses = sample(1:3, 1)
  clause_list = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:2, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clause_list), error = function(e) NULL)
  if (is.null(f)) next

  f_neg_neg = tryCatch(!(!f), error = function(e) NULL)
  if (is.null(f_neg_neg)) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("  ERROR [trial %d]: !!f failed\n", trial)); next
  }

  n_tests = n_tests + 1
  t_orig = evaluate_formula(f, u)
  t_nn = evaluate_formula(f_neg_neg, u)
  if (!all(t_orig == t_nn)) {
    n_failures = n_failures + 1
    idx = which(t_orig != t_nn)[1]
    cat(sprintf("  FAIL [trial %d]: !!f != f at row %d\n", trial, idx))
  }
}
cat(sprintf("  Double negation: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== De Morgan: !(f1 & f2) == !f1 | !f2 ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b"))
  Y = CnfSymbol(u, "Y", c("c", "d"))
  Z = CnfSymbol(u, "Z", c("e", "f"))
  syms = list(X = X, Y = Y, Z = Z)

  mk_clause = function() {
    n_atoms = sample(1:2, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  }

  f1 = tryCatch(CnfFormula(list(mk_clause(), mk_clause())), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(list(mk_clause())), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  f_and = tryCatch(f1 & f2, error = function(e) NULL)
  if (is.null(f_and)) next

  neg_and = tryCatch(!f_and, error = function(e) NULL)
  neg_f1 = tryCatch(!f1, error = function(e) NULL)
  neg_f2 = tryCatch(!f2, error = function(e) NULL)
  if (is.null(neg_and) || is.null(neg_f1) || is.null(neg_f2)) next

  demorgan = tryCatch(neg_f1 | neg_f2, error = function(e) NULL)
  if (is.null(demorgan)) next

  n_tests = n_tests + 1
  t_neg_and = evaluate_formula(neg_and, u)
  t_demorgan = evaluate_formula(demorgan, u)
  if (!all(t_neg_and == t_demorgan)) {
    n_failures = n_failures + 1
    idx = which(t_neg_and != t_demorgan)[1]
    cat(sprintf("  FAIL [trial %d]: De Morgan violation at row %d\n", trial, idx))
  }
}
cat(sprintf("  De Morgan &: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== De Morgan reverse: !(f1 | f2) == !f1 & !f2 ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b"))
  Y = CnfSymbol(u, "Y", c("c", "d"))
  syms = list(X = X, Y = Y)

  mk_formula = function() {
    s = sample(names(syms), 1)
    dom = u[[s]]
    cl = as.CnfClause(syms[[s]] %among% sample(dom, 1))
    CnfFormula(list(cl))
  }

  f1 = tryCatch(mk_formula(), error = function(e) NULL)
  f2 = tryCatch(mk_formula(), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  f_or = tryCatch(f1 | f2, error = function(e) NULL)
  if (is.null(f_or)) next

  neg_or = tryCatch(!f_or, error = function(e) NULL)
  neg_f1 = tryCatch(!f1, error = function(e) NULL)
  neg_f2 = tryCatch(!f2, error = function(e) NULL)
  if (is.null(neg_or) || is.null(neg_f1) || is.null(neg_f2)) next

  demorgan = tryCatch(neg_f1 & neg_f2, error = function(e) NULL)
  if (is.null(demorgan)) next

  n_tests = n_tests + 1
  t_neg_or = evaluate_formula(neg_or, u)
  t_demorgan = evaluate_formula(demorgan, u)
  if (!all(t_neg_or == t_demorgan)) {
    n_failures = n_failures + 1
    idx = which(t_neg_or != t_demorgan)[1]
    cat(sprintf("  FAIL [trial %d]: De Morgan reverse violation at row %d\n", trial, idx))
  }
}
cat(sprintf("  De Morgan |: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Negation of tautology/contradiction ===\n")
u2 = CnfUniverse()
X2 = CnfSymbol(u2, "X", c("a", "b", "c"))
Y2 = CnfSymbol(u2, "Y", c("d", "e"))

n_tests = n_tests + 1
neg_true = !as.CnfFormula(TRUE)
if (!isFALSE(as.logical(neg_true))) {
  n_failures = n_failures + 1
  cat("  FAIL: !TRUE should be FALSE\n")
}

n_tests = n_tests + 1
neg_false = !as.CnfFormula(FALSE)
if (!isTRUE(as.logical(neg_false))) {
  n_failures = n_failures + 1
  cat("  FAIL: !FALSE should be TRUE\n")
}

n_tests = n_tests + 1
contra = X2 %among% "a" & X2 %among% "b"
neg_contra = tryCatch(!contra, error = function(e) NULL)
if (!is.null(neg_contra)) {
  if (!isTRUE(as.logical(neg_contra))) {
    n_failures = n_failures + 1
    cat("  FAIL: !contradiction should be TRUE\n")
  }
}

cat(sprintf("  Tautology/contradiction: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Negation + AND combo ===\n")
for (trial in 1:150) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b"))
  Y = CnfSymbol(u, "Y", c("c", "d"))
  Z = CnfSymbol(u, "Z", c("e", "f"))
  syms = list(X = X, Y = Y, Z = Z)

  n_clauses = sample(2:4, 1)
  clause_list = lapply(1:n_clauses, function(j) {
    s = sample(names(syms), 1)
    dom = u[[s]]
    as.CnfClause(syms[[s]] %among% sample(dom, 1))
  })

  f = tryCatch(CnfFormula(clause_list), error = function(e) NULL)
  if (is.null(f)) next

  neg_f = tryCatch(!f, error = function(e) NULL)
  if (is.null(neg_f)) next

  n_tests = n_tests + 1
  t_f = evaluate_formula(f, u)
  t_neg = evaluate_formula(neg_f, u)
  if (!all(t_f != t_neg)) {
    n_failures = n_failures + 1
    idx = which(t_f == t_neg)[1]
    cat(sprintf("  FAIL [trial %d]: f and !f agree at row %d\n", trial, idx))
  }
}
cat(sprintf("  Negation + AND: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Negation with multi-symbol clauses ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e"))
  syms = list(X = X, Y = Y)

  n_clauses = sample(1:3, 1)
  clause_list = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:2, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clause_list), error = function(e) NULL)
  if (is.null(f)) next

  neg_f = tryCatch(!f, error = function(e) NULL)
  if (is.null(neg_f)) next

  n_tests = n_tests + 1
  t_f = evaluate_formula(f, u)
  t_neg = evaluate_formula(neg_f, u)
  if (!all(t_f != t_neg)) {
    n_failures = n_failures + 1
    idx = which(t_f == t_neg)[1]
    cat(sprintf("  FAIL [trial %d]: f and !f agree at row %d (f=%s, !f=%s)\n",
      trial, idx, t_f[idx], t_neg[idx]))
  }
}
cat(sprintf("  Multi-symbol negation: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
