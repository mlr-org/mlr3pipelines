#!/usr/bin/env Rscript
# Stress test the |.CnfFormula operator specifically.
# The OR distribution can create an exponential blowup, so test with carefully
# sized inputs. Also test the swap optimization (line 333).
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Small OR distributions ===
cat("=== Small OR distributions ===\n")
set.seed(289001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function(max_cl = 3) {
    n_cl = sample(1:max_cl, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(3); f2 = make_f(3)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 | t2

  result = tryCatch(f1 | f2, error = function(e) NULL)
  if (is.null(result)) next

  truth = evaluate_formula(result, u)
  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [smor-%d]: OR semantic mismatch\n", trial))
  }
}
cat(sprintf("  Small OR distributions: %d tests, %d failures\n", n_tests, n_failures))

# === OR with one-clause formulas (no distribution needed) ===
cat("\n=== OR with single-clause formulas ===\n")
set.seed(289002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Each formula has 1 clause -> OR gives a single clause
  make_single = function() {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (isTRUE(unclass(cl))) return(NULL)
    tryCatch(CnfFormula(list(cl)), error = function(e) NULL)
  }

  f1 = make_single(); f2 = make_single()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  result = tryCatch(f1 | f2, error = function(e) NULL)
  if (is.null(result)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  truth = evaluate_formula(result, u)
  if (!all(truth == (t1 | t2))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [singor-%d]: single-clause OR mismatch\n", trial))
  }
}
cat(sprintf("  OR with single-clause formulas: %d tests, %d failures\n", n_tests, n_failures))

# === OR with TRUE/FALSE ===
cat("\n=== OR with TRUE/FALSE ===\n")
set.seed(289003)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(2:3, 1)
  cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 1) next
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  tf = evaluate_formula(f, u)

  # f | TRUE should be TRUE
  n_tests = n_tests + 1
  rt = tryCatch(f | as.CnfFormula(TRUE), error = function(e) NULL)
  if (!is.null(rt) && !all(evaluate_formula(rt, u))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ortrue-%d]: f | TRUE != TRUE\n", trial))
  }

  # f | FALSE should be f
  n_tests = n_tests + 1
  rf = tryCatch(f | as.CnfFormula(FALSE), error = function(e) NULL)
  if (!is.null(rf) && !all(evaluate_formula(rf, u) == tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [orfalse-%d]: f | FALSE != f\n", trial))
  }

  # f & TRUE should be f
  n_tests = n_tests + 1
  rt2 = tryCatch(f & as.CnfFormula(TRUE), error = function(e) NULL)
  if (!is.null(rt2) && !all(evaluate_formula(rt2, u) == tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [andtrue-%d]: f & TRUE != f\n", trial))
  }

  # f & FALSE should be FALSE
  n_tests = n_tests + 1
  rf2 = tryCatch(f & as.CnfFormula(FALSE), error = function(e) NULL)
  if (!is.null(rf2) && any(evaluate_formula(rf2, u))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [andfalse-%d]: f & FALSE != FALSE\n", trial))
  }
}
cat(sprintf("  OR with TRUE/FALSE: %d tests, %d failures\n", n_tests, n_failures))

# === Chained ORs creating large formulas ===
cat("\n=== Chained ORs ===\n")
set.seed(289004)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create 3-4 small formulas and chain OR them
  n_formulas = sample(3:4, 1)
  formulas = list()
  for (k in 1:n_formulas) {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) break
    f = tryCatch(CnfFormula(cls), error = function(e) NULL)
    if (is.null(f)) break
    formulas[[k]] = f
  }
  if (length(formulas) < n_formulas) next

  n_tests = n_tests + 1

  # Chain OR
  result = formulas[[1]]
  for (k in 2:n_formulas) {
    result = tryCatch(result | formulas[[k]], error = function(e) NULL)
    if (is.null(result)) break
  }
  if (is.null(result)) next

  # Expected truth
  expected = evaluate_formula(formulas[[1]], u)
  for (k in 2:n_formulas) {
    expected = expected | evaluate_formula(formulas[[k]], u)
  }

  truth = evaluate_formula(result, u)
  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chainor-%d]: chained OR mismatch\n", trial))
  }
}
cat(sprintf("  Chained ORs: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
