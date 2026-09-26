#!/usr/bin/env Rscript
# Test algebraic properties of boolean algebra:
# - Absorption: f & (f | g) == f
# - Double negation: !!f == f
# - Distributivity: f & (g | h) == (f & g) | (f & h)
# - Idempotency: f & f == f, f | f == f
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Absorption ===\n")
set.seed(283001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:3, 1)
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

  f = make_f(); g = make_f()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1
  # Absorption: f & (f | g) should == f
  fog = tryCatch(f | g, error = function(e) NULL)
  if (is.null(fog)) next
  result = tryCatch(f & fog, error = function(e) NULL)
  if (is.null(result)) next

  tf = evaluate_formula(f, u)
  tr = evaluate_formula(result, u)
  if (!all(tf == tr)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [absorb-%d]: f & (f | g) != f\n", trial))
  }

  n_tests = n_tests + 1
  # Absorption: f | (f & g) should == f
  fag = tryCatch(f & g, error = function(e) NULL)
  if (is.null(fag)) next
  result2 = tryCatch(f | fag, error = function(e) NULL)
  if (is.null(result2)) next

  tr2 = evaluate_formula(result2, u)
  if (!all(tf == tr2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [absorb2-%d]: f | (f & g) != f\n", trial))
  }
}
cat(sprintf("  Absorption: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Double negation ===\n")
set.seed(283002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(1:4, 1)
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

  n_tests = n_tests + 1
  nnf = tryCatch(!!f, error = function(e) NULL)
  if (is.null(nnf)) next

  tf = evaluate_formula(f, u)
  tnnf = evaluate_formula(nnf, u)
  if (!all(tf == tnnf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dblneg-%d]: !!f != f\n", trial))
  }
}
cat(sprintf("  Double negation: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Distributivity ===\n")
set.seed(283003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:2, 1)
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

  f = make_f(); g = make_f(); h = make_f()
  if (is.null(f) || is.null(g) || is.null(h)) next

  n_tests = n_tests + 1
  # f & (g | h) should == (f & g) | (f & h)
  goh = tryCatch(g | h, error = function(e) NULL)
  if (is.null(goh)) next
  lhs = tryCatch(f & goh, error = function(e) NULL)
  fg = tryCatch(f & g, error = function(e) NULL)
  fh = tryCatch(f & h, error = function(e) NULL)
  if (is.null(fg) || is.null(fh)) next
  rhs = tryCatch(fg | fh, error = function(e) NULL)
  if (is.null(lhs) || is.null(rhs)) next

  tl = evaluate_formula(lhs, u)
  tr = evaluate_formula(rhs, u)
  if (!all(tl == tr)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [distrib-%d]: f & (g | h) != (f & g) | (f & h)\n", trial))
  }
}
cat(sprintf("  Distributivity: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Idempotency ===\n")
set.seed(283004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(2:4, 1)
  cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 2) next

  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  tf = evaluate_formula(f, u)

  n_tests = n_tests + 1
  # f & f == f
  ff = tryCatch(f & f, error = function(e) NULL)
  if (!is.null(ff) && !all(evaluate_formula(ff, u) == tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [idemp-and-%d]: f & f != f\n", trial))
  }

  n_tests = n_tests + 1
  # f | f == f
  fof = tryCatch(f | f, error = function(e) NULL)
  if (!is.null(fof) && !all(evaluate_formula(fof, u) == tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [idemp-or-%d]: f | f != f\n", trial))
  }
}
cat(sprintf("  Idempotency: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
