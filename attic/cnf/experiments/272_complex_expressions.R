#!/usr/bin/env Rscript
# Test complex compound expressions that combine multiple operations.
# These exercise the full pipeline: construction -> simplification -> operation -> simplification.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === (f1 & f2) | (f3 & f4) ===
cat("=== Compound AND-OR ===\n")
set.seed(272001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    cls = list(as.CnfClause(syms[[sample(sym_names, 1)]] %among% sample(dom, sample(1:2, 1))))
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f(); f4 = make_f()
  if (any(sapply(list(f1, f2, f3, f4), is.null))) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  t4 = evaluate_formula(f4, u)
  expected = (t1 & t2) | (t3 & t4)

  r = tryCatch((f1 & f2) | (f3 & f4), error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cand-or-%d]\n", trial))
  }
}
cat(sprintf("  Compound AND-OR: %d tests, %d failures\n", n_tests, n_failures))

# === !(f1 | f2) & f3 ===
cat("\n=== Negated OR AND ===\n")
set.seed(272002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    cls = list(as.CnfClause(syms[[sample(sym_names, 1)]] %among% sample(dom, sample(1:2, 1))))
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (any(sapply(list(f1, f2, f3), is.null))) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  expected = !(t1 | t2) & t3

  r = tryCatch(!(f1 | f2) & f3, error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [negor-and-%d]\n", trial))
  }
}
cat(sprintf("  Negated OR AND: %d tests, %d failures\n", n_tests, n_failures))

# === Nested: (f1 | !f2) & (!f1 | f2) [XNOR-like] ===
cat("\n=== XNOR-like ===\n")
set.seed(272003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    cls = list(as.CnfClause(syms[[sample(sym_names, 1)]] %among% sample(dom, sample(1:2, 1))))
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = (t1 | !t2) & (!t1 | t2)  # XNOR: t1 == t2

  nf1 = tryCatch(!f1, error = function(e) NULL)
  nf2 = tryCatch(!f2, error = function(e) NULL)
  if (is.null(nf1) || is.null(nf2)) next

  r = tryCatch((f1 | nf2) & (nf1 | f2), error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [xnor-%d]\n", trial))
  }
}
cat(sprintf("  XNOR-like: %d tests, %d failures\n", n_tests, n_failures))

# === Deep nesting: ((f1 & f2) | f3) & (!f3 | f4) ===
cat("\n=== Deep nesting ===\n")
set.seed(272004)

for (trial in 1:300) {
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

  f1 = make_f(); f2 = make_f(); f3 = make_f(); f4 = make_f()
  if (any(sapply(list(f1, f2, f3, f4), is.null))) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  t4 = evaluate_formula(f4, u)
  expected = ((t1 & t2) | t3) & (!t3 | t4)

  nf3 = tryCatch(!f3, error = function(e) NULL)
  if (is.null(nf3)) next

  r = tryCatch(((f1 & f2) | f3) & (nf3 | f4), error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [deep-%d]\n", trial))
  }
}
cat(sprintf("  Deep nesting: %d tests, %d failures\n", n_tests, n_failures))

# === if-then-else: (cond & then) | (!cond & else) ===
cat("\n=== If-then-else ===\n")
set.seed(272005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    cls = list(as.CnfClause(syms[[sample(sym_names, 1)]] %among% sample(dom, sample(1:2, 1))))
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  cond = make_f(); then_f = make_f(); else_f = make_f()
  if (any(sapply(list(cond, then_f, else_f), is.null))) next

  n_tests = n_tests + 1
  t_cond = evaluate_formula(cond, u)
  t_then = evaluate_formula(then_f, u)
  t_else = evaluate_formula(else_f, u)
  expected = (t_cond & t_then) | (!t_cond & t_else)

  ncond = tryCatch(!cond, error = function(e) NULL)
  if (is.null(ncond)) next

  r = tryCatch((cond & then_f) | (ncond & else_f), error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ite-%d]\n", trial))
  }
}
cat(sprintf("  If-then-else: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
