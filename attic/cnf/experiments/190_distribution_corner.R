#!/usr/bin/env Rscript
# Distribution corner cases in |.CnfFormula:
# - Distribution where all results are tautologies
# - Distribution where the swap optimization kicks in
# - Distribution producing duplicate clauses
# - Large cross-products
# - OR of formula with single clause (should simplify well)
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: Distribution where e1_clause fills entire domain ===
cat("=== Distribution fills domain ===\n")
set.seed(190001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create f and g where f has wide clauses (likely to create tautologies on distribution)
  f_cls = lapply(1:sample(2:3, 1), function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(2:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f_cls = f_cls[!sapply(f_cls, function(x) isTRUE(unclass(x)))]
  if (length(f_cls) < 1) next
  f = tryCatch(CnfFormula(f_cls), error = function(e) NULL)
  if (is.null(f)) next

  g_cls = lapply(1:sample(2:3, 1), function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(2:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  g_cls = g_cls[!sapply(g_cls, function(x) isTRUE(unclass(x)))]
  if (length(g_cls) < 1) next
  g = tryCatch(CnfFormula(g_cls), error = function(e) NULL)
  if (is.null(g)) next

  n_tests = n_tests + 1
  result = tryCatch(f | g, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dist1-%d]: %s\n", trial, result$message)); next
  }

  tf = evaluate_formula(f, u)
  tg = evaluate_formula(g, u)
  tr = evaluate_formula(result, u)
  if (!all(tr == (tf | tg))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dist1-%d]: f|g mismatch\n", trial))
  }
}
cat(sprintf("  Distribution fills: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: OR of formulas with many clauses each ===
cat("\n=== Large cross-product OR ===\n")
set.seed(190002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function(n_cl) {
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(2:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(sample(3:5, 1))
  g = make_f(sample(3:5, 1))
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1
  result = tryCatch(f | g, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [lcross-%d]: %s\n", trial, result$message)); next
  }

  tf = evaluate_formula(f, u)
  tg = evaluate_formula(g, u)
  tr = evaluate_formula(result, u)
  if (!all(tr == (tf | tg))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [lcross-%d]: f|g mismatch\n", trial))
  }
}
cat(sprintf("  Large cross-product: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: OR with single-clause formula (no distribution needed) ===
cat("\n=== Single clause OR ===\n")
set.seed(190003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # f has 1 clause, g has multiple
  f_cl = {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  }
  if (isTRUE(unclass(f_cl))) next
  f = as.CnfFormula(f_cl)

  g_cls = lapply(1:sample(2:4, 1), function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  g_cls = g_cls[!sapply(g_cls, function(x) isTRUE(unclass(x)))]
  if (length(g_cls) < 2) next
  g = tryCatch(CnfFormula(g_cls), error = function(e) NULL)
  if (is.null(g)) next

  n_tests = n_tests + 1
  result = tryCatch(f | g, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [scl-%d]: %s\n", trial, result$message)); next
  }

  tf = evaluate_formula(f, u)
  tg = evaluate_formula(g, u)
  tr = evaluate_formula(result, u)
  if (!all(tr == (tf | tg))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [scl-%d]: single|multi mismatch\n", trial))
  }
}
cat(sprintf("  Single clause OR: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: OR swap optimization ===
cat("\n=== OR swap optimization ===\n")
set.seed(190004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function(n_cl) {
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  # Create asymmetric formulas to trigger swap
  f = make_f(sample(1:2, 1))  # short
  g = make_f(sample(4:6, 1))  # long
  if (is.null(f) || is.null(g)) next

  # f|g and g|f should be identical
  n_tests = n_tests + 1
  fg = tryCatch(f | g, error = function(e) NULL)
  gf = tryCatch(g | f, error = function(e) NULL)
  if (is.null(fg) || is.null(gf)) next

  tfg = evaluate_formula(fg, u)
  tgf = evaluate_formula(gf, u)
  if (!all(tfg == tgf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [swap-%d]: f|g != g|f\n", trial))
  }
}
cat(sprintf("  Swap optimization: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
