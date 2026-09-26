#!/usr/bin/env Rscript
# Large formula operations:
# Test AND, OR, NOT on formulas with many clauses (10-30).
# This produces very large intermediate results, especially for OR
# which distributes. Also tests the swap optimization in |.CnfFormula.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: Large formula AND large formula ===
cat("=== Large AND large ===\n")
set.seed(223001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_large_f = function(n_range) {
    n_cl = sample(n_range, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 2) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_large_f(5:15)
  f2 = make_large_f(5:15)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  result = tryCatch(f1 & f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [largeand-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 & t2
  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [largeand-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Large AND large: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Small formula OR large formula (tests swap optimization) ===
cat("\n=== Small OR large (swap optimization) ===\n")
set.seed(223002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function(n_range) {
    n_cl = sample(n_range, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f_small = make_f(1:2)
  f_large = make_f(4:8)
  if (is.null(f_small) || is.null(f_large)) next

  n_tests = n_tests + 1
  # Test both orderings (swap should make them equivalent)
  r1 = tryCatch(f_small | f_large, error = function(e) e)
  r2 = tryCatch(f_large | f_small, error = function(e) e)

  if (inherits(r1, "error") || inherits(r2, "error")) {
    n_failures = n_failures + 1
    msg = if (inherits(r1, "error")) r1$message else r2$message
    cat(sprintf("ERROR [swapl-%d]: %s\n", trial, msg)); next
  }

  t_small = evaluate_formula(f_small, u)
  t_large = evaluate_formula(f_large, u)
  expected = t_small | t_large

  actual1 = evaluate_formula(r1, u)
  actual2 = evaluate_formula(r2, u)

  if (!all(actual1 == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [swapl-%d]: small|large mismatch\n", trial)); next
  }
  if (!all(actual2 == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [swapl-%d]: large|small mismatch\n", trial))
  }
}
cat(sprintf("  Small OR large: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: NOT of large formula ===
cat("\n=== NOT of large formula ===\n")
set.seed(223003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:6, 1)
  cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 2) next

  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  nf = tryCatch(!f, error = function(e) e)
  if (inherits(nf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [lnot-%d]: %s\n", trial, nf$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_nf = evaluate_formula(nf, u)
  if (!all(t_nf == !t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [lnot-%d]: negation mismatch\n", trial))
  }
}
cat(sprintf("  NOT of large: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Triple composition: (f1 | f2) & !f3 ===
cat("\n=== Triple composition ===\n")
set.seed(223004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:4, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (any(sapply(list(f1, f2, f3), is.null))) next

  n_tests = n_tests + 1
  result = tryCatch((f1 | f2) & !f3, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [triple-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  expected = (t1 | t2) & !t3
  actual = evaluate_formula(result, u)

  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [triple-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Triple composition: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
