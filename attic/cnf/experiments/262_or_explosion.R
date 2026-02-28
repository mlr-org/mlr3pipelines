#!/usr/bin/env Rscript
# Test OR distribution that creates many intermediate clauses.
# f1 | f2 where both have multiple clauses creates len(f1)*len(f2) clauses.
# Test that the simplification after distribution produces correct results
# even when many intermediate tautologies are created and eliminated.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Medium OR explosion (2x3 = 6 clauses) ===
cat("=== 2x3 OR ===\n")
set.seed(262001)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function(n_cl) {
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

  f1 = make_f(2); f2 = make_f(3)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 | t2

  r = tryCatch(f1 | f2, error = function(e) NULL)
  if (is.null(r)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [or2x3-%d]: OR failed\n", trial)); next
  }
  if (!all(evaluate_formula(r, u) == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [or2x3-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2x3 OR: %d tests, %d failures\n", n_tests, n_failures))

# === Larger OR explosion (3x3 = 9 clauses) ===
cat("\n=== 3x3 OR ===\n")
set.seed(262002)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function(n_cl) {
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < n_cl) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(3); f2 = make_f(3)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 | t2

  r = tryCatch(f1 | f2, error = function(e) NULL)
  if (is.null(r)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [or3x3-%d]: OR failed\n", trial)); next
  }
  if (!all(evaluate_formula(r, u) == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [or3x3-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  3x3 OR: %d tests, %d failures\n", n_tests, n_failures))

# === Chained OR: (f1 | f2) | f3 ===
cat("\n=== Chained OR ===\n")
set.seed(262003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(2:3, 1)
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

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  expected = t1 | t2 | t3

  r = tryCatch((f1 | f2) | f3, error = function(e) NULL)
  if (is.null(r)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [orchained-%d]: OR failed\n", trial)); next
  }
  if (!all(evaluate_formula(r, u) == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [orchained-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Chained OR: %d tests, %d failures\n", n_tests, n_failures))

# === OR that should create many tautologies ===
cat("\n=== OR with tautologies ===\n")
set.seed(262004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create f1 and f2 that use the same symbols with complementary ranges
  # This should create many tautological clauses during distribution
  clauses1 = list()
  clauses2 = list()
  for (j in 1:2) {
    s = sample(sym_names, 1)
    r = sample(dom, sample(1:2, 1))
    r_comp = setdiff(dom, r)
    if (length(r_comp) == 0) r_comp = sample(dom, 1)
    clauses1[[j]] = as.CnfClause(syms[[s]] %among% r)
    clauses2[[j]] = as.CnfClause(syms[[s]] %among% r_comp)
  }
  clauses1 = clauses1[!sapply(clauses1, function(x) isTRUE(unclass(x)) || isFALSE(unclass(x)))]
  clauses2 = clauses2[!sapply(clauses2, function(x) isTRUE(unclass(x)) || isFALSE(unclass(x)))]
  if (length(clauses1) < 1 || length(clauses2) < 1) next

  f1 = tryCatch(CnfFormula(clauses1), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(clauses2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 | t2

  r = tryCatch(f1 | f2, error = function(e) NULL)
  if (is.null(r)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ortaut-%d]: OR failed\n", trial)); next
  }
  if (!all(evaluate_formula(r, u) == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ortaut-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  OR with tautologies: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
