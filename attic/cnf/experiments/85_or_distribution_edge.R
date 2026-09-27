#!/usr/bin/env Rscript
# Test |.CnfFormula distribution edge cases:
# - Asymmetric formula sizes (the swap on line 333)
# - Distribution producing tautological clauses (eliminated during distribution)
# - Distribution producing duplicate clauses
# - Large cross-products
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Swap optimization test ===
cat("=== OR swap optimization ===\n")
set.seed(85001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create f1 with few clauses, f2 with many clauses
  make_formula = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  # Test f1 | f2 with different size ratios
  f_small = make_formula(1)
  f_big = make_formula(sample(3:5, 1))
  if (is.null(f_small) || is.null(f_big)) next

  n_tests = n_tests + 1

  # Both orderings should produce same result
  r1 = tryCatch(f_small | f_big, error = function(e) NULL)
  r2 = tryCatch(f_big | f_small, error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    t1 = evaluate_formula(r1, u)
    t2 = evaluate_formula(r2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [swap-%d]: different results\n", trial))
    }
  }
}
cat(sprintf("  Swap optimization: %d tests, %d failures\n", n_tests, n_failures))

# === Distribution producing tautologies ===
cat("\n=== Distribution tautology detection ===\n")
set.seed(85002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Create clauses where OR distribution should produce tautological clauses
  # f1 = A %among% {a,b}, f2 = (A %among% {c}) & (B %among% {a})
  # f1 | f2 = distribute: (A %among% {a,b} | A %among% {c}) & (A %among% {a,b} | B %among% {a})
  # First distributed clause: A %among% {a,b,c} = TRUE, should be eliminated
  v1 = sample(dom, sample(1:2, 1))
  v2 = setdiff(dom, v1)

  f1 = tryCatch(CnfFormula(list(as.CnfClause(A %among% v1))), error = function(e) NULL)
  clauses2 = list(
    as.CnfClause(A %among% v2),
    as.CnfClause(B %among% sample(dom, sample(1:2, 1)))
  )
  f2 = tryCatch(CnfFormula(clauses2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  r = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(r, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [taut-dist-%d]: %s\n", trial, r$message)); next
  }

  # Verify semantically
  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  t_r = evaluate_formula(r, u)
  if (!all((t_f1 | t_f2) == t_r)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [taut-dist-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Distribution tautology: %d tests, %d failures\n", n_tests, n_failures))

# === OR with single-clause formulas (minimal distribution) ===
cat("\n=== Minimal distribution ===\n")
set.seed(85003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Single clause formulas: f1 | f2 should produce a single clause
  chosen1 = sample(names(syms), sample(1:2, 1))
  atoms1 = lapply(chosen1, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  cl1 = as.CnfClause(Reduce(`|`, atoms1))

  chosen2 = sample(names(syms), sample(1:2, 1))
  atoms2 = lapply(chosen2, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  cl2 = as.CnfClause(Reduce(`|`, atoms2))

  f1 = tryCatch(CnfFormula(list(cl1)), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(list(cl2)), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  r = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(r, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [min-dist-%d]: %s\n", trial, r$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  t_r = evaluate_formula(r, u)
  if (!all((t_f1 | t_f2) == t_r)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [min-dist-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Minimal distribution: %d tests, %d failures\n", n_tests, n_failures))

# === Large cross-products from OR ===
cat("\n=== Large cross-product OR ===\n")
set.seed(85004)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  # 3-clause | 3-clause -> up to 9 distributed clauses
  f1 = make_formula(3)
  f2 = make_formula(3)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  r = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(r, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cross-%d]: %s\n", trial, r$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  t_r = evaluate_formula(r, u)
  if (!all((t_f1 | t_f2) == t_r)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cross-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Large cross-product: %d tests, %d failures\n", n_tests, n_failures))

# === Triple OR: (f1 | f2) | f3 vs f1 | (f2 | f3) ===
cat("\n=== Triple OR associativity ===\n")
set.seed(85005)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula(sample(1:2, 1))
  f2 = make_formula(sample(1:2, 1))
  f3 = make_formula(sample(1:2, 1))
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  n_tests = n_tests + 1
  r1 = tryCatch((f1 | f2) | f3, error = function(e) NULL)
  r2 = tryCatch(f1 | (f2 | f3), error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    t1 = evaluate_formula(r1, u)
    t2 = evaluate_formula(r2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [triple-or-%d]: associativity violation\n", trial))
    }
  }
}
cat(sprintf("  Triple OR: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
