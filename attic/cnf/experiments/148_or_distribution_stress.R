#!/usr/bin/env Rscript
# Stress test |.CnfFormula distribution code:
# - Distribution where tautology detection fires during inner loop
# - Distribution with single-clause formulas (1x1, 1xN, Nx1)
# - Distribution where all clauses become tautological
# - Chained OR creating large intermediate formulas
# - Verify swap optimization (length check at line 333) is correct
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Distribution tautology detection ===
cat("=== Distribution tautology during inner loop ===\n")
set.seed(148001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create two formulas, each with 1-3 clauses
  make_f = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_f(sample(1:3, 1))
  f2 = make_f(sample(1:3, 1))
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  result = tryCatch(f1 | f2, error = function(e) NULL)
  if (is.null(result)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dist-%d]: OR failed\n", trial)); next
  }
  t_or = evaluate_formula(result, u)
  if (!all(t_or == (t1 | t2))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dist-%d]: f1|f2 semantics wrong\n", trial))
  }
}
cat(sprintf("  Distribution tautology: %d tests, %d failures\n", n_tests, n_failures))

# === OR with near-tautological clauses ===
cat("\n=== Near-tautological OR ===\n")
set.seed(148002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  # Create formulas with large ranges (2 out of 3)
  # so that OR distribution is likely to create tautologies
  make_f = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 2))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_f(sample(1:3, 1))
  f2 = make_f(sample(1:3, 1))
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  result = tryCatch(f1 | f2, error = function(e) NULL)
  if (is.null(result)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [near-taut-%d]: OR failed\n", trial)); next
  }
  t_or = evaluate_formula(result, u)
  if (!all(t_or == (t1 | t2))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [near-taut-%d]\n", trial))
  }
}
cat(sprintf("  Near-tautological: %d tests, %d failures\n", n_tests, n_failures))

# === Chained OR (f1 | f2 | f3 | f4) ===
cat("\n=== Chained OR ===\n")
set.seed(148003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  n_formulas = sample(3:4, 1)
  formulas = lapply(1:n_formulas, function(i) make_f(sample(1:2, 1)))
  if (any(sapply(formulas, is.null))) next

  # Compute expected truth table
  truths = lapply(formulas, function(f) evaluate_formula(f, u))
  expected = Reduce(`|`, truths)

  # Compute chained OR
  n_tests = n_tests + 1
  result = tryCatch(Reduce(function(x, y) x | y, formulas), error = function(e) NULL)
  if (is.null(result)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d]: chained OR failed\n", trial)); next
  }
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d]: chained OR semantics wrong\n", trial))
  }
}
cat(sprintf("  Chained OR: %d tests, %d failures\n", n_tests, n_failures))

# === OR swap optimization test ===
cat("\n=== OR swap optimization ===\n")
set.seed(148004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  make_f = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  # Create asymmetric formulas to test swap: f1 has 1 clause, f2 has 3-4
  f_short = make_f(1)
  f_long = make_f(sample(3:4, 1))
  if (is.null(f_short) || is.null(f_long)) next

  t_short = evaluate_formula(f_short, u)
  t_long = evaluate_formula(f_long, u)
  expected = t_short | t_long

  # Test both orderings (swap should make them equivalent)
  n_tests = n_tests + 1
  r1 = tryCatch(f_short | f_long, error = function(e) NULL)
  r2 = tryCatch(f_long | f_short, error = function(e) NULL)
  if (is.null(r1) || is.null(r2)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [swap-%d]: OR failed\n", trial)); next
  }
  t1 = evaluate_formula(r1, u)
  t2 = evaluate_formula(r2, u)
  if (!all(t1 == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [swap-%d]: short|long wrong\n", trial))
  }
  if (!all(t2 == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [swap-%d]: long|short wrong\n", trial))
  }
}
cat(sprintf("  Swap optimization: %d tests, %d failures\n", n_tests, n_failures))

# === OR then AND then OR (complex expression tree) ===
cat("\n=== Complex expression trees ===\n")
set.seed(148005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    clauses = lapply(1:sample(1:2, 1), function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f(); f4 = make_f()
  if (is.null(f1) || is.null(f2) || is.null(f3) || is.null(f4)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  t4 = evaluate_formula(f4, u)

  # (f1 | f2) & (f3 | f4)
  expected = (t1 | t2) & (t3 | t4)
  n_tests = n_tests + 1

  result = tryCatch({
    left = f1 | f2
    right = f3 | f4
    left & right
  }, error = function(e) NULL)
  if (is.null(result)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [tree-%d]\n", trial)); next
  }
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [tree-%d]: expression tree wrong\n", trial))
  }
}
cat(sprintf("  Expression trees: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
