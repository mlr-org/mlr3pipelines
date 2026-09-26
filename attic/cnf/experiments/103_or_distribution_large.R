#!/usr/bin/env Rscript
# Test |.CnfFormula with large intermediate formulas:
# - Distribution creates many clauses that need simplification
# - Swap optimization (length(e1) > length(e2))
# - Distribution where most results are tautological
# - Distribution followed by &, then more operations
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Swap optimization: ensure length(e1) > length(e2) triggers swap correctly ===
cat("=== OR swap optimization ===\n")
set.seed(103001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Create f1 with more clauses than f2
  f1_clauses = lapply(1:sample(3:5, 1), function(i) {
    n_sym = sample(1:2, 1)
    chosen = sample(c("A", "B", "C"), n_sym)
    atoms = lapply(chosen, function(s) {
      sym = switch(s, A = A, B = B, C = C)
      sym %among% sample(dom, sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f1 = tryCatch(CnfFormula(f1_clauses), error = function(e) NULL)
  if (is.null(f1)) next

  f2_clauses = lapply(1:sample(1:2, 1), function(i) {
    n_sym = sample(1:2, 1)
    chosen = sample(c("A", "B", "C"), n_sym)
    atoms = lapply(chosen, function(s) {
      sym = switch(s, A = A, B = B, C = C)
      sym %among% sample(dom, sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f2 = tryCatch(CnfFormula(f2_clauses), error = function(e) NULL)
  if (is.null(f2)) next

  # f1 has more clauses, so |.CnfFormula should swap
  n_tests = n_tests + 1
  or_result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(or_result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [swap-%d]: %s\n", trial, or_result$message)); next
  }

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t_or = evaluate_formula(or_result, u)
  expected = t1 | t2
  if (!all(t_or == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [swap-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  OR swap: %d tests, %d failures\n", n_tests, n_failures))

# === Large cross-product: multi-clause OR multi-clause ===
cat("\n=== Large cross-product OR ===\n")
set.seed(103002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2", "3")
  n_vars = sample(3:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:min(3, n_vars), 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula(sample(2:4, 1))
  f2 = make_formula(sample(2:4, 1))
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  or_result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(or_result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [large-or-%d]: %s\n", trial, or_result$message)); next
  }

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t_or = evaluate_formula(or_result, u)
  expected = t1 | t2
  if (!all(t_or == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [large-or-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Large cross-product OR: %d tests, %d failures\n", n_tests, n_failures))

# === Chained OR: (f1 | f2) | f3 ===
cat("\n=== Chained OR ===\n")
set.seed(103003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("x", "y", "z")
  n_vars = sample(3:4, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("W", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:min(2, n_vars), 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula(sample(1:3, 1))
  f2 = make_formula(sample(1:3, 1))
  f3 = make_formula(sample(1:3, 1))
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  n_tests = n_tests + 1
  chained = tryCatch((f1 | f2) | f3, error = function(e) e)
  if (inherits(chained, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d]: %s\n", trial, chained$message)); next
  }

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  t_chain = evaluate_formula(chained, u)
  expected = t1 | t2 | t3
  if (!all(t_chain == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Chained OR: %d tests, %d failures\n", n_tests, n_failures))

# === OR then AND then OR ===
cat("\n=== Interleaved OR and AND ===\n")
set.seed(103004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  make_formula = function() {
    n_cl = sample(1:2, 1)
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(c("A", "B", "C"), n_sym)
      atoms = lapply(chosen, function(s) {
        sym = switch(s, A = A, B = B, C = C)
        sym %among% sample(dom, sample(1:2, 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula(); f2 = make_formula(); f3 = make_formula(); f4 = make_formula()
  if (is.null(f1) || is.null(f2) || is.null(f3) || is.null(f4)) next

  n_tests = n_tests + 1
  result = tryCatch((f1 | f2) & (f3 | f4), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mixed-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u); t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u); t4 = evaluate_formula(f4, u)
  t_result = evaluate_formula(result, u)
  expected = (t1 | t2) & (t3 | t4)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixed-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Interleaved OR/AND: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
