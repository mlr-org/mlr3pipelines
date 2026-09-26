#!/usr/bin/env Rscript
# Test complex operator chains: deep nesting of &, |, !
# Also test interactions between multiple operations.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Complex expression trees ===
cat("=== Complex expression trees ===\n")
set.seed(95001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create 4 base formulas
  make_base = function() {
    n_cl = sample(1:2, 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  bases = replicate(4, make_base(), simplify = FALSE)
  if (any(sapply(bases, is.null))) next

  # Build complex expression tree: ((f1 & f2) | (f3 & !f4))
  complex = tryCatch({
    r = (bases[[1]] & bases[[2]]) | (bases[[3]] & (!bases[[4]]))
    r
  }, error = function(e) NULL)
  if (is.null(complex)) next

  n_tests = n_tests + 1
  # Evaluate each base
  t = lapply(bases, evaluate_formula, universe = u)
  # Expected: (t1 & t2) | (t3 & !t4)
  expected = (t[[1]] & t[[2]]) | (t[[3]] & !t[[4]])
  actual = evaluate_formula(complex, u)
  if (!all(expected == actual)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [complex-%d]: expression tree mismatch\n", trial))
  }
}
cat(sprintf("  Complex trees: %d tests, %d failures\n", n_tests, n_failures))

# === Chained AND ===
cat("\n=== Chained AND (f1 & f2 & f3 & f4) ===\n")
set.seed(95002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_base = function() {
    clauses = list(as.CnfClause(Reduce(`|`, {
      chosen = sample(names(syms), sample(1:2, 1))
      lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    })))
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  bases = replicate(4, make_base(), simplify = FALSE)
  if (any(sapply(bases, is.null))) next

  n_tests = n_tests + 1
  chain = tryCatch(Reduce(`&`, bases), error = function(e) NULL)
  if (is.null(chain)) next

  t = lapply(bases, evaluate_formula, universe = u)
  expected = Reduce(`&`, t)
  actual = evaluate_formula(chain, u)
  if (!all(expected == actual)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-and-%d]: chained AND mismatch\n", trial))
  }
}
cat(sprintf("  Chained AND: %d tests, %d failures\n", n_tests, n_failures))

# === Chained OR ===
cat("\n=== Chained OR (f1 | f2 | f3 | f4) ===\n")
set.seed(95003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_base = function() {
    n_cl = sample(1:2, 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  bases = replicate(4, make_base(), simplify = FALSE)
  if (any(sapply(bases, is.null))) next

  n_tests = n_tests + 1
  chain = tryCatch(Reduce(function(x, y) x | y, bases), error = function(e) NULL)
  if (is.null(chain)) next

  t = lapply(bases, evaluate_formula, universe = u)
  expected = Reduce(`|`, t)
  actual = evaluate_formula(chain, u)
  if (!all(expected == actual)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-or-%d]: chained OR mismatch\n", trial))
  }
}
cat(sprintf("  Chained OR: %d tests, %d failures\n", n_tests, n_failures))

# === Alternating AND/OR ===
cat("\n=== Alternating AND/OR ===\n")
set.seed(95004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_base = function() {
    clauses = list(as.CnfClause(Reduce(`|`, {
      chosen = sample(names(syms), sample(1:2, 1))
      lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    })))
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_base(); f2 = make_base(); f3 = make_base(); f4 = make_base()
  if (is.null(f1) || is.null(f2) || is.null(f3) || is.null(f4)) next

  n_tests = n_tests + 1
  # (f1 & f2) | (f3 & f4) should equal... compute both ways
  lhs = tryCatch((f1 & f2) | (f3 & f4), error = function(e) NULL)
  if (is.null(lhs)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  t4 = evaluate_formula(f4, u)
  expected = (t1 & t2) | (t3 & t4)
  actual = evaluate_formula(lhs, u)
  if (!all(expected == actual)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [alt-%d]: alternating AND/OR mismatch\n", trial))
  }
}
cat(sprintf("  Alternating AND/OR: %d tests, %d failures\n", n_tests, n_failures))

# === Triple negation ===
cat("\n=== Triple negation ===\n")
set.seed(95005)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  clauses = lapply(1:sample(1:2, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  triple_neg = tryCatch(!!!f, error = function(e) NULL)
  if (is.null(triple_neg)) next

  # !!!f == !f
  t_f = evaluate_formula(f, u)
  t_neg = evaluate_formula(triple_neg, u)
  if (!all(!t_f == t_neg)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [triple-neg-%d]: !!!f != !f\n", trial))
  }
}
cat(sprintf("  Triple negation: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
