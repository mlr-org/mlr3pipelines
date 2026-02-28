#!/usr/bin/env Rscript
# Final torture test: combine the hardest patterns.
# Each test generates a random formula, performs multiple operations,
# and verifies semantic correctness at every step.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Multi-operation gauntlet ===
cat("=== Multi-operation gauntlet ===\n")
set.seed(96001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create two random formulas
  make_formula = function() {
    n_cl = sample(1:3, 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:3, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula()
  f2 = make_formula()
  if (is.null(f1) || is.null(f2)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  # Pick a random operation
  op = sample(1:7, 1)
  result = switch(op,
    # 1: f1 & f2
    tryCatch(f1 & f2, error = function(e) NULL),
    # 2: f1 | f2
    tryCatch(f1 | f2, error = function(e) NULL),
    # 3: !f1
    tryCatch(!f1, error = function(e) NULL),
    # 4: !(f1 & f2)
    tryCatch(!(f1 & f2), error = function(e) NULL),
    # 5: !f1 | !f2  (De Morgan of 4)
    tryCatch((!f1) | (!f2), error = function(e) NULL),
    # 6: f1 & !f1 (should be FALSE)
    tryCatch(f1 & (!f1), error = function(e) NULL),
    # 7: f1 | !f1 (should be TRUE)
    tryCatch(f1 | (!f1), error = function(e) NULL)
  )
  if (is.null(result)) next

  n_tests = n_tests + 1
  actual = evaluate_formula(result, u)
  expected = switch(op,
    t1 & t2,
    t1 | t2,
    !t1,
    !(t1 & t2),
    (!t1) | (!t2),
    rep(FALSE, length(t1)),
    rep(TRUE, length(t1))
  )
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [gauntlet-%d-op%d]: mismatch\n", trial, op))
  }
}
cat(sprintf("  Gauntlet: %d tests, %d failures\n", n_tests, n_failures))

# === Absorption law stress test: f & (f | g) == f ===
cat("\n=== Absorption law ===\n")
set.seed(96002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function() {
    n_cl = sample(1:2, 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f = make_formula()
  g = make_formula()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1

  # f & (f | g) should be semantically equal to f
  fog = tryCatch(f | g, error = function(e) NULL)
  if (is.null(fog)) next
  result = tryCatch(f & fog, error = function(e) NULL)
  if (is.null(result)) next

  t_f = evaluate_formula(f, u)
  t_result = evaluate_formula(result, u)
  if (!all(t_f == t_result)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [absorb-%d]: f & (f | g) != f\n", trial))
  }
}
cat(sprintf("  Absorption: %d tests, %d failures\n", n_tests, n_failures))

# === Consensus theorem: (f & g) | (f & !g) == f ===
cat("\n=== Consensus theorem ===\n")
set.seed(96003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function() {
    clauses = list(as.CnfClause(Reduce(`|`, {
      chosen = sample(names(syms), sample(1:2, 1))
      lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    })))
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f = make_formula()
  g = make_formula()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1

  # (f & g) | (f & !g) == f
  neg_g = tryCatch(!g, error = function(e) NULL)
  if (is.null(neg_g)) next

  fg = tryCatch(f & g, error = function(e) NULL)
  fng = tryCatch(f & neg_g, error = function(e) NULL)
  if (is.null(fg) || is.null(fng)) next

  result = tryCatch(fg | fng, error = function(e) NULL)
  if (is.null(result)) next

  t_f = evaluate_formula(f, u)
  t_result = evaluate_formula(result, u)
  if (!all(t_f == t_result)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [consensus-%d]: (f & g) | (f & !g) != f\n", trial))
  }
}
cat(sprintf("  Consensus: %d tests, %d failures\n", n_tests, n_failures))

# === XOR via (f & !g) | (!f & g) ===
cat("\n=== XOR expression ===\n")
set.seed(96004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function() {
    clauses = list(as.CnfClause(Reduce(`|`, {
      chosen = sample(names(syms), sample(1:2, 1))
      lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    })))
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f = make_formula()
  g = make_formula()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1

  xor_result = tryCatch({
    (f & (!g)) | ((!f) & g)
  }, error = function(e) NULL)
  if (is.null(xor_result)) next

  t_f = evaluate_formula(f, u)
  t_g = evaluate_formula(g, u)
  expected = xor(t_f, t_g)
  actual = evaluate_formula(xor_result, u)
  if (!all(expected == actual)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [xor-%d]: XOR mismatch\n", trial))
  }
}
cat(sprintf("  XOR: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
