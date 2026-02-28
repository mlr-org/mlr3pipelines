#!/usr/bin/env Rscript
# Random walk through formula space: start with a formula, apply random
# operations (AND, OR, NOT), and verify each step produces correct results.
# This tests that sequences of operations maintain consistency, exercising
# simplification on the output of previous simplifications.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Random walk ===\n")
set.seed(269001)

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

  # Start with a random formula
  f = make_f()
  if (is.null(f)) next
  t_f = evaluate_formula(f, u)

  # Take 5-10 random steps
  n_steps = sample(5:10, 1)
  all_ok = TRUE
  for (step in 1:n_steps) {
    n_tests = n_tests + 1
    op = sample(1:4, 1)

    if (op == 1) {
      # AND with random formula
      g = make_f()
      if (is.null(g)) next
      t_g = evaluate_formula(g, u)
      f_new = tryCatch(f & g, error = function(e) NULL)
      if (is.null(f_new)) next
      expected = t_f & t_g
    } else if (op == 2) {
      # OR with random formula
      g = make_f()
      if (is.null(g)) next
      t_g = evaluate_formula(g, u)
      f_new = tryCatch(f | g, error = function(e) NULL)
      if (is.null(f_new)) next
      expected = t_f | t_g
    } else if (op == 3) {
      # Negate
      f_new = tryCatch(!f, error = function(e) NULL)
      if (is.null(f_new)) next
      expected = !t_f
    } else {
      # AND with self
      f_new = tryCatch(f & f, error = function(e) NULL)
      if (is.null(f_new)) next
      expected = t_f
    }

    t_new = evaluate_formula(f_new, u)
    if (!all(t_new == expected)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [walk-%d-step%d]: op=%d mismatch\n", trial, step, op))
      all_ok = FALSE
      break
    }

    # Continue walk with new formula
    f = f_new
    t_f = t_new
  }
}
cat(sprintf("  Random walk: %d tests, %d failures\n", n_tests, n_failures))

# === Longer walks with binary domains ===
cat("\n=== Binary random walk ===\n")
set.seed(269002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    cls = list(as.CnfClause(syms[[sample(sym_names, 1)]] %among% sample(dom, 1)))
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f()
  if (is.null(f)) next
  t_f = evaluate_formula(f, u)

  n_steps = sample(8:15, 1)
  for (step in 1:n_steps) {
    n_tests = n_tests + 1
    op = sample(1:3, 1)

    if (op == 1) {
      g = make_f()
      if (is.null(g)) next
      t_g = evaluate_formula(g, u)
      f_new = tryCatch(f & g, error = function(e) NULL)
      if (is.null(f_new)) next
      expected = t_f & t_g
    } else if (op == 2) {
      g = make_f()
      if (is.null(g)) next
      t_g = evaluate_formula(g, u)
      f_new = tryCatch(f | g, error = function(e) NULL)
      if (is.null(f_new)) next
      expected = t_f | t_g
    } else {
      f_new = tryCatch(!f, error = function(e) NULL)
      if (is.null(f_new)) next
      expected = !t_f
    }

    t_new = evaluate_formula(f_new, u)
    if (!all(t_new == expected)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [binwalk-%d-step%d]: op=%d mismatch\n", trial, step, op))
      break
    }
    f = f_new
    t_f = t_new
  }
}
cat(sprintf("  Binary random walk: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
