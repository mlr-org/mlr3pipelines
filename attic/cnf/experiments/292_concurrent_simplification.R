#!/usr/bin/env Rscript
# Test that the same universe can be used for multiple independent formulas.
# Also test that modifying one formula doesn't affect another.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Independent formulas same universe ===\n")
set.seed(292001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(2:4, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  # Create multiple formulas from the same universe
  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)

  # Operate on some
  n_tests = n_tests + 1
  r12 = tryCatch(f1 & f2, error = function(e) NULL)
  if (!is.null(r12)) {
    if (!all(evaluate_formula(r12, u) == (t1 & t2))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [indep-%d]: f1 & f2 wrong\n", trial))
    }
  }

  # Check that f3 is still correct after operating on f1 and f2
  n_tests = n_tests + 1
  if (!all(evaluate_formula(f3, u) == t3)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [indep-stale-%d]: f3 changed after f1&f2\n", trial))
  }

  # Negate f1 and check f2 is unaffected
  n_tests = n_tests + 1
  nf1 = tryCatch(!f1, error = function(e) NULL)
  if (!is.null(nf1)) {
    if (!all(evaluate_formula(f2, u) == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [indep-neg-%d]: f2 changed after !f1\n", trial))
    }
  }
}
cat(sprintf("  Independent formulas same universe: %d tests, %d failures\n", n_tests, n_failures))

# === Repeated operations on same formula ===
cat("\n=== Repeated operations ===\n")
set.seed(292002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(2:4, 1)
  cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 2) next

  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  tf = evaluate_formula(f, u)

  # Apply multiple operations and check the original is unchanged
  for (op_trial in 1:5) {
    op = sample(3, 1)
    if (op == 1) tryCatch(!f, error = function(e) NULL)
    else if (op == 2) tryCatch(f & f, error = function(e) NULL)
    else tryCatch(f | f, error = function(e) NULL)
  }

  n_tests = n_tests + 1
  if (!all(evaluate_formula(f, u) == tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [repeat-%d]: formula changed after operations\n", trial))
  }
}
cat(sprintf("  Repeated operations: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
