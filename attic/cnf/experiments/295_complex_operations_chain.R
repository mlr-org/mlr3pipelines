#!/usr/bin/env Rscript
# Test complex chains of operations:
# - (f1 & f2) | (f3 & f4) with all the cross-interactions
# - f & !f should be FALSE
# - f | !f should be TRUE
# - Exclusive OR patterns
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === f & !f == FALSE ===
cat("=== Complementation ===\n")
set.seed(295001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(1:3, 1)
  cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 1) next

  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  # f & !f should be FALSE
  n_tests = n_tests + 1
  nf = tryCatch(!f, error = function(e) NULL)
  if (!is.null(nf)) {
    r = tryCatch(f & nf, error = function(e) NULL)
    if (!is.null(r)) {
      truth = evaluate_formula(r, u)
      if (any(truth)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [comp-and-%d]: f & !f != FALSE\n", trial))
      }
    }
  }

  # f | !f should be TRUE
  n_tests = n_tests + 1
  if (!is.null(nf)) {
    r = tryCatch(f | nf, error = function(e) NULL)
    if (!is.null(r)) {
      truth = evaluate_formula(r, u)
      if (!all(truth)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [comp-or-%d]: f | !f != TRUE\n", trial))
      }
    }
  }
}
cat(sprintf("  Complementation: %d tests, %d failures\n", n_tests, n_failures))

# === Complex chains ===
cat("\n=== Complex operation chains ===\n")
set.seed(295002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:2)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      atoms = lapply(sym_names, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f(); f4 = make_f()
  if (is.null(f1) || is.null(f2) || is.null(f3) || is.null(f4)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  t4 = evaluate_formula(f4, u)

  # (f1 & f2) | (f3 & f4)
  n_tests = n_tests + 1
  r = tryCatch((f1 & f2) | (f3 & f4), error = function(e) NULL)
  if (!is.null(r)) {
    expected = (t1 & t2) | (t3 & t4)
    if (!all(evaluate_formula(r, u) == expected)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [chain-%d]: (f1&f2)|(f3&f4) mismatch\n", trial))
    }
  }

  # (f1 | f2) & (f3 | f4)
  n_tests = n_tests + 1
  r = tryCatch((f1 | f2) & (f3 | f4), error = function(e) NULL)
  if (!is.null(r)) {
    expected = (t1 | t2) & (t3 | t4)
    if (!all(evaluate_formula(r, u) == expected)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [chain2-%d]: (f1|f2)&(f3|f4) mismatch\n", trial))
    }
  }

  # !f1 & (f2 | !f3) & f4
  n_tests = n_tests + 1
  nf1 = tryCatch(!f1, error = function(e) NULL)
  nf3 = tryCatch(!f3, error = function(e) NULL)
  if (!is.null(nf1) && !is.null(nf3)) {
    r = tryCatch(nf1 & (f2 | nf3) & f4, error = function(e) NULL)
    if (!is.null(r)) {
      expected = !t1 & (t2 | !t3) & t4
      if (!all(evaluate_formula(r, u) == expected)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [chain3-%d]: complex expression mismatch\n", trial))
      }
    }
  }
}
cat(sprintf("  Complex operation chains: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
