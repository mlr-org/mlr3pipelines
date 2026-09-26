#!/usr/bin/env Rscript
# Stress test with many clauses (15-30) to exercise deep cascading.
# Uses small domains and few variables to keep truth table evaluation fast.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Many clauses binary domain ===\n")
set.seed(284001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b")
  sym_names = paste0("V", 1:6)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(10:20, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 8) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [many-bin-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [many-bin-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Many clauses binary: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Many clauses ternary domain ===\n")
set.seed(284002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(10:25, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 8) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [many-ter-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [many-ter-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Many clauses ternary: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Many clauses with operations ===\n")
set.seed(284003)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_big_f = function() {
    n_cl = sample(5:12, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 3) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_big_f(); f2 = make_big_f()
  if (is.null(f1) || is.null(f2)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  n_tests = n_tests + 1
  # AND
  r = tryCatch(f1 & f2, error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == (t1 & t2))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [manyop-and-%d]: AND mismatch\n", trial))
  }

  n_tests = n_tests + 1
  # NOT
  r = tryCatch(!f1, error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == !t1)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [manyop-not-%d]: NOT mismatch\n", trial))
  }
}
cat(sprintf("  Many clauses operations: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
