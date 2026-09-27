#!/usr/bin/env Rscript
# Heavy OR distribution testing:
# The | operator distributes clauses, creating potentially large intermediate
# formulas. Test with various sizes to stress the distribution and simplification.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: Multi-clause OR multi-clause ===
cat("=== Multi-clause OR multi-clause ===\n")
set.seed(242001)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function(n_cl_range = c(2, 4)) {
    n_cl = sample(n_cl_range[1]:n_cl_range[2], 1)
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

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mcor-%d]: %s\n", trial, result$message)); next
  }
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 | t2
  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mcor-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Multi-clause OR: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: OR that produces many tautological clauses ===
# When e1_clause + e2_clause covers full domain for some symbol
cat("\n=== OR with tautology elimination ===\n")
set.seed(242002)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b")  # small domain so tautologies are common
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(2:4, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ortaut-%d]: %s\n", trial, result$message)); next
  }
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 | t2
  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ortaut-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  OR tautology: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Chained OR operations ===
cat("\n=== Chained OR ===\n")
set.seed(242003)

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

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (any(sapply(list(f1, f2, f3), is.null))) next

  n_tests = n_tests + 1
  result = tryCatch((f1 | f2) | f3, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d]: %s\n", trial, result$message)); next
  }
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  expected = t1 | t2 | t3
  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Chained OR: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: OR with swap path (length(e1) > length(e2)) ===
cat("\n=== OR swap path ===\n")
set.seed(242004)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # f1 has more clauses than f2
  make_f = function(n_cl_range) {
    n_cl = sample(n_cl_range[1]:n_cl_range[2], 1)
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

  f_long = make_f(c(3, 5))
  f_short = make_f(c(1, 2))
  if (is.null(f_long) || is.null(f_short)) next

  n_tests = n_tests + 1

  # f_long | f_short (triggers swap since length(e1) > length(e2))
  result1 = tryCatch(f_long | f_short, error = function(e) e)
  # f_short | f_long (no swap)
  result2 = tryCatch(f_short | f_long, error = function(e) e)

  if (inherits(result1, "error") || inherits(result2, "error")) {
    n_failures = n_failures + 1
    msg = if (inherits(result1, "error")) result1$message else result2$message
    cat(sprintf("ERROR [swap-%d]: %s\n", trial, msg)); next
  }

  tl = evaluate_formula(f_long, u)
  ts = evaluate_formula(f_short, u)
  expected = tl | ts

  actual1 = evaluate_formula(result1, u)
  actual2 = evaluate_formula(result2, u)

  if (!all(actual1 == expected) || !all(actual2 == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [swap-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  OR swap: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
