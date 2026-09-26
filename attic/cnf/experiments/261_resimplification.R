#!/usr/bin/env Rscript
# Test re-simplification: take an already-simplified formula, extract its clauses,
# and re-create the formula. The result should be semantically equivalent.
# This tests idempotency and also exercises the code path where simplification
# encounters already-optimal clause structures.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Re-simplify after construction ===
cat("=== Re-simplify ===\n")
set.seed(261001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  truth_orig = evaluate_formula(f, u)

  # Extract clauses and re-simplify
  f_bare = unclass(f)
  if (is.logical(f_bare)) next  # TRUE/FALSE formulas don't have extractable clauses

  # Re-create via simplify_cnf
  f2 = tryCatch(simplify_cnf(f_bare, u), error = function(e) NULL)
  if (is.null(f2)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [resimpl-%d]: re-simplification failed\n", trial)); next
  }

  truth_resimpl = evaluate_formula(f2, u)
  if (!all(truth_orig == truth_resimpl)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [resimpl-%d]: re-simplification changed semantics\n", trial))
  }
}
cat(sprintf("  Re-simplify: %d tests, %d failures\n", n_tests, n_failures))

# === Re-simplify after operations ===
cat("\n=== Re-simplify after ops ===\n")
set.seed(261002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:3, 1)
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

  # Perform operations
  op = sample(1:4, 1)
  f = tryCatch(switch(op,
    f1 & f2,
    f1 | f2,
    !f1,
    (f1 & f2) | (!f1)
  ), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  truth_orig = evaluate_formula(f, u)

  f_bare = unclass(f)
  if (is.logical(f_bare)) next

  # Re-simplify
  f2_re = tryCatch(simplify_cnf(f_bare, u), error = function(e) NULL)
  if (is.null(f2_re)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [resimplop-%d]: re-simplification failed\n", trial)); next
  }

  truth_resimpl = evaluate_formula(f2_re, u)
  if (!all(truth_orig == truth_resimpl)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [resimplop-%d]: re-simplification changed semantics\n", trial))
  }
}
cat(sprintf("  Re-simplify after ops: %d tests, %d failures\n", n_tests, n_failures))

# === Triple simplification ===
cat("\n=== Triple simplification ===\n")
set.seed(261003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f1 = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f1)) next
  f1_bare = unclass(f1)
  if (is.logical(f1_bare)) next

  f2 = tryCatch(simplify_cnf(f1_bare, u), error = function(e) NULL)
  if (is.null(f2)) next
  f2_bare = unclass(f2)
  if (is.logical(f2_bare)) next

  f3 = tryCatch(simplify_cnf(f2_bare, u), error = function(e) NULL)
  if (is.null(f3)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)

  if (!all(t1 == t2) || !all(t2 == t3)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [triple-%d]: simplification not idempotent\n", trial))
  }
}
cat(sprintf("  Triple simplification: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
