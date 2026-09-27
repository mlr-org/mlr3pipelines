#!/usr/bin/env Rscript
# Test |.CnfFormula with varying sizes to stress the distribution logic.
# The cross-product can create many clauses.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== OR distribution scale tests ===\n")
set.seed(78001)

# Test: (f1 with N1 clauses) | (f2 with N2 clauses)
# Result has up to N1 * N2 clauses (before simplification)
for (trial in 1:300) {
  u = CnfUniverse()
  n_vars = sample(2:3, 1)
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # f1: 2-5 clauses
  n_cl1 = sample(2:5, 1)
  clauses1 = lapply(1:n_cl1, function(j) {
    chosen = sample(names(syms), sample(1:min(n_vars, 2), 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f1 = tryCatch(CnfFormula(clauses1), error = function(e) NULL)
  if (is.null(f1)) next

  # f2: 2-5 clauses
  n_cl2 = sample(2:5, 1)
  clauses2 = lapply(1:n_cl2, function(j) {
    chosen = sample(names(syms), sample(1:min(n_vars, 2), 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f2 = tryCatch(CnfFormula(clauses2), error = function(e) NULL)
  if (is.null(f2)) next

  # f1 | f2
  f_or = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(f_or, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [or-scale-%d]: %s\n", trial, f_or$message)); next
  }

  # Evaluate all three
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t_or = evaluate_formula(f_or, u)

  n_tests = n_tests + 1
  expected = t1 | t2
  if (!all(t_or == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [or-scale-%d]: f1 | f2 semantics wrong\n", trial))
  }
}
cat(sprintf("  OR distribution scale: %d tests, %d failures\n", n_tests, n_failures))

# === AND then OR ===
cat("\n=== AND then OR ===\n")
set.seed(78002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function() {
    n_cl = sample(1:3, 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula()
  f2 = make_formula()
  f3 = make_formula()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  # (f1 & f2) | f3
  f_and = tryCatch(f1 & f2, error = function(e) NULL)
  if (is.null(f_and)) next
  f_result = tryCatch(f_and | f3, error = function(e) NULL)
  if (is.null(f_result)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  t_result = evaluate_formula(f_result, u)

  n_tests = n_tests + 1
  expected = (t1 & t2) | t3
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [and-or-%d]: (f1 & f2) | f3 semantics wrong\n", trial))
  }
}
cat(sprintf("  AND then OR: %d tests, %d failures\n", n_tests, n_failures))

# === OR then AND ===
cat("\n=== OR then AND ===\n")
set.seed(78003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function() {
    n_cl = sample(1:3, 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula()
  f2 = make_formula()
  f3 = make_formula()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  # f1 | f2 & f3 (& binds tighter... but R doesn't know that for Ops)
  # Let me be explicit: (f1 | f2) & f3
  f_or = tryCatch(f1 | f2, error = function(e) NULL)
  if (is.null(f_or)) next
  f_result = tryCatch(f_or & f3, error = function(e) NULL)
  if (is.null(f_result)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  t_result = evaluate_formula(f_result, u)

  n_tests = n_tests + 1
  expected = (t1 | t2) & t3
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [or-and-%d]: (f1 | f2) & f3 semantics wrong\n", trial))
  }
}
cat(sprintf("  OR then AND: %d tests, %d failures\n", n_tests, n_failures))

# === Complex expression trees ===
cat("\n=== Complex expression trees ===\n")
set.seed(78004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_clause = function() {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  }

  # Build: ((c1 & c2) | (c3 & c4)) & (c5 | c6)
  c1 = make_clause()
  c2 = make_clause()
  c3 = make_clause()
  c4 = make_clause()
  c5 = make_clause()
  c6 = make_clause()

  f12 = tryCatch(as.CnfFormula(c1) & as.CnfFormula(c2), error = function(e) NULL)
  f34 = tryCatch(as.CnfFormula(c3) & as.CnfFormula(c4), error = function(e) NULL)
  if (is.null(f12) || is.null(f34)) next

  f1234 = tryCatch(f12 | f34, error = function(e) NULL)
  if (is.null(f1234)) next

  f56 = tryCatch(as.CnfFormula(c5) | as.CnfFormula(c6), error = function(e) NULL)
  if (is.null(f56)) next

  f_result = tryCatch(f1234 & f56, error = function(e) NULL)
  if (is.null(f_result)) next

  # Evaluate
  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  eval_clause = function(cl) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) return(rep(TRUE, nrow(assignments)))
    if (isFALSE(cl_bare)) return(rep(FALSE, nrow(assignments)))
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    cl_truth
  }

  t_c1 = eval_clause(c1)
  t_c2 = eval_clause(c2)
  t_c3 = eval_clause(c3)
  t_c4 = eval_clause(c4)
  t_c5 = eval_clause(c5)
  t_c6 = eval_clause(c6)

  expected = ((t_c1 & t_c2) | (t_c3 & t_c4)) & (t_c5 | t_c6)
  actual = evaluate_formula(f_result, u)

  n_tests = n_tests + 1
  if (!all(expected == actual)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [complex-%d]: complex expression semantics wrong\n", trial))
  }
}
cat(sprintf("  Complex expression trees: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
