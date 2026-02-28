#!/usr/bin/env Rscript
# Test very large formulas (30-100 clauses) to stress memory and indexing:
# - Many clauses with binary vars (SAT-like)
# - Many clauses with larger domains
# - Formulas where most clauses get eliminated
# - Dense formulas where few get eliminated
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

evaluate_raw_clauses = function(clauses, universe) {
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }
  raw_truth
}

# === Large binary SAT-like (many clauses, binary vars) ===
cat("=== Large binary SAT-like ===\n")
set.seed(106001)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("0", "1")
  n_vars = sample(5:7, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # 30-60 clauses, each with 2-3 vars (3-SAT style)
  n_clauses = sample(30:60, 1)
  clauses = lapply(1:n_clauses, function(j) {
    chosen = sample(names(syms), sample(2:3, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sat-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sat-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Large SAT: %d tests, %d failures\n", n_tests, n_failures))

# === Large ternary (50+ clauses, ternary vars) ===
cat("\n=== Large ternary ===\n")
set.seed(106002)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(4:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("Y", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_clauses = sample(40:80, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(1:min(3, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [tern-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [tern-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Large ternary: %d tests, %d failures\n", n_tests, n_failures))

# === Large with units (many clauses + some units) ===
cat("\n=== Large with units ===\n")
set.seed(106003)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  n_vars = sample(4:6, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("Z", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  clauses = list()
  # 2-3 units
  n_units = sample(2:min(3, n_vars - 1), 1)
  unit_syms = sample(names(syms), n_units)
  for (s in unit_syms) {
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, 1))
  }

  # 30-50 non-unit clauses
  n_nonunit = sample(30:50, 1)
  for (j in 1:n_nonunit) {
    n_sym = sample(2:min(4, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [units-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [units-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Large with units: %d tests, %d failures\n", n_tests, n_failures))

# === 100 clauses stress test ===
cat("\n=== 100 clauses ===\n")
set.seed(106004)

for (trial in 1:50) {
  u = CnfUniverse()
  dom = c("0", "1")
  n_vars = sample(6:8, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("W", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_clauses = 100
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [100cl-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [100cl-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  100 clauses: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
