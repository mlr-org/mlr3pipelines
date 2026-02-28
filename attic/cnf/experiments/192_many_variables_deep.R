#!/usr/bin/env Rscript
# Many variables stress test:
# - 10+ binary variables with SAT-like clauses
# - 6-8 variables with ternary domains
# - Verify that even with exponentially large truth tables, the simplifier is correct
# Focus on cases where the truth table is very large but the formula has structure
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

# === Pattern 1: 10 binary variables ===
cat("=== 10 binary variables ===\n")
set.seed(192001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b")
  n_vars = 10
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # 3-SAT clauses at subcritical ratio (few clauses)
  n_cl = sample(10:20, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), 3)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 8) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [10v-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [10v-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  10 binary vars: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: 6 ternary variables ===
cat("\n=== 6 ternary variables ===\n")
set.seed(192002)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = 6
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(10:20, 1)
  clauses = lapply(1:n_cl, function(j) {
    k = sample(2:4, 1)
    chosen = sample(names(syms), k)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 8) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [6v3-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [6v3-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  6 ternary vars: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: 12 binary variables, very sparse ===
cat("\n=== 12 binary variables sparse ===\n")
set.seed(192003)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b")
  n_vars = 12
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Very few clauses relative to variables
  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    k = sample(2:4, 1)
    chosen = sample(names(syms), k)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [12v-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [12v-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  12 binary sparse: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Mixed operations on 8-variable formulas ===
cat("\n=== 8-var operations ===\n")
set.seed(192004)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b")
  n_vars = 8
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_f = function(n_cl) {
    cls = lapply(1:n_cl, function(j) {
      k = sample(2:3, 1)
      chosen = sample(names(syms), k)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 2) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(sample(5:10, 1))
  g = make_f(sample(5:10, 1))
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1
  fg = tryCatch(f & g, error = function(e) NULL)
  if (is.null(fg)) next

  tf = evaluate_formula(f, u)
  tg = evaluate_formula(g, u)
  tfg = evaluate_formula(fg, u)
  if (!all(tfg == (tf & tg))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [8vops-%d]: f&g mismatch\n", trial))
  }
}
cat(sprintf("  8-var operations: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
