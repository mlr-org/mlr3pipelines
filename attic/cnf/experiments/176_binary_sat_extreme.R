#!/usr/bin/env Rscript
# Extreme binary SAT testing: many variables, random 3-SAT clauses
# at the phase transition ratio (~4.26 clauses per variable for 3-SAT)
# This stress tests all simplification paths simultaneously
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

# === 6 variables, phase transition 3-SAT ===
cat("=== 6-var 3-SAT phase transition ===\n")
set.seed(176001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b")
  syms = list()
  n_vars = 6
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = round(4.26 * n_vars) + sample(-2:2, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), 3)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [6v-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [6v-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  6-var 3-SAT: %d tests, %d failures\n", n_tests, n_failures))

# === 7 variables ===
cat("\n=== 7-var 3-SAT ===\n")
set.seed(176002)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b")
  syms = list()
  n_vars = 7
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = round(4.26 * n_vars) + sample(-3:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), 3)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [7v-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [7v-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  7-var 3-SAT: %d tests, %d failures\n", n_tests, n_failures))

# === 8 variables (128 assignments) ===
cat("\n=== 8-var 3-SAT ===\n")
set.seed(176003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b")
  syms = list()
  n_vars = 8
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = round(4.26 * n_vars) + sample(-3:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), 3)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [8v-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [8v-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  8-var 3-SAT: %d tests, %d failures\n", n_tests, n_failures))

# === Mixed: 2-SAT + 3-SAT clauses ===
cat("\n=== Mixed 2-SAT + 3-SAT ===\n")
set.seed(176004)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b")
  syms = list()
  n_vars = sample(6:8, 1)
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(15:30, 1)
  clauses = lapply(1:n_cl, function(j) {
    k = sample(2:3, 1)  # mix of 2-SAT and 3-SAT
    chosen = sample(names(syms), k)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mix-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mix-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Mixed 2+3-SAT: %d tests, %d failures\n", n_tests, n_failures))

# === 3-valued SAT (domain 3) ===
cat("\n=== 3-valued SAT ===\n")
set.seed(176005)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  n_vars = sample(4:5, 1)
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(8:20, 1)
  clauses = lapply(1:n_cl, function(j) {
    k = sample(2:3, 1)
    chosen = sample(names(syms), k)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [3val-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3val-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  3-valued SAT: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
