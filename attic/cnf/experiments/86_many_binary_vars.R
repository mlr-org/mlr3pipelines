#!/usr/bin/env Rscript
# Test with many binary variables to stress the matrix operations.
# 8-10 binary variables, various clause patterns.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

check_fc = function(formula, raw_clauses, universe, label) {
  n_tests <<- n_tests + 1
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in raw_clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }

  simplified_truth = evaluate_formula(formula, universe)
  mismatches = which(raw_truth != simplified_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d (raw=%s, simp=%s)\n",
      label, idx, raw_truth[idx], simplified_truth[idx]))
    return(FALSE)
  }
  TRUE
}

# === 8 binary variables, 3-SAT-like ===
cat("=== 8 binary vars, 3-SAT ===\n")
set.seed(86001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("0", "1")
  syms = list()
  for (v in 1:8) {
    vname = paste0("X", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # 3-SAT: each clause has exactly 3 variables, each with one value
  n_cl = sample(5:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), 3)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [3sat-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("3sat-%d", trial))
}
cat(sprintf("  8-var 3-SAT: %d tests, %d failures\n", n_tests, n_failures))

# === 10 binary variables, mixed clause sizes ===
cat("\n=== 10 binary vars, mixed clauses ===\n")
set.seed(86002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1")
  syms = list()
  for (v in 1:10) {
    vname = paste0("X", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(5:20, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:5, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [10var-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("10var-%d", trial))
}
cat(sprintf("  10-var mixed: %d tests, %d failures\n", n_tests, n_failures))

# === 8 binary vars with many units ===
cat("\n=== 8 binary vars, many units ===\n")
set.seed(86003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1")
  syms = list()
  for (v in 1:8) {
    vname = paste0("X", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Start with 3-5 units, then add more clauses
  n_units = sample(3:5, 1)
  unit_syms = sample(names(syms), n_units)
  unit_clauses = lapply(unit_syms, function(s) {
    as.CnfClause(syms[[s]] %among% sample(dom, 1))
  })

  n_other = sample(3:8, 1)
  other_clauses = lapply(1:n_other, function(j) {
    chosen = sample(names(syms), sample(2:4, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })

  clauses = c(unit_clauses, other_clauses)
  # shuffle
  clauses = clauses[sample(length(clauses))]

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [unit8-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("unit8-%d", trial))
}
cat(sprintf("  8-var units: %d tests, %d failures\n", n_tests, n_failures))

# === Dense: near phase transition (clause/var ratio ~4.2 for 3-SAT) ===
cat("\n=== Dense 3-SAT near phase transition ===\n")
set.seed(86004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1")
  n_vars = sample(7:9, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("X", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Near phase transition: ~4.2 clauses per variable
  n_cl = round(n_vars * runif(1, 3.5, 5.0))
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), 3)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [dense-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("dense-%d", trial))
}
cat(sprintf("  Dense 3-SAT: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
