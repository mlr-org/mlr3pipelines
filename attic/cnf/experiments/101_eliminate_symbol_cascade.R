#!/usr/bin/env Rscript
# Test eliminate_symbol_from_clause cascading effects:
# - Unit propagation that creates new units from clauses not yet in pairwise loop
# - meta_idx > meta_idx_outer early return path
# - Symbol elimination that cascades through multiple clauses
# - Dense symbol overlap patterns
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# Evaluate a list of raw CnfClause objects against all assignments
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

# === Constructed cascade: unit eliminates symbol, creating new unit, which cascades ===
cat("=== Constructed cascade chains ===\n")
set.seed(101001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("0", "1", "2")

  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  # Unit: A %among% "0"
  # Clause: A %among% c("0","1") | B %among% "0"  -- A gets restricted to "0", then removed, B becomes unit
  # Clause: B %among% c("0","1") | C %among% "0"  -- B gets restricted, C becomes unit
  # Clause: C %among% c("0","1") | D %among% sample(dom, sample(1:2, 1))

  d_vals = sample(dom, sample(1:2, 1))
  clauses = list(
    as.CnfClause(A %among% "0"),
    as.CnfClause(A %among% c("0", "1") | B %among% "0"),
    as.CnfClause(B %among% c("0", "1") | C %among% "0"),
    as.CnfClause(C %among% c("0", "1") | D %among% d_vals)
  )

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cascade-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cascade-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Cascade chains: %d tests, %d failures\n", n_tests, n_failures))

# === Dense symbol sharing: all clauses share many symbols ===
cat("\n=== Dense symbol sharing ===\n")
set.seed(101002)

for (trial in 1:500) {
  u = CnfUniverse()
  n_vars = sample(3:5, 1)
  dom_size = sample(2:4, 1)
  d = paste0("v", 1:dom_size)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  # Create clauses where every clause uses ALL or nearly ALL symbols
  n_clauses = sample(3:6, 1)
  clauses = list()
  for (j in 1:n_clauses) {
    n_syms_in_clause = sample(max(2, n_vars - 1):n_vars, 1)
    chosen = sample(names(syms), n_syms_in_clause)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(d, sample(1:(dom_size - 1), 1)))
    clauses[[j]] = as.CnfClause(Reduce(`|`, atoms))
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dense-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dense-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Dense sharing: %d tests, %d failures\n", n_tests, n_failures))

# === Long chains: unit at one end cascades through many clauses ===
cat("\n=== Long cascade chains ===\n")
set.seed(101003)

for (trial in 1:200) {
  u = CnfUniverse()
  chain_len = sample(4:8, 1)
  dom = c("0", "1", "2")

  syms = list()
  for (i in 1:(chain_len + 1)) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  clauses = list()
  # Unit at the start
  clauses[[1]] = as.CnfClause(syms[["V1"]] %among% "0")

  # Chain: each clause links consecutive symbols
  for (i in 1:chain_len) {
    v_cur = paste0("V", i)
    v_next = paste0("V", i + 1)
    cur_vals = sample(dom, 2)  # 2 out of 3 values
    if (!"0" %in% cur_vals) cur_vals[1] = "0"  # ensure unit propagation works
    next_vals = sample(dom, sample(1:2, 1))
    clauses[[i + 1]] = as.CnfClause(syms[[v_cur]] %among% cur_vals | syms[[v_next]] %among% next_vals)
  }

  # Add a final clause with the last symbol
  last_v = paste0("V", chain_len + 1)
  clauses[[length(clauses) + 1]] = as.CnfClause(syms[[last_v]] %among% sample(dom, sample(1:2, 1)))

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [longchain-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [longchain-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Long chains: %d tests, %d failures\n", n_tests, n_failures))

# === Multiple units creating cascading eliminations ===
cat("\n=== Multi-unit cascade ===\n")
set.seed(101004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  n_vars = sample(4:6, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("Y", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  clauses = list()
  # Multiple units
  n_units = sample(2:min(3, n_vars), 1)
  unit_syms = sample(names(syms), n_units)
  for (s in unit_syms) {
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, 1))
  }

  # Non-unit clauses that reference unit symbols + others
  n_nonunit = sample(3:6, 1)
  for (j in 1:n_nonunit) {
    n_in_clause = sample(2:min(4, n_vars), 1)
    chosen = sample(names(syms), n_in_clause)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multiunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multiunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multi-unit cascade: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
