#!/usr/bin/env Rscript
# Test the HLA unit phase specifically:
# - delayedAssign("roe_inverse", ...) path
# - Non-unit HLA eliminates clauses, then unit HLA runs with updated remaining_nonunit_entries
# - Hidden tautology via unit HLA
# - Hidden subsumption via unit HLA
# - roe_inverse matching correctness
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

# === Constructed: unit + clauses that can be hidden-tautology-eliminated ===
cat("=== Unit HLA: hidden tautology ===\n")
set.seed(102001)

# Pattern: Unit for A. Clauses that contain A and are "almost tautological" for another symbol
# After HLA extends the clause's other symbol to its full domain, it becomes tautological.
for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  a_val = sample(dom, 1)
  b_vals = sample(dom, 2)  # 2 of 3 values
  c_vals = sample(dom, 2)

  # Unit: A = a_val
  # Clause 1: B %among% b_vals | C %among% c_vals  (non-unit, 2 symbols)
  # Clause 2: A %among% c(a_val, other) | B %among% setdiff(dom, b_vals) -- unit HLA should reveal hidden tautology
  # (Unit A restricts clause 2's A to a_val, which is already a unit. Then HLA: B union complement-of-A's-restriction-in-clause2.)
  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(B %among% b_vals | C %among% c_vals),
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | B %among% sample(dom, sample(1:2, 1)))
  )

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [unithla-taut-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unithla-taut-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Unit HLA hidden tautology: %d tests, %d failures\n", n_tests, n_failures))

# === Mixed: non-unit HLA + unit HLA in same formula ===
cat("\n=== Mixed non-unit + unit HLA ===\n")
set.seed(102002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom_size = sample(3:5, 1)
  d = paste0("v", 1:dom_size)
  n_vars = sample(3:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  # Mix of units and non-units to exercise both HLA phases
  clauses = list()
  # 1-2 units
  n_units = sample(1:min(2, n_vars - 1), 1)
  unit_syms = sample(names(syms), n_units)
  for (s in unit_syms) {
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(d, 1))
  }

  # 3-5 non-unit clauses with overlapping symbols
  n_nonunit = sample(3:5, 1)
  for (j in 1:n_nonunit) {
    n_sym = sample(2:min(3, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      n_vals = sample(1:(dom_size - 1), 1)
      syms[[s]] %among% sample(d, n_vals)
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mixed-hla-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixed-hla-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Mixed HLA: %d tests, %d failures\n", n_tests, n_failures))

# === Stress: many units + many clauses to exercise roe_inverse deeply ===
cat("\n=== roe_inverse stress ===\n")
set.seed(102003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  n_vars = sample(5:7, 1)
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

  # Many non-unit clauses (5-10) to stress the roe_inverse matching
  n_nonunit = sample(5:10, 1)
  for (j in 1:n_nonunit) {
    n_sym = sample(2:min(4, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      n_vals = sample(1:(length(dom) - 1), 1)
      syms[[s]] %among% sample(dom, n_vals)
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [roeinv-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [roeinv-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  roe_inverse stress: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
