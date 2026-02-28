#!/usr/bin/env Rscript
# Test HLA unit phase specifically (lines 724-778 of simplify_cnf)
# Key patterns:
# - Unit clause hidden tautology elimination via multiple 2-symbol clauses
# - Unit clause hidden subsumption elimination
# - roe_inverse lazy evaluation (delayedAssign) being triggered
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

# === HLA unit hidden tautology: multiple 2-symbol clauses complement a unit ===
cat("=== HLA unit hidden tautology ===\n")
set.seed(50001)

# Pattern: unit U = {A: {a1}}, clauses D1 = {A: {a1}, B: {b1}}, D2 = {A: {a1}, B: {b2}}
# After unit propagation, D1 and D2 are unchanged (their A range is already {a1})
# HLA on U with D1: adds complement of D1[B] = {b2} to U -> U gets hidden B: {b2}
# HLA on U with D2: adds complement of D2[B] = {b1} to U -> U gets hidden B: {b1, b2} = full domain -> hidden taut!
for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2"))

  unit_a = as.CnfClause(A %among% "a1")
  d1 = as.CnfClause(A %among% "a1" | B %among% "b1")
  d2 = as.CnfClause(A %among% "a1" | B %among% "b2")
  # extra random clause to keep it interesting
  vals_a = sample(c("a1", "a2", "a3"), sample(1:2, 1))
  vals_b = sample(c("b1", "b2"), 1)
  d3 = as.CnfClause(A %among% vals_a | B %among% vals_b)

  clauses = list(unit_a, d1, d2, d3)
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [hla-unit-taut %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("hla-unit-taut-%d", trial))
}
cat(sprintf("  HLA unit hidden taut: %d tests, %d failures\n", n_tests, n_failures))

# === HLA unit hidden subsumption ===
cat("\n=== HLA unit hidden subsumption ===\n")
set.seed(50002)

for (trial in 1:100) {
  u = CnfUniverse()
  n_vars = sample(3:4, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1", "2"))
  }

  # Create a unit clause
  unit_sym = sample(names(syms), 1)
  unit_val = sample(c("0", "1", "2"), sample(1:2, 1))
  clauses = list(as.CnfClause(syms[[unit_sym]] %among% unit_val))

  # Create 2-symbol clauses that share the unit symbol (for HLA)
  n_extra = sample(3:6, 1)
  for (i in 1:n_extra) {
    other_sym = sample(setdiff(names(syms), unit_sym), 1)
    unit_val_in_clause = sample(unit_val, min(length(unit_val), sample(1:2, 1)))
    other_val = sample(c("0", "1", "2"), sample(1:2, 1))
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[unit_sym]] %among% unit_val_in_clause | syms[[other_sym]] %among% other_val
    )
  }

  # Add a few random clauses too
  for (i in 1:sample(1:3, 1)) {
    n_atoms = sample(2:min(n_vars, 3), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1", "2"), sample(1:2, 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [hla-unit-sub %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("hla-unit-sub-%d", trial))
}
cat(sprintf("  HLA unit hidden sub: %d tests, %d failures\n", n_tests, n_failures))

# === HLA with roe_inverse triggering ===
cat("\n=== HLA roe_inverse exercise ===\n")
set.seed(50003)

# Force patterns where symbol_registry lookups during HLA unit phase
# trigger the delayedAssign for roe_inverse
for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1", "2"))
  }

  # Multiple units to exercise the unit HLA phase
  n_units = sample(1:2, 1)
  clauses = list()
  unit_syms = sample(names(syms), n_units)
  for (us in unit_syms) {
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[us]] %among% sample(c("0", "1", "2"), sample(1:2, 1))
    )
  }

  # Many non-unit clauses involving unit symbols and other symbols
  n_nonunit = sample(4:8, 1)
  for (i in 1:n_nonunit) {
    n_atoms = sample(2:min(4, 4), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1", "2"), sample(1:2, 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [roe-inv %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("roe-inv-%d", trial))
}
cat(sprintf("  roe_inverse exercise: %d tests, %d failures\n", n_tests, n_failures))

# === Randomized HLA unit stress (many units + varied non-units) ===
cat("\n=== HLA unit stress randomized ===\n")
set.seed(50004)

for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(3:5, 1)
  dom_size = sample(2:4, 1)
  dom = paste0("d", 1:dom_size)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Many units (up to half the variables)
  n_units = sample(1:min(3, n_vars), 1)
  clauses = list()
  unit_syms = sample(names(syms), n_units)
  for (us in unit_syms) {
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[us]] %among% sample(dom, sample(1:(dom_size-1), 1))
    )
  }

  # Non-units of varying sizes
  n_nonunit = sample(4:10, 1)
  for (i in 1:n_nonunit) {
    n_atoms = sample(2:min(n_vars, 4), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:max(1, dom_size-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [hla-stress %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("hla-stress-%d", trial))
}
cat(sprintf("  HLA unit stress: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
