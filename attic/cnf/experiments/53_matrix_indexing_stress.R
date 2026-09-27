#!/usr/bin/env Rscript
# Stress test the meta_idx / clause_idx / available_inverse mapping
# Force situations where:
# 1. Units are created during the pairwise phase, changing the mapping
# 2. Subsumption eliminates clauses mid-pairwise
# 3. SSE modifies clauses, invalidating matrix entries
# 4. Many clauses compete for the same symbols
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

# === Unit creation during pairwise phase ===
cat("=== Unit creation during pairwise ===\n")
set.seed(53001)

# Pattern: two non-unit clauses that SSE each other down to units
# E.g., {A: {a1, a2}, B: {b1}} and {A: {a1}, B: {b1, b2}}
# SSE: first clause restricts second's A range to {a1, a2} ∩ {a1} (no change)
# But second restricts first's B range to {b1, b2} ∩ {b1} (no change)
# We need patterns where SSE actually creates units.
# {A: {a1}, B: {b1, b2}} and {A: {a1, a2}, B: {b1}}
# SSE: second restricts first's B to {b1} -> first becomes {A:{a1}, B:{b1}}... nope, still 2 symbols
# For SSE to create a unit: need a clause to lose all but one symbol
# {A: {a1}, B: {b1}} (2 symbols). If A gets restricted to {} by SSE, then B becomes unit.
# But restrict A to {} means clause becomes FALSE... not unit.
# For SSE to create unit: need to eliminate a symbol entirely (make it cover full domain)
# Wait, that's not how SSE works. SSE intersects. Let me think...
# SSE restricts the range to the intersection. If the intersection equals the full domain, that can't happen
# (intersection can only shrink). If the range becomes empty, the symbol is eliminated from the clause.
# If the clause then has 1 symbol, it becomes a unit.

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1", "2"))
  }

  # Create clauses designed to trigger SSE that eliminates symbols
  # Clause with {A:{0}, B:{0,1,2}} and clause {A:{0,1}, B:{0}}
  # SSE from second on first: restrict B to intersect({0,1,2}, {0}) = {0} (not elimination, just restriction)
  # We need more complex scenarios. Let's use random clauses but with
  # overlapping patterns that are likely to cascade.
  n_cl = sample(5:10, 1)
  clauses = list()
  for (i in 1:n_cl) {
    n_atoms = sample(2:min(4, 4), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      # Use restricted ranges to increase chance of SSE creating units
      syms[[s]] %among% sample(c("0", "1", "2"), sample(1:2, 1))
    })
    clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [unit-pairwise %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("unit-pairwise-%d", trial))
}
cat(sprintf("  Unit during pairwise: %d tests, %d failures\n", n_tests, n_failures))

# === Subsumption cascade during pairwise ===
cat("\n=== Subsumption cascade ===\n")
set.seed(53002)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  # Create a mix of "broad" clauses (many values) and "narrow" clauses (few values)
  # Broad clauses should subsume narrow clauses
  clauses = list()
  for (i in 1:sample(3:5, 1)) {
    # Narrow clauses
    chosen = sample(names(syms), sample(2:3, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), 1)
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  for (i in 1:sample(2:4, 1)) {
    # Broad clauses (supersets of narrow ones)
    chosen = sample(names(syms), sample(2:3, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(2:3, 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [subsum-cascade %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("subsum-cascade-%d", trial))
}
cat(sprintf("  Subsumption cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Many clauses sharing symbols (dense symbol_registry) ===
cat("\n=== Dense symbol registry ===\n")
set.seed(53003)

for (trial in 1:200) {
  u = CnfUniverse()
  # 3 variables, every clause uses at least 2
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1", "2", "3"))
  }

  # All clauses use at least 2 of the 3 variables (high overlap)
  n_cl = sample(6:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(2:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1", "2", "3"), sample(1:3, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [dense-sr %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("dense-sr-%d", trial))
}
cat(sprintf("  Dense symbol registry: %d tests, %d failures\n", n_tests, n_failures))

# === SSE modifies clause ranges mid-pairwise ===
cat("\n=== SSE range modification ===\n")
set.seed(53004)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:5) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }

  # Many narrow clauses that are likely to SSE each other
  n_cl = sample(8:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(2:4, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [sse-mod %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("sse-mod-%d", trial))
}
cat(sprintf("  SSE range modification: %d tests, %d failures\n", n_tests, n_failures))

# === Units mixed with long clauses ===
cat("\n=== Units + long clauses ===\n")
set.seed(53005)

for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(5:6, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1", "2"))
  }

  clauses = list()
  # 1-2 unit clauses
  for (i in 1:sample(1:2, 1)) {
    s = sample(names(syms), 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(c("0", "1", "2"), sample(1:2, 1)))
  }
  # Long clauses (4-6 symbols)
  for (i in 1:sample(3:5, 1)) {
    n_atoms = sample(4:min(n_vars, 6), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1", "2"), sample(1:2, 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  # Short clauses (2 symbols)
  for (i in 1:sample(2:3, 1)) {
    chosen = sample(names(syms), 2)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1", "2"), 1)
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [units-long %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("units-long-%d", trial))
}
cat(sprintf("  Units + long clauses: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
