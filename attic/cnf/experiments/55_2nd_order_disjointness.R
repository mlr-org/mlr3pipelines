#!/usr/bin/env Rscript
# Test the 2nd-order SSE disjointness check in try_sse_2nd_order (line 456-458)
# The condition: clause_oneend and clause_twoends must be disjoint on symbol_intersect
# outside of clause_target's range.
# Also tests handle_sse_2nd_order_twoend's double loop (lines 409-438)
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

# === Pattern forcing 2nd-order SSE with known disjointness ===
cat("=== 2nd-order SSE disjointness patterns ===\n")
set.seed(55001)

# Classic resolution pattern:
# target = {A: {a1,a2}, B: {b1}}
# oneend = {A: {a1}, C: {c1}}  -- not subset of target on C (missing), subset on A
# twoend = {A: {a2}, B: {b2}, C: {c2}}  -- not subset on B, not subset on C
# 2nd-order SSE check: oneend.A ∩ twoend.A = {a1} ∩ {a2} = {} (disjoint outside target.A)
# If disjoint: restrict target.B to union(oneend.B [missing=full], twoend.B {b2})
# This pattern needs careful setup.

for (trial in 1:200) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2", "b3"))
  C = CnfSymbol(u, "C", c("c1", "c2", "c3"))

  # Create clauses where 2nd-order SSE patterns are likely
  n_cl = sample(4:8, 1)
  clauses = list()
  for (i in 1:n_cl) {
    n_atoms = sample(2:3, 1)
    chosen = sample(c("A", "B", "C"), n_atoms)
    doms = list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3"), C = c("c1", "c2", "c3"))
    atoms = lapply(chosen, function(s) {
      sym = switch(s, A = A, B = B, C = C)
      sym %among% sample(doms[[s]], sample(1:2, 1))
    })
    clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [disjoint %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("disjoint-%d", trial))
}
cat(sprintf("  Disjointness patterns: %d tests, %d failures\n", n_tests, n_failures))

# === Explicit resolution/2nd-order patterns ===
cat("\n=== Explicit 2nd-order SSE patterns ===\n")
set.seed(55002)

for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(3:4, 1)
  dom_size = sample(3:5, 1)
  dom = paste0("d", 1:dom_size)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create a "target" clause with 2+ symbols
  target_syms = sample(names(syms), sample(2:min(3, n_vars), 1))
  target_atoms = lapply(target_syms, function(s) {
    syms[[s]] %among% sample(dom, sample(1:max(1, dom_size-1), 1))
  })
  clauses = list(as.CnfClause(Reduce(`|`, target_atoms)))

  # Create "oneend" clauses (subset of target on all symbols except one they don't share)
  for (i in 1:sample(2:4, 1)) {
    n_atoms = sample(2:min(3, n_vars), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:max(1, dom_size-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  # Create "twoend" clauses (subset of target on all but two symbols)
  for (i in 1:sample(1:3, 1)) {
    n_atoms = sample(2:min(n_vars, 3), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:max(1, dom_size-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [explicit-2nd %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("explicit-2nd-%d", trial))
}
cat(sprintf("  Explicit 2nd-order: %d tests, %d failures\n", n_tests, n_failures))

# === Many variables small domains (forces many pairwise comparisons) ===
cat("\n=== Many vars, small domains ===\n")
set.seed(55003)

for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(6:8, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }

  n_cl = sample(6:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(2:min(4, n_vars), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [many-vars %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("many-vars-%d", trial))
}
cat(sprintf("  Many vars small domains: %d tests, %d failures\n", n_tests, n_failures))

# === Forced cascading during pairwise (meta_idx > meta_idx_outer path) ===
cat("\n=== Cascading during pairwise ===\n")
set.seed(55004)

# This targets lines 169 and 254: `if (meta_idx > meta_idx_outer) return(FALSE)`
# We need clauses where unit propagation during the pairwise phase affects
# clauses that haven't been processed yet.
for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1", "2"))
  }

  # Mix units with non-units to force cascading during pairwise
  clauses = list()
  # A unit that will propagate
  clauses[[1]] = as.CnfClause(syms[["V1"]] %among% sample(c("0", "1", "2"), sample(1:2, 1)))

  # Non-units that share V1 and other variables
  for (i in 1:sample(4:8, 1)) {
    n_atoms = sample(2:4, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1", "2"), sample(1:2, 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [cascade-pw %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("cascade-pw-%d", trial))
}
cat(sprintf("  Cascading during pairwise: %d tests, %d failures\n", n_tests, n_failures))

# === Randomized 2nd-order SSE with domain 4-5 ===
cat("\n=== Randomized 2nd-order SSE large domains ===\n")
set.seed(55005)

for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(3:4, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    dom_size = sample(3:5, 1)
    syms[[vname]] = CnfSymbol(u, vname, paste0("d", 1:dom_size))
  }

  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(2:min(n_vars, 3), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [2nd-large %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("2nd-large-%d", trial))
}
cat(sprintf("  2nd-order large domains: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
