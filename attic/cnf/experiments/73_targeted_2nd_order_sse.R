#!/usr/bin/env Rscript
# Specifically construct formulas that should trigger 2nd-order SSE.
# 2nd-order SSE requires:
#   - clause A is subset of target T on all symbols except one (s1)
#   - clause B is subset of target T on all symbols except two (s1 and s2)
#   - A and B are disjoint on s1 outside of T
#   - Then T's range on s2 can be restricted to union of A[s2] and B[s2]
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

# === Manually constructed 2nd-order SSE scenarios ===
cat("=== Constructed 2nd-order SSE ===\n")

# Scenario: 3 variables, domain {a, b, c}
# Clause A: V1 in {a} | V2 in {a, b}        -- subset of T on V2, not on V1
# Clause B: V1 in {b} | V2 in {a, b} | V3 in {a}  -- subset of T on V2, not on V1 or V3
# Target T: V2 in {a, b, c} | V3 in {a, b}
# A and B disjoint on V1 outside T (A has {a}, B has {b}, T doesn't have V1)
# So T[V3] can be restricted to union of A[V3] and B[V3]
# A[V3] = universe (not present), B[V3] = {a}
# This is NOT the right scenario since A doesn't mention V3...

# Let me think more carefully:
# For oneend: A covers T on all except s1. B covers T on all except s1 and s2.
# A and B disjoint on s1 outside T.
# Then T[s2] = union(A[s2], B[s2]).

# Simpler scenario with 2 symbols:
# A: V1 in {a}  (unit)
# B: V1 in {b} | V2 in {a}  (2-symbol clause)
# T: V2 in {a, b}
# A covers T on all except V1 (A doesn't mention V2 -> any V2 works)
# Wait, "cover" means subset relationship. If A = {V1: {a}}, and T = {V2: {a,b}},
# then we need A's ranges to be subsets of T's ranges for shared symbols.
# Since A only has V1, and T only has V2, there are NO shared symbols except
# potentially through the is_not_subset_of matrix logic.

# Let me re-read the actual check...
# In the simplifier, "not_subset_count[meta_idx, meta_idx_other]" counts how many
# symbols in meta_idx are NOT subsets of the corresponding range in meta_idx_other.
# If meta_idx doesn't have a symbol that meta_idx_other has, that doesn't matter.
# If meta_idx HAS a symbol that meta_idx_other doesn't, then it IS NOT a subset
# (since meta_idx_other implicitly has full range for that symbol... wait, no).

# Actually from the pairwise comparison code (line 560+):
# is_not_subset_of[[meta_idx_outer]][meta_idx_inner, k] tracks whether
# entry_outer's k-th symbol range is NOT a subset of entry_inner's range for that symbol.
# If entry_inner doesn't HAVE that symbol, we check if entry_inner has the symbol
# in symbol_registry. If not in symbol_registry -> the symbol exists nowhere else -> skip.
# If in symbol_registry but not in entry_inner -> entry_inner doesn't restrict that symbol,
# so entry_outer is a subset by default (is_not_subset_of = FALSE).

# So let me construct a proper 2nd-order SSE scenario:
set.seed(73001)

u = CnfUniverse()
V1 = CnfSymbol(u, "V1", c("a", "b", "c"))
V2 = CnfSymbol(u, "V2", c("a", "b", "c"))
V3 = CnfSymbol(u, "V3", c("a", "b", "c"))

# Scenario 1: Standard 2nd order SSE trigger
# Three 2-symbol clauses where two resolve against a third on different symbols
for (trial in 1:500) {
  u = CnfUniverse()
  n_vars = sample(3:4, 1)
  dom_size = sample(3:4, 1)
  dom = paste0("d", 1:dom_size)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create clauses with specific structure for 2nd order SSE
  n_cl = sample(4:8, 1)
  clauses = list()

  # Create a "target" clause with 2-3 symbols
  target_syms = sample(names(syms), sample(2:min(3, n_vars), 1))
  target_atoms = lapply(target_syms, function(s) {
    syms[[s]] %among% sample(dom, sample(1:max(1, dom_size - 1), 1))
  })
  clauses[[1]] = as.CnfClause(Reduce(`|`, target_atoms))

  # Create "oneend" clauses that cover target on all but one symbol
  for (i in 2:n_cl) {
    # Pick symbols that overlap with target and some that don't
    n_atoms = sample(1:min(n_vars, 3), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:max(1, dom_size - 1), 1))
    })
    clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [2nd-order-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("2nd-order-%d", trial))
}
cat(sprintf("  Constructed 2nd-order SSE: %d tests, %d failures\n", n_tests, n_failures))

# === Symmetric 2nd-order SSE ===
cat("\n=== Symmetric 2nd-order SSE ===\n")
set.seed(73002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create complementary pairs that should trigger 2nd-order SSE
  n_cl = sample(4:7, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(2:3, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:3, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [sym-2nd-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("sym-2nd-%d", trial))
}
cat(sprintf("  Symmetric 2nd-order: %d tests, %d failures\n", n_tests, n_failures))

# === Three-clause interactions ===
cat("\n=== Three-clause interactions ===\n")
set.seed(73003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create triples of clauses that interact
  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(2:3, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [triple-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("triple-%d", trial))
}
cat(sprintf("  Three-clause interactions: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
