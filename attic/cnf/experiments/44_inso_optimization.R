#!/usr/bin/env Rscript
# Test the use_inso optimization in register_unit
# This optimization uses the is_not_subset_of matrix to skip unnecessary
# apply_domain_restriction calls during cascading unit creation.
# Triggered when SSE creates a new unit during the pairwise comparison phase.
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
    cat(sprintf("  Assignment: %s\n",
      paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
    return(FALSE)
  }
  TRUE
}

# === Pattern: SSE during pairwise phase creates new unit ===
cat("=== SSE creates unit during pairwise phase ===\n")

# For SSE to create a new unit during the pairwise comparison:
# 1. Need two non-unit clauses where SSE eliminates a symbol, leaving a 1-symbol clause
# 2. This means one clause has 2 symbols, and SSE removes one, leaving a unit
# 3. The clauses must come AFTER the unit queue (so they're non-units during initial processing)

# Example:
# Clause A (sorted first, length 2): (X in {x1} | Y in {y1})
# Clause B (sorted later, length 2): (X in {x1, x2} | Y in {y2})
# A is subset of B on X: {x1} ⊆ {x1, x2} yes
# A not subset of B on Y: {y1} ⊆ {y2} no
# not_subset_count(A over B) = 1, SSE: restrict B[Y] to A[Y] = {y1}
# B becomes (X in {x1, x2} | Y in {y1 ∩ y2}) = (X in {x1, x2} | Y in {})
# Y becomes empty → eliminate Y → B becomes unit (X in {x1, x2})
# New X-unit propagates to other clauses that use X

u = CnfUniverse()
X = CnfSymbol(u, "X", c("x1", "x2", "x3"))
Y = CnfSymbol(u, "Y", c("y1", "y2"))
Z = CnfSymbol(u, "Z", c("z1", "z2"))

clauses = list(
  as.CnfClause(X %among% "x1" | Y %among% "y1"),           # clause A
  as.CnfClause(X %among% c("x1", "x2") | Y %among% "y2"),  # clause B (SSE → unit X in {x1, x2})
  as.CnfClause(X %among% c("x1", "x3") | Z %among% "z1"),  # affected by new X-unit
  as.CnfClause(X %among% "x2" | Z %among% "z2"),           # affected by new X-unit
  as.CnfClause(Y %among% "y1" | Z %among% "z1")            # shares Y with clause A
)
f = tryCatch(CnfFormula(clauses), error = function(e) e)
if (inherits(f, "error")) {
  n_tests = n_tests + 1; n_failures = n_failures + 1
  cat(sprintf("ERROR inso-1: %s\n", f$message))
} else {
  check_fc(f, clauses, u, "inso-1")
}

# Pattern 2: Multiple SSEs creating multiple units in sequence
u2 = CnfUniverse()
A = CnfSymbol(u2, "A", c("a1", "a2", "a3"))
B = CnfSymbol(u2, "B", c("b1", "b2"))
C = CnfSymbol(u2, "C", c("c1", "c2"))
D = CnfSymbol(u2, "D", c("d1", "d2"))

clauses2 = list(
  # These two create A-unit via SSE
  as.CnfClause(A %among% "a1" | B %among% "b1"),
  as.CnfClause(A %among% c("a1", "a2") | B %among% "b2"),
  # These two create C-unit via SSE (after A-unit propagation)
  as.CnfClause(A %among% "a2" | C %among% "c1" | D %among% "d1"),
  as.CnfClause(A %among% c("a1", "a3") | C %among% "c2" | D %among% "d1"),
  # Another clause to interact with
  as.CnfClause(B %among% "b1" | C %among% "c2" | D %among% "d2")
)
f2 = tryCatch(CnfFormula(clauses2), error = function(e) e)
if (inherits(f2, "error")) {
  n_tests = n_tests + 1; n_failures = n_failures + 1
  cat(sprintf("ERROR inso-2: %s\n", f2$message))
} else {
  check_fc(f2, clauses2, u2, "inso-2")
}

cat(sprintf("  SSE creates unit: %d tests, %d failures\n", n_tests, n_failures))

# === Randomized patterns that maximize cascading during pairwise phase ===
cat("\n=== Randomized cascading during pairwise ===\n")
set.seed(44001)

for (trial in 1:500) {
  u = CnfUniverse()
  n_vars = sample(3:5, 1)
  dom_sizes = sample(2:3, n_vars, replace = TRUE)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  # Don't include explicit units - let SSE create them
  clauses = list()
  n_clauses = sample(4:8, 1)
  for (i in 1:n_clauses) {
    # Use exactly 2 symbols per clause to maximize SSE-to-unit chances
    n_atoms = 2
    chosen = sample(names(syms), min(n_atoms, n_vars))
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [inso trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("inso-%d", trial))
}
cat(sprintf("  Randomized inso: %d tests, %d failures\n", n_tests, n_failures))

# === Patterns with mixed 2-sym and 3-sym clauses ===
cat("\n=== Mixed clause sizes ===\n")
set.seed(44002)

for (trial in 1:300) {
  u = CnfUniverse()
  n_vars = sample(4:5, 1)
  dom_sizes = sample(2:3, n_vars, replace = TRUE)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  clauses = list()
  # Mix of 2-symbol and 3-symbol clauses
  for (i in 1:sample(5:10, 1)) {
    n_atoms = sample(2:min(3, n_vars), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [mixed trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("mixed-%d", trial))
}
cat(sprintf("  Mixed sizes: %d tests, %d failures\n", n_tests, n_failures))

# === Specific pattern: subsumption during pairwise creates cascade ===
cat("\n=== Subsumption cascade during pairwise ===\n")

# When one clause subsumes another during pairwise building, the subsumed clause
# is eliminated. Other clauses that were using the subsumed clause in their
# is_not_subset_of entries now have stale data. The code handles this via
# `if (eliminated[[clause_idx_inner]] || ...) next` checks.

u3 = CnfUniverse()
P = CnfSymbol(u3, "P", c("p1", "p2"))
Q = CnfSymbol(u3, "Q", c("q1", "q2"))
R = CnfSymbol(u3, "R", c("r1", "r2"))
S = CnfSymbol(u3, "S", c("s1", "s2"))

clauses3 = list(
  # Small clause that subsumes larger ones
  as.CnfClause(P %among% "p1" | Q %among% "q1"),
  # This is a superset of the first clause, will be eliminated
  as.CnfClause(P %among% c("p1", "p2") | Q %among% c("q1", "q2") | R %among% "r1"),
  # This interacts with the first clause
  as.CnfClause(P %among% "p2" | Q %among% "q2" | R %among% "r2"),
  # This uses R and S
  as.CnfClause(R %among% "r1" | S %among% "s1"),
  as.CnfClause(Q %among% "q1" | S %among% "s2")
)
f3 = tryCatch(CnfFormula(clauses3), error = function(e) e)
if (inherits(f3, "error")) {
  n_tests = n_tests + 1; n_failures = n_failures + 1
  cat(sprintf("ERROR subsump-cascade: %s\n", f3$message))
} else {
  check_fc(f3, clauses3, u3, "subsump-cascade")
}

cat(sprintf("  Subsumption cascade: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
