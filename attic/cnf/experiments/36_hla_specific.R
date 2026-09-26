#!/usr/bin/env Rscript
# Test HLA (Hidden Literal Addition) specific patterns
# HLA is the most complex phase of simplification - test it thoroughly
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

# === Hidden tautology elimination ===
# Create a formula where HLA makes a clause cover an entire domain, making it a tautology
cat("=== Hidden tautology patterns ===\n")

# Pattern: Three clauses that, via HLA, make a fourth clause a hidden tautology
# Clause target: (X in {1} | Y in {a})
# Helper 1: (X in {1} | Y in {b})  --> adds complement({b}) setdiff domain = {c} to target's Y
# Helper 2: (X in {1} | Y in {c})  --> now target Y = {a,c}, helper's {c} is subset, adding complement {a,b}\{c} = ?
# Actually HLA works on the target's Y: first HLA from helper1 adds setdiff(dom, {a, b}) = {c} -> target Y = {a, c}
# Then HLA from helper2 adds setdiff(dom, {a, c, c}) = {b} -> target Y = {a, c, b} = full domain -> hidden tautology!

u = CnfUniverse()
X = CnfSymbol(u, "X", c("1", "2", "3"))
Y = CnfSymbol(u, "Y", c("a", "b", "c"))

clauses = list(
  as.CnfClause(X %among% "1" | Y %among% "a"),   # target for HLA
  as.CnfClause(X %among% "1" | Y %among% "b"),   # helper 1
  as.CnfClause(X %among% "1" | Y %among% "c"),   # helper 2
  as.CnfClause(X %among% c("1", "2"))             # a unit to constrain X
)
f = tryCatch(CnfFormula(clauses), error = function(e) e)
if (inherits(f, "error")) {
  n_tests = n_tests + 1; n_failures = n_failures + 1
  cat(sprintf("ERROR: HT1: %s\n", f$message))
} else {
  check_fc(f, clauses, u, "hidden-taut-1")
}

# Variant with 4-value domain needing 3 HLA steps
u2 = CnfUniverse()
X2 = CnfSymbol(u2, "X", c("1", "2"))
Y2 = CnfSymbol(u2, "Y", c("a", "b", "c", "d"))

clauses2 = list(
  as.CnfClause(X2 %among% "1" | Y2 %among% "a"),
  as.CnfClause(X2 %among% "1" | Y2 %among% "b"),
  as.CnfClause(X2 %among% "1" | Y2 %among% "c"),
  as.CnfClause(X2 %among% "1" | Y2 %among% "d")
)
f2 = tryCatch(CnfFormula(clauses2), error = function(e) e)
if (inherits(f2, "error")) {
  n_tests = n_tests + 1; n_failures = n_failures + 1
  cat(sprintf("ERROR: HT2: %s\n", f2$message))
} else {
  check_fc(f2, clauses2, u2, "hidden-taut-2")
}

cat(sprintf("  Hidden tautology: %d tests, %d failures\n", n_tests, n_failures))

# === Hidden subsumption elimination ===
cat("\n=== Hidden subsumption patterns ===\n")

# Pattern: HLA extends a clause until it becomes a superset of another clause
u3 = CnfUniverse()
A = CnfSymbol(u3, "A", c("a1", "a2", "a3"))
B = CnfSymbol(u3, "B", c("b1", "b2"))
C = CnfSymbol(u3, "C", c("c1", "c2"))

# Clause 1 (target): (A in {a1} | B in {b1} | C in {c1})
# Clause 2 (helper): (A in {a1} | B in {b1})
# HLA using clause 2 on clause 1: clause 2 has A,B. For C: not in clause 2, so not_subset_count for C is 1.
# Wait, that means clause 2 is subset of clause 1 on A and B (if a1 subset of a1, b1 subset of b1).
# not_subset_count(clause2 over clause1) on C = TRUE (C not in clause2), = 1 total → SSE, not HLA.
# SSE would restrict clause1's C to intersect with clause2's C (which doesn't exist → empty).
# That would eliminate C from clause 1.

# Let me construct a proper HLA pattern instead:
# Target: (A in {a1, a2} | B in {b1} | C in {c1})
# Helper: (A in {a1} | B in {b1} | C in {c2})
# Helper's A⊆Target's A? {a1} ⊆ {a1, a2} yes
# Helper's B⊆Target's B? {b1} ⊆ {b1} yes
# Helper's C⊆Target's C? {c2} ⊆ {c1} no
# not_subset_count = 1, on C.
# HLA adds complement of helper's C to target's C: setdiff(dom(C), {c1, c2}) = {} → target C stays {c1}
# Hmm, domain of C is only {c1, c2}, so complement is empty.

# Better: use larger domain
u4 = CnfUniverse()
P = CnfSymbol(u4, "P", c("p1", "p2", "p3"))
Q = CnfSymbol(u4, "Q", c("q1", "q2", "q3"))
R = CnfSymbol(u4, "R", c("r1", "r2", "r3"))

# Target: (P in {p1} | Q in {q1} | R in {r1})
# Helper1: (P in {p1} | Q in {q2} | R in {r1})  -- subset on P,R; not on Q
#   HLA adds setdiff(dom(Q), {q1, q2}) = {q3} → target Q becomes {q1, q3}
# Helper2: (P in {p1} | Q in {q3} | R in {r1})  -- subset on P,R; Q: {q3} ⊆ {q1,q3}? yes after HLA
#   Wait, after HLA, target Q = {q1, q3}. Helper2's Q = {q3} ⊆ {q1, q3} → now subset!
#   So not_subset_count drops to 0 → hidden subsumption → target is eliminated!
# Plus need a "base" clause to prevent everything from being trivially simplified
clauses4 = list(
  as.CnfClause(P %among% "p1" | Q %among% "q1" | R %among% "r1"),  # target
  as.CnfClause(P %among% "p1" | Q %among% "q2" | R %among% "r1"),  # helper 1
  as.CnfClause(P %among% "p1" | Q %among% "q3" | R %among% "r1"),  # helper 2 (triggers subsumption after HLA)
  as.CnfClause(P %among% c("p1", "p2") | R %among% "r2")           # anchor clause
)
f4 = tryCatch(CnfFormula(clauses4), error = function(e) e)
if (inherits(f4, "error")) {
  n_tests = n_tests + 1; n_failures = n_failures + 1
  cat(sprintf("ERROR: HS1: %s\n", f4$message))
} else {
  check_fc(f4, clauses4, u4, "hidden-sub-1")
}

cat(sprintf("  Hidden subsumption: %d tests, %d failures\n", n_tests, n_failures))

# === Randomized HLA patterns ===
cat("\n=== Randomized HLA stress ===\n")
set.seed(36001)

for (trial in 1:300) {
  u = CnfUniverse()
  # 3 vars with domain 3-4 to make HLA likely
  dom_sizes = sample(3:4, 3, replace = TRUE)
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  # Create groups of clauses that share most atoms (HLA-friendly)
  # Pick a "base" pattern and vary one symbol across clauses
  base_atoms = sample(names(syms), 2)
  base_vals = lapply(base_atoms, function(s) {
    dom = u[[s]]
    sample(dom, sample(1:max(1, length(dom)-1), 1))
  })
  names(base_vals) = base_atoms

  varying_sym = setdiff(names(syms), base_atoms)[1]
  vary_dom = u[[varying_sym]]

  clauses = list()
  # Create clauses that share the base pattern but differ on the varying symbol
  for (i in seq_along(vary_dom)) {
    atoms = list()
    for (s in base_atoms) {
      atoms[[length(atoms) + 1]] = syms[[s]] %among% base_vals[[s]]
    }
    atoms[[length(atoms) + 1]] = syms[[varying_sym]] %among% vary_dom[i]
    clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  # Add some random clauses
  for (i in 1:sample(2:4, 1)) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [HLA trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("HLA-%d", trial))
}
cat(sprintf("  HLA stress: %d tests, %d failures\n", n_tests, n_failures))

# === Unit HLA phase ===
# The unit HLA phase is separate from the non-unit HLA phase
# It processes units using remaining non-unit clauses for HLA
cat("\n=== Unit HLA phase patterns ===\n")
set.seed(36002)

for (trial in 1:200) {
  u = CnfUniverse()
  # Create a mix of units and clauses where units can be HLA-eliminated
  n_vars = sample(3:5, 1)
  dom_sizes = sample(2:4, n_vars, replace = TRUE)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  clauses = list()
  # Add units for most variables (this exercises the unit HLA path)
  unit_vars = sample(names(syms), min(n_vars - 1, sample(1:3, 1)))
  for (s in unit_vars) {
    dom = u[[s]]
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1)))
  }

  # Add multi-symbol clauses
  for (i in 1:sample(3:8, 1)) {
    n_atoms = sample(2:min(n_vars, 4), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [UHLA trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("UHLA-%d", trial))
}
cat(sprintf("  Unit HLA: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
