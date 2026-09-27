#!/usr/bin/env Rscript
# Test 2nd-order SSE (resolution subsumption) patterns specifically
# This targets the handle_sse_2nd_order_oneend and _twoend functions
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

# === Classic 2nd-order SSE pattern ===
cat("=== Classic 2nd-order SSE ===\n")

# 2nd-order SSE needs:
# - A "twoend" clause T: subset of target on all except TWO symbols S1, S2
# - A "oneend" clause O: subset of target on all except ONE symbol S1 (or S2)
# The resolution: restrict target[S2] to union(O[S2], T[S2])

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
B = CnfSymbol(u, "B", c("b1", "b2", "b3"))
C = CnfSymbol(u, "C", c("c1", "c2"))

# Target: (A in {a1, a2} | B in {b1, b2} | C in {c1})
# Twoend: (A in {a3} | B in {b3} | C in {c1})  -- non-subset on A and B, subset on C
# Oneend: (A in {a1, a2} | B in {b1} | C in {c1})  -- subset on A and C, non-subset on B
# Wait, let me re-read the code. The "target" for restriction is idx_target, and the restriction
# is applied to symbol_target on the target clause.

# Let me be more precise:
# "Twoend" means not_subset_count == 2 (two symbols not subsets)
# "Oneend" means not_subset_count == 1 (one symbol not subset)
# In handle_sse_2nd_order_twoend(meta_idx, meta_idx_other):
#   meta_idx is the "base" clause, meta_idx_other is the "twoend target"
#   The two non-subset symbols from meta_idx in meta_idx_other are identified
#   Then for each pair of non-subset symbols (s1, s2):
#     look for a "oneend" clause that is subset of meta_idx_other on all but s1
#     Then restrict meta_idx_other[s2] to union(oneend[s2], meta_idx[s2])

# Pattern A:
# Base (meta_idx): (A in {a1} | B in {b1})
# Target (meta_idx_other): (A in {a1, a2} | B in {b1, b2} | C in {c1})
# Base is subset of Target on... A: {a1} ⊆ {a1, a2} yes. B: {b1} ⊆ {b1, b2} yes. C: not in Base.
# C not in Base means it's a non-subset. So not_subset_count = 1 (just C). That's oneend, not twoend.

# For twoend, Base must have 2 symbols not subset of Target.
# Base: (A in {a2} | B in {b2} | C in {c1})
# Target: (A in {a1} | B in {b1} | C in {c1})
# A: {a2} ⊆ {a1}? No. B: {b2} ⊆ {b1}? No. C: {c1} ⊆ {c1}? Yes.
# not_subset_count = 2 (A and B). This is twoend.
# Now we need a oneend clause w.r.t. Target that has not_subset on A (or B):
# Oneend: (A in {a1} | B in {b2} | C in {c1}) -- A ⊆ Target[A]? {a1} ⊆ {a1} yes. B: {b2} ⊆ {b1} no. C: {c1} ⊆ {c1} yes.
# This is oneend on B.
# Resolution: restrict Target[A] to union(Oneend[A], Base[A]) = union({a1}, {a2}) = {a1, a2}
# But Target[A] = {a1}, and the restriction would EXPAND it (domain restriction can only shrink, not expand!)

# Hmm, I'm getting confused about the direction. Let me re-read try_sse_2nd_order carefully.
# apply_domain_restriction(idx_target, symbol_target, char_union(clause_oneend[[symbol_target]], clause_twoends[[symbol_target]]), FALSE)
# idx_target is the clause being restricted. The restriction is to the union of oneend[symbol_target] and twoend[symbol_target].
# But apply_domain_restriction does an INTERSECTION with the restringent.
# So Target[symbol_target] becomes intersect(Target[symbol_target], union(oneend[symbol_target], twoend[symbol_target]))

# For this to actually DO something, we need union(oneend[symbol_target], twoend[symbol_target]) to be a proper subset of Target[symbol_target].

# OK I think I need a more concrete example. Let me use:
# Clause 1 (oneend w.r.t. clause 3): (A in {a1} | B in {b1})
#   - A: {a1} ⊆ clause3[A]? depends on clause 3
#   - B: {b1} ⊆ clause3[B]? depends on clause 3
# Clause 2 (twoend w.r.t. clause 3): (A in {a2} | B in {b2})
# Clause 3 (target): (A in {a1, a2, a3} | B in {b1, b2, b3})
#
# Check clause 1 vs clause 3:
#   A: {a1} ⊆ {a1,a2,a3} yes. B: {b1} ⊆ {b1,b2,b3} yes.
#   not_subset_count = 0 → subsumption! Clause 3 eliminated. That's not what we want.

# We need the oneend/twoend clauses to NOT be subsets on some symbols.
# Let me add a third variable C to prevent trivial subsumption.

# Clause 1 (oneend): (A in {a1} | B in {b1} | C in {c2})
#   vs Clause 3 target: (A in {a1, a2} | B in {b1, b2} | C in {c1})
#   A: {a1} ⊆ {a1,a2} yes. B: {b1} ⊆ {b1,b2} yes. C: {c2} ⊆ {c1} no.
#   not_subset_count = 1 (C is non-subset). This is oneend on C.

# Clause 2 (twoend): (A in {a2} | B in {b2} | C in {c2})
#   vs Clause 3 target: (A in {a1, a2} | B in {b1, b2} | C in {c1})
#   A: {a2} ⊆ {a1,a2} yes. B: {b2} ⊆ {b1,b2} yes. C: {c2} ⊆ {c1} no.
#   not_subset_count = 1 (C is non-subset). This is also oneend! Not twoend.

# For twoend we need TWO non-subset symbols. Let me try:
# Clause 2 (twoend): (A in {a3} | B in {b2} | C in {c2})
#   A: {a3} ⊆ {a1,a2} no. B: {b2} ⊆ {b1,b2} yes. C: {c2} ⊆ {c1} no.
#   not_subset_count = 2 (A and C). Twoend!

# So for 2nd-order SSE with symbol_target = A:
# In handle_sse_2nd_order_twoend, the two non-subset symbols are A and C.
# For symbol A: look for oneend clauses w.r.t. target that have non-subset on A only.
# Clause 1 is oneend on C, not on A. So no match for symbol A.
# For symbol C: look for oneend clauses w.r.t. target that have non-subset on C only.
# Clause 1 is oneend on C. Match!
# Resolution: restrict target[A] (the OTHER non-subset symbol) to union(clause1[A], clause2[A]) = union({a1}, {a3}) = {a1, a3}
# Target[A] = {a1, a2}, intersected with {a1, a3} = {a1}. Target A shrinks!

# This is a valid 2nd-order SSE pattern.
clauses = list(
  as.CnfClause(A %among% "a1" | B %among% "b1" | C %among% "c2"),  # oneend (on C) w.r.t. target
  as.CnfClause(A %among% "a3" | B %among% "b2" | C %among% "c2"),  # twoend (on A,C) w.r.t. target
  as.CnfClause(A %among% c("a1", "a2") | B %among% c("b1", "b2") | C %among% "c1")  # target
)
f = tryCatch(CnfFormula(clauses), error = function(e) e)
if (inherits(f, "error")) {
  n_tests = n_tests + 1; n_failures = n_failures + 1
  cat(sprintf("ERROR 2nd-order-1: %s\n", f$message))
} else {
  check_fc(f, clauses, u, "2nd-order-1")
}

# === Twoend-twoend interaction ===
# handle_sse_2nd_order_twoend also looks for OTHER twoend clauses with "common overlap"
cat("\n=== Twoend-twoend interaction ===\n")

u2 = CnfUniverse()
P = CnfSymbol(u2, "P", c("p1", "p2", "p3"))
Q = CnfSymbol(u2, "Q", c("q1", "q2", "q3"))
R = CnfSymbol(u2, "R", c("r1", "r2"))

# Target: (P in {p1, p2} | Q in {q1, q2} | R in {r1})
# Twoend1: (P in {p3} | Q in {q1} | R in {r2})
#   P: {p3} ⊆ {p1,p2} no. Q: {q1} ⊆ {q1,q2} yes. R: {r2} ⊆ {r1} no.
#   not_subset_count = 2 (P, R)
# Twoend2: (P in {p1} | Q in {q3} | R in {r2})
#   P: {p1} ⊆ {p1,p2} yes. Q: {q3} ⊆ {q1,q2} no. R: {r2} ⊆ {r1} no.
#   not_subset_count = 2 (Q, R)
# Common non-subset symbol = R
# The twoend-twoend part: both have R as non-subset, so they share that symbol.
# This would trigger try_sse_2nd_order with:
#   oneend = twoend2, twoend = twoend1, target = Target, symbol_target = P (the other non-subset of twoend1)
# restriction: union(twoend2[P], twoend1[P]) = union({p1}, {p3}) = {p1, p3}
# Target[P] = {p1, p2} intersected with {p1, p3} = {p1}. Shrinks!

clauses2 = list(
  as.CnfClause(P %among% c("p1", "p2") | Q %among% c("q1", "q2") | R %among% "r1"),  # target
  as.CnfClause(P %among% "p3" | Q %among% "q1" | R %among% "r2"),                     # twoend1
  as.CnfClause(P %among% "p1" | Q %among% "q3" | R %among% "r2")                      # twoend2
)
f2 = tryCatch(CnfFormula(clauses2), error = function(e) e)
if (inherits(f2, "error")) {
  n_tests = n_tests + 1; n_failures = n_failures + 1
  cat(sprintf("ERROR twoend-twoend: %s\n", f2$message))
} else {
  check_fc(f2, clauses2, u2, "twoend-twoend")
}

cat(sprintf("  Twoend-twoend: %d tests, %d failures\n", n_tests, n_failures))

# === Randomized 2nd-order SSE patterns ===
cat("\n=== Randomized 2nd-order SSE ===\n")
set.seed(39001)

for (trial in 1:500) {
  u = CnfUniverse()
  # 3-4 vars with domains 2-4 (sweet spot for 2nd-order SSE)
  n_vars = sample(3:4, 1)
  dom_sizes = sample(2:4, n_vars, replace = TRUE)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  # Create clauses with varying overlap patterns to trigger 2nd-order SSE
  n_clauses = sample(4:10, 1)
  clauses = list()
  for (i in 1:n_clauses) {
    # Each clause uses 2-n_vars symbols
    n_atoms = sample(2:n_vars, 1)
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
    cat(sprintf("ERROR [2nd trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("2nd-%d", trial))
}
cat(sprintf("  Random 2nd-order: %d tests, %d failures\n", n_tests, n_failures))

# === Patterns that specifically exercise the on_update_range path ===
cat("\n=== on_update_range patterns ===\n")
set.seed(39002)

for (trial in 1:300) {
  u = CnfUniverse()
  # Use 4 variables to maximize range-shrinking interactions
  n_vars = 4
  dom_sizes = sample(2:3, n_vars, replace = TRUE)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  # First, create some "wide" clauses (many values per symbol)
  # that can be narrowed by SSE
  clauses = list()
  for (i in 1:sample(3:6, 1)) {
    n_atoms = sample(2:n_vars, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      # Prefer wide ranges (more values) to create opportunities for range shrinking
      n_vals = if (length(dom) > 1 && runif(1) > 0.3) length(dom) - 1 else sample(1:max(1, length(dom)-1), 1)
      syms[[s]] %among% sample(dom, n_vals)
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  # Then add "narrow" clauses that can trigger SSE
  for (i in 1:sample(2:4, 1)) {
    n_atoms = sample(2:min(n_vars, 3), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, 1)  # single value
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [range trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("range-%d", trial))
}
cat(sprintf("  on_update_range: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
