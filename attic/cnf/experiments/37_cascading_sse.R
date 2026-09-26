#!/usr/bin/env Rscript
# Test cascading self-subsumption elimination (SSE) and unit propagation
# Focus on chains where one simplification triggers another
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

# === SSE creating new units that cascade ===
cat("=== SSE -> unit -> propagation cascade ===\n")

# Pattern: SSE restricts a clause to a single value, making it a unit,
# which then propagates and further simplifies

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
B = CnfSymbol(u, "B", c("b1", "b2"))
C = CnfSymbol(u, "C", c("c1", "c2"))

# Clause 1: (A in {a1, a2} | B in {b1})  -- long clause
# Clause 2: (A in {a1, a2})              -- unit for A
# SSE: clause 2 is subset of clause 1 on A (not_subset_count = 1 on B)
# SSE restricts clause 1's B to intersect with clause 2's B (doesn't exist → empty)
# Wait, that's subsumption, not SSE. Let me rethink.

# Actually for SSE:
# Clause P: (A in {a1} | B in {b1})
# Clause Q: (A in {a1, a2} | B in {b2})
# P[A] = {a1} ⊆ Q[A] = {a1, a2}: yes
# P[B] = {b1} ⊆ Q[B] = {b2}: no
# not_subset_count(P over Q) = 1 on B
# SSE: restrict Q[B] to intersect with P[B] = {b1} → Q becomes (A in {a1, a2} | B in {b1} ∩ {b2}) = (A in {a1, a2} | B in {})
# B becomes empty → eliminate B from clause Q → Q becomes unit (A in {a1, a2})
# New unit A propagates...

clauses = list(
  as.CnfClause(A %among% "a1" | B %among% "b1"),
  as.CnfClause(A %among% c("a1", "a2") | B %among% "b2"),
  as.CnfClause(A %among% "a1" | C %among% "c1"),  # will be affected by unit propagation
  as.CnfClause(B %among% "b1" | C %among% "c2")   # will also be affected
)
f = tryCatch(CnfFormula(clauses), error = function(e) e)
if (inherits(f, "error")) {
  n_tests = n_tests + 1; n_failures = n_failures + 1
  cat(sprintf("ERROR cascade-1: %s\n", f$message))
} else {
  check_fc(f, clauses, u, "cascade-1")
}

# === Multi-step cascade: SSE -> unit -> SSE -> unit ===
cat("\n=== Multi-step cascading ===\n")

u2 = CnfUniverse()
X = CnfSymbol(u2, "X", c("x1", "x2", "x3"))
Y = CnfSymbol(u2, "Y", c("y1", "y2"))
Z = CnfSymbol(u2, "Z", c("z1", "z2"))
W = CnfSymbol(u2, "W", c("w1", "w2"))

# Step 1: SSE on clause 2 using clause 1 creates unit for Y
# Step 2: Y-unit propagates to clause 3, creating unit for Z
# Step 3: Z-unit propagates to clause 4
clauses2 = list(
  as.CnfClause(X %among% "x1" | Y %among% "y1"),              # clause 1
  as.CnfClause(X %among% c("x1", "x2") | Y %among% "y2"),     # clause 2: SSE with clause 1 eliminates Y
  as.CnfClause(Y %among% "y1" | Z %among% "z1"),              # clause 3: after Y unit, may cascade to Z
  as.CnfClause(Z %among% "z1" | W %among% "w1")               # clause 4: affected by Z changes
)
f2 = tryCatch(CnfFormula(clauses2), error = function(e) e)
if (inherits(f2, "error")) {
  n_tests = n_tests + 1; n_failures = n_failures + 1
  cat(sprintf("ERROR cascade-2: %s\n", f2$message))
} else {
  check_fc(f2, clauses2, u2, "cascade-2")
}

cat(sprintf("  Multi-step: %d tests, %d failures\n", n_tests, n_failures))

# === Randomized cascading patterns ===
cat("\n=== Randomized cascading ===\n")
set.seed(37001)

for (trial in 1:400) {
  u = CnfUniverse()
  n_vars = sample(3:5, 1)
  dom_sizes = sample(2:3, n_vars, replace = TRUE)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  # Create chain-like clauses: each clause shares a variable with the next
  clauses = list()
  var_names = names(syms)

  # Chain: (V1, V2), (V2, V3), (V3, V4), ...
  for (i in 1:(n_vars - 1)) {
    v1 = var_names[i]
    v2 = var_names[i + 1]
    dom1 = u[[v1]]; dom2 = u[[v2]]
    a1 = syms[[v1]] %among% sample(dom1, sample(1:max(1, length(dom1)-1), 1))
    a2 = syms[[v2]] %among% sample(dom2, sample(1:max(1, length(dom2)-1), 1))
    clauses[[length(clauses) + 1]] = as.CnfClause(a1 | a2)
  }

  # Add some cross-links
  for (i in 1:sample(1:3, 1)) {
    n_atoms = sample(2:min(n_vars, 3), 1)
    chosen = sample(var_names, n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  # Add a unit or two
  for (i in 1:sample(0:2, 1)) {
    s = sample(var_names, 1)
    dom = u[[s]]
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1)))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [casc trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("casc-%d", trial))
}
cat(sprintf("  Cascading: %d tests, %d failures\n", n_tests, n_failures))

# === Dense interaction patterns ===
# Many clauses sharing many variables, maximizing interaction
cat("\n=== Dense interaction ===\n")
set.seed(37002)

for (trial in 1:300) {
  u = CnfUniverse()
  # 3 vars, small domains, many clauses
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:sample(2:3, 1)))
  }

  n_clauses = sample(8:15, 1)
  clauses = list()
  for (i in 1:n_clauses) {
    n_atoms = sample(1:3, 1)
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
    cat(sprintf("ERROR [dense trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("dense-%d", trial))
}
cat(sprintf("  Dense: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
