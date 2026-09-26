#!/usr/bin/env Rscript
# Test scenarios where clauses change length during simplification
# This exercises: eliminate_symbol_from_clause, new unit creation,
# and the is_not_subset_of column handling when clause size changes
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

# === Clause shrinks from length 3 to unit ===
cat("=== Length 3 -> unit ===\n")

# Unit propagation removes two symbols from a 3-symbol clause, creating a new unit
u = CnfUniverse()
A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
B = CnfSymbol(u, "B", c("b1", "b2"))
C = CnfSymbol(u, "C", c("c1", "c2"))

# Units for A and B
# Clause: (A in {a3} | B in {b2} | C in {c1})
# After unit A in {a1, a2}: A range in clause becomes {} → eliminate A → (B in {b2} | C in {c1})
# After unit B in {b1}: B range in clause becomes {} → eliminate B → (C in {c1})
# Now clause is a unit!
clauses = list(
  as.CnfClause(A %among% c("a1", "a2")),       # unit A
  as.CnfClause(B %among% "b1"),                  # unit B
  as.CnfClause(A %among% "a3" | B %among% "b2" | C %among% "c1")  # will shrink to unit
)
f = tryCatch(CnfFormula(clauses), error = function(e) e)
if (inherits(f, "error")) {
  n_tests = n_tests + 1; n_failures = n_failures + 1
  cat(sprintf("ERROR 3-to-unit: %s\n", f$message))
} else {
  check_fc(f, clauses, u, "3-to-unit")
}

# === Clause shrinks to empty → contradiction ===
cat("\n=== Clause shrinks to empty (contradiction) ===\n")

u2 = CnfUniverse()
X = CnfSymbol(u2, "X", c("x1", "x2"))
Y = CnfSymbol(u2, "Y", c("y1", "y2"))

# Unit X in {x1}, clause (X in {x2} | Y in {y2}), unit Y in {y1}
# After X unit: clause becomes (Y in {y2})
# After Y unit: clause restricts Y to {y2} ∩ {y1} = {} → contradiction
clauses2 = list(
  as.CnfClause(X %among% "x1"),
  as.CnfClause(Y %among% "y1"),
  as.CnfClause(X %among% "x2" | Y %among% "y2")
)
f2 = tryCatch(CnfFormula(clauses2), error = function(e) e)
n_tests = n_tests + 1
if (inherits(f2, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR empty-contra: %s\n", f2$message))
} else if (!isFALSE(as.logical(f2))) {
  n_failures = n_failures + 1
  cat("FAIL: should be contradiction\n")
}

cat(sprintf("  Length transitions: %d tests, %d failures\n", n_tests, n_failures))

# === Randomized length transition stress ===
cat("\n=== Randomized length transitions ===\n")
set.seed(42001)

for (trial in 1:500) {
  u = CnfUniverse()
  n_vars = sample(3:5, 1)
  dom_sizes = sample(2:3, n_vars, replace = TRUE)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  clauses = list()
  # Add restrictive units to force symbol elimination from other clauses
  n_units = sample(1:min(3, n_vars), 1)
  unit_vars = sample(names(syms), n_units)
  for (s in unit_vars) {
    dom = u[[s]]
    # Use a PROPER subset to force restriction
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, max(1, length(dom)-1)))
  }

  # Add clauses with many symbols, some of which will be eliminated
  for (i in 1:sample(3:6, 1)) {
    n_atoms = sample(2:min(n_vars, 4), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      # Sometimes use values that will be eliminated by units
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [len trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("len-%d", trial))
}
cat(sprintf("  Randomized transitions: %d tests, %d failures\n", n_tests, n_failures))

# === SSE creates new unit which creates further SSE ===
cat("\n=== SSE chain creating new units ===\n")
set.seed(42002)

for (trial in 1:300) {
  u = CnfUniverse()
  n_vars = 4
  dom_sizes = sample(2:3, n_vars, replace = TRUE)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  # Create interlocking 2-symbol clauses that can cascade via SSE
  clauses = list()
  for (i in 1:sample(4:8, 1)) {
    v1 = sample(names(syms), 1)
    v2 = sample(setdiff(names(syms), v1), 1)
    dom1 = u[[v1]]; dom2 = u[[v2]]
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[v1]] %among% sample(dom1, sample(1:max(1, length(dom1)-1), 1)) |
      syms[[v2]] %among% sample(dom2, sample(1:max(1, length(dom2)-1), 1))
    )
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [sse-chain trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("sse-chain-%d", trial))
}
cat(sprintf("  SSE chain: %d tests, %d failures\n", n_tests, n_failures))

# === Clauses that become units during the is_not_subset_of building phase ===
cat("\n=== Units during subset matrix building ===\n")
set.seed(42003)

for (trial in 1:200) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2"))
  C = CnfSymbol(u, "C", c("c1", "c2"))
  D = CnfSymbol(u, "D", c("d1", "d2"))

  syms = list(A = A, B = B, C = C, D = D)

  # Mix of units and multi-symbol clauses
  clauses = list()
  # One unit that forces cascading
  s = sample(c("A", "B", "C", "D"), 1)
  dom = u[[s]]
  clauses[[1]] = as.CnfClause(syms[[s]] %among% sample(dom, max(1, length(dom)-1)))

  # Several multi-symbol clauses
  for (i in 1:sample(4:7, 1)) {
    n_atoms = sample(2:4, 1)
    chosen = sample(c("A", "B", "C", "D"), n_atoms)
    atoms = lapply(chosen, function(s2) {
      dom2 = u[[s2]]
      syms[[s2]] %among% sample(dom2, sample(1:max(1, length(dom2)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [matrix-unit trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("matrix-unit-%d", trial))
}
cat(sprintf("  Units during matrix: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
