#!/usr/bin/env Rscript
# Test deep cascading unit propagation:
# Where restricting one unit cascades through many clauses,
# creating new units that cascade further.
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

set.seed(27182)

# Strategy 1: Chain of implications
# X1 in {a} -> X2 must be in {b} -> X3 must be in {c} -> ...
cat("=== Chain cascading ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(3:6, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    dom_size = sample(2:4, 1)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_size))
  }

  # Build chain: V1 restricted -> affects clauses with V1 and V2 -> V2 restricted -> ...
  clauses = list()
  # Start with a unit for V1
  dom1 = u[["V1"]]
  unit_val = sample(dom1, 1)
  clauses[[1]] = as.CnfClause(syms[["V1"]] %among% unit_val)

  # Chain of implications
  for (v in 2:n_vars) {
    prev = paste0("V", v - 1)
    curr = paste0("V", v)
    dom_prev = u[[prev]]
    dom_curr = u[[curr]]

    # For each value in prev's domain, create a clause linking to curr
    for (val in dom_prev) {
      curr_vals = sample(dom_curr, sample(1:length(dom_curr), 1))
      clauses[[length(clauses) + 1]] = as.CnfClause(
        syms[[prev]] %among% val | syms[[curr]] %among% curr_vals
      )
    }
  }

  # Add some random extra clauses
  n_extra = sample(0:3, 1)
  for (i in seq_len(n_extra)) {
    v1 = sample(names(syms), 1)
    v2 = sample(setdiff(names(syms), v1), 1)
    d1 = u[[v1]]; d2 = u[[v2]]
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[v1]] %among% sample(d1, sample(1:max(1, length(d1)-1), 1)) |
      syms[[v2]] %among% sample(d2, sample(1:max(1, length(d2)-1), 1))
    )
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [cascade trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("cascade-%d", trial))
}
cat(sprintf("  Chain cascading: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 2: Web of mutual implications
# Multiple units that together restrict many variables
cat("\n=== Web cascading ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:5) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:3))
  }

  clauses = list()
  # Add 2-3 units
  n_units = sample(2:3, 1)
  unit_vars = sample(names(syms), n_units)
  for (uv in unit_vars) {
    dom = u[[uv]]
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[uv]] %among% sample(dom, sample(1:2, 1))
    )
  }

  # Add many clauses that connect variables
  n_clauses = sample(5:15, 1)
  for (i in 1:n_clauses) {
    n_atoms = sample(2:4, 1)
    chosen = sample(names(syms), min(n_atoms, 5))
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [web trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("web-%d", trial))
}
cat(sprintf("  Web cascading: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 3: Contradicting cascades
# Units that cascade to create contradictions
cat("\n=== Contradiction cascading ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2"))
  C = CnfSymbol(u, "C", c("c1", "c2", "c3"))
  D = CnfSymbol(u, "D", c("d1", "d2"))
  syms = list(A = A, B = B, C = C, D = D)

  clauses = list()
  # Add conflicting units and implications
  clauses[[1]] = as.CnfClause(A %among% sample(c("a1", "a2", "a3"), sample(1:2, 1)))
  clauses[[2]] = as.CnfClause(A %among% sample(c("a1", "a2", "a3"), sample(1:2, 1)))

  # Add implications that cascade
  for (i in 1:sample(3:6, 1)) {
    v1 = sample(names(syms), 1)
    v2 = sample(setdiff(names(syms), v1), 1)
    d1 = u[[v1]]; d2 = u[[v2]]
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[v1]] %among% sample(d1, sample(1:max(1, length(d1)-1), 1)) |
      syms[[v2]] %among% sample(d2, sample(1:max(1, length(d2)-1), 1))
    )
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [contra trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("contra-%d", trial))
}
cat(sprintf("  Contradiction cascading: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
