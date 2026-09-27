#!/usr/bin/env Rscript
# Test patterns where multiple eliminations happen concurrently:
# - Multiple clauses get subsumed during the same unit propagation
# - SSE on one pair triggers subsumption of another pair
# - HLA on one clause triggers subsumption of another
# These test the interaction between event-driven functions
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

# === Multiple subsumptions from single unit ===
cat("=== Multiple subsumptions from unit ===\n")
set.seed(62001)

for (trial in 1:200) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("0", "1", "2"))
  B = CnfSymbol(u, "B", c("0", "1", "2"))
  C = CnfSymbol(u, "C", c("0", "1", "2"))

  # Unit for A
  a_val = sample(c("0", "1", "2"), sample(1:2, 1))
  unit_a = as.CnfClause(A %among% a_val)

  # Multiple clauses that will be subsumed by unit_a
  # (their A-range is a superset of a_val)
  clauses = list(unit_a)
  for (i in 1:sample(3:6, 1)) {
    b_val = sample(c("0", "1", "2"), sample(1:2, 1))
    c_val = sample(c("0", "1", "2"), sample(1:2, 1))
    # Sometimes make A-range a superset of unit, sometimes not
    if (sample(c(TRUE, FALSE), 1)) {
      clauses[[length(clauses) + 1]] = as.CnfClause(A %among% a_val | B %among% b_val)
    } else {
      a_val2 = sample(c("0", "1", "2"), sample(1:2, 1))
      clauses[[length(clauses) + 1]] = as.CnfClause(A %among% a_val2 | B %among% b_val | C %among% c_val)
    }
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [multi-sub %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("multi-sub-%d", trial))
}
cat(sprintf("  Multiple subsumptions: %d tests, %d failures\n", n_tests, n_failures))

# === SSE chain triggering subsumption ===
cat("\n=== SSE chain -> subsumption ===\n")
set.seed(62002)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1", "2"))
  }

  # Create clauses where SSE on one pair should narrow ranges,
  # potentially causing subsumption of another pair
  n_cl = sample(5:10, 1)
  clauses = list()
  for (i in 1:n_cl) {
    n_atoms = sample(2:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      # Use narrow ranges to increase SSE likelihood
      syms[[s]] %among% sample(c("0", "1", "2"), sample(1:2, 1))
    })
    clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [sse-chain %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("sse-chain-%d", trial))
}
cat(sprintf("  SSE chain subsumption: %d tests, %d failures\n", n_tests, n_failures))

# === Multiple contradicting units ===
cat("\n=== Multiple contradicting units ===\n")
set.seed(62003)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  # Sometimes create contradicting units for the same variable
  clauses = list()
  # Unit 1 for V1
  clauses[[1]] = as.CnfClause(syms[["V1"]] %among% sample(c("a", "b", "c"), sample(1:2, 1)))
  # Unit 2 for V1 (might contradict)
  clauses[[2]] = as.CnfClause(syms[["V1"]] %among% sample(c("a", "b", "c"), sample(1:2, 1)))
  # Other clauses
  for (i in 1:sample(2:4, 1)) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [contra-units %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("contra-units-%d", trial))
}
cat(sprintf("  Contradicting units: %d tests, %d failures\n", n_tests, n_failures))

# === Cascading unit creation ===
cat("\n=== Cascading unit creation ===\n")
set.seed(62004)

# Pattern: non-unit clause that becomes unit after another unit restricts it,
# which then further restricts more clauses
for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1", "2"))
  }

  # A chain: unit V1, clause {V1, V2}, clause {V2, V3}, clause {V3, V4}
  # Unit V1 restricts clause {V1, V2} which might become unit for V2
  # Unit V2 restricts clause {V2, V3} which might become unit for V3
  # etc.
  clauses = list()
  v1_val = sample(c("0", "1", "2"), 1)
  clauses[[1]] = as.CnfClause(syms[["V1"]] %among% v1_val)

  for (i in 1:3) {
    s1 = paste0("V", i)
    s2 = paste0("V", i + 1)
    v1_range = sample(c("0", "1", "2"), sample(1:2, 1))
    v2_range = sample(c("0", "1", "2"), sample(1:2, 1))
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[s1]] %among% v1_range | syms[[s2]] %among% v2_range
    )
  }

  # Add some random clauses
  for (i in 1:sample(2:4, 1)) {
    n_atoms = sample(2:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1", "2"), sample(1:2, 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [cascade-unit %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("cascade-unit-%d", trial))
}
cat(sprintf("  Cascading unit creation: %d tests, %d failures\n", n_tests, n_failures))

# === Concurrent elimination stress ===
cat("\n=== Concurrent elimination stress ===\n")
set.seed(62005)

for (trial in 1:300) {
  u = CnfUniverse()
  n_vars = sample(3:5, 1)
  dom_size = sample(2:3, 1)
  dom = paste0("d", 1:dom_size)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Many clauses with high overlap
  n_cl = sample(8:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:min(n_vars, 3), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:max(1, dom_size-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [concurrent %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("concurrent-%d", trial))
}
cat(sprintf("  Concurrent elimination: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
