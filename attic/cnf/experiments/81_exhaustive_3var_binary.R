#!/usr/bin/env Rscript
# Exhaustive test: 3 binary variables, all possible 2-clause and 3-clause combinations.
# This is a complete enumeration to catch any remaining edge cases.
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

u = CnfUniverse()
A = CnfSymbol(u, "A", c("0", "1"))
B = CnfSymbol(u, "B", c("0", "1"))
C = CnfSymbol(u, "C", c("0", "1"))

# Generate ALL possible non-trivial clauses for 3 binary variables
# A clause is a named list of value sets. For each variable, either:
# - not present (any value OK)
# - "0" only
# - "1" only
# (value set of both values = tautological for that var, effectively absent)
# So for each var: 3 states (absent, "0", "1")
# But at least one var must be present (otherwise it's TRUE/tautology)
# 3^3 - 1 = 26 possible non-trivial clauses

all_clauses = list()
syms = list(A = "A", B = "B", C = "C")
dom = c("0", "1")

states = list(list(), list("0"), list("1"))  # absent, 0, 1
for (sa in 1:3) {
  for (sb in 1:3) {
    for (sc in 1:3) {
      if (sa == 1 && sb == 1 && sc == 1) next  # all absent = TRUE, skip
      cl = list()
      if (sa > 1) cl[["A"]] = states[[sa]][[1]]
      if (sb > 1) cl[["B"]] = states[[sb]][[1]]
      if (sc > 1) cl[["C"]] = states[[sc]][[1]]
      # Convert to CnfClause
      atoms = lapply(names(cl), function(nm) {
        get(nm, envir = environment()) # this won't work, need to use the symbols directly
      })
      # Actually, build clause from raw list
      clause = structure(cl, universe = u, class = "CnfClause")
      all_clauses[[length(all_clauses) + 1]] = clause
    }
  }
}
n_clauses = length(all_clauses)
cat(sprintf("Generated %d possible clauses\n", n_clauses))

# === All 2-clause combinations ===
cat("\n=== All 2-clause combinations ===\n")
for (i in 1:n_clauses) {
  for (j in i:n_clauses) {
    clauses = list(all_clauses[[i]], all_clauses[[j]])
    f = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(f, "error")) {
      n_tests = n_tests + 1; n_failures = n_failures + 1
      next
    }
    check_fc(f, clauses, u, sprintf("2cl-%d-%d", i, j))
  }
}
cat(sprintf("  2-clause: %d tests, %d failures\n", n_tests, n_failures))

# === Random 3-clause combinations ===
cat("\n=== Random 3-clause combinations ===\n")
set.seed(81001)

for (trial in 1:5000) {
  indices = sort(sample(n_clauses, 3, replace = TRUE))
  clauses = list(all_clauses[[indices[1]]], all_clauses[[indices[2]]], all_clauses[[indices[3]]])

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    next
  }
  check_fc(f, clauses, u, sprintf("3cl-%d", trial))
}
cat(sprintf("  3-clause: %d tests, %d failures\n", n_tests, n_failures))

# === Random 4-5 clause combinations ===
cat("\n=== Random 4-5 clause combinations ===\n")
set.seed(81002)

for (trial in 1:3000) {
  n_cl = sample(4:5, 1)
  indices = sort(sample(n_clauses, n_cl, replace = TRUE))
  clauses = lapply(indices, function(i) all_clauses[[i]])

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    next
  }
  check_fc(f, clauses, u, sprintf("45cl-%d", trial))
}
cat(sprintf("  4-5 clause: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
