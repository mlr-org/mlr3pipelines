#!/usr/bin/env Rscript
# Boundary satisfiability formulas:
# - Exactly one satisfying assignment
# - Exactly two satisfying assignments
# - All-but-one satisfying
# - Pigeonhole-like constraints
# These stress the simplifier with near-boundary formulas
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

evaluate_raw_clauses = function(clauses, universe) {
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }
  raw_truth
}

# === Pattern 1: Single-satisfying-assignment formulas ===
cat("=== Single satisfying assignment ===\n")
set.seed(188001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Pick a single satisfying assignment
  target = sapply(names(syms), function(s) sample(dom, 1))

  # Create clauses that all include the target but exclude many others
  clauses = list()
  for (j in 1:sample(5:10, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      # Include the target value plus some random others
      vals = unique(c(target[s], sample(dom, sample(0:1, 1))))
      syms[[s]] %among% vals
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sing-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sing-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Single satisfying: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Exactly k satisfying assignments ===
cat("\n=== Exactly k satisfying ===\n")
set.seed(188002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  # 9 total assignments (3x3), pick 2-4 to satisfy
  k = sample(2:4, 1)
  all_assignments = expand.grid(A = dom, B = dom, stringsAsFactors = FALSE)
  sat_idx = sample(nrow(all_assignments), k)

  # Create clauses that are consistent with satisfying assignments
  clauses = list()
  for (j in 1:sample(4:8, 1)) {
    # Each clause must be true for all satisfying assignments
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    # Find values that make at least one literal true for each satisfying row
    atoms = lapply(chosen, function(s) {
      col_idx = which(names(all_assignments) == s)
      possible_vals = unique(all_assignments[sat_idx, col_idx])
      # Include some but not necessarily all of domain
      vals = sample(dom, sample(max(1, length(possible_vals)):length(dom), 1))
      # Make sure all satisfying assignments have at least one true literal
      syms[[s]] %among% vals
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ksat-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ksat-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Exactly k sat: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Pigeonhole-like constraints ===
cat("\n=== Pigeonhole-like ===\n")
set.seed(188003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Constraints like "A and B can't both be 'a'"
  # Expressed as: not(A=a & B=a) = (A in {b,c,d}) | (B in {b,c,d})
  clauses = list()
  for (j in 1:sample(4:8, 1)) {
    s1 = sample(names(syms), 1)
    s2 = sample(setdiff(names(syms), s1), 1)
    val = sample(dom, 1)
    # "s1 and s2 can't both be val"
    other_vals = setdiff(dom, val)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s1]] %among% other_vals | syms[[s2]] %among% other_vals)
  }
  # Add some positive constraints
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [pigeon-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [pigeon-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Pigeonhole: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: At-most-one constraints ===
cat("\n=== At-most-one constraints ===\n")
set.seed(188004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # "At most one of {A,B,C} can be 'a'"
  # Pairwise: for each pair (X,Y), not(X=a & Y=a)
  clauses = list()
  syms_to_constrain = sample(names(syms), 3)
  val = sample(dom, 1)
  other_vals = setdiff(dom, val)
  for (i in 1:2) {
    for (j in (i+1):3) {
      clauses[[length(clauses) + 1]] = as.CnfClause(
        syms[[syms_to_constrain[i]]] %among% other_vals |
        syms[[syms_to_constrain[j]]] %among% other_vals
      )
    }
  }
  # Additional random clauses
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [amo-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [amo-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  At-most-one: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
