#!/usr/bin/env Rscript
# Test paths that lead to contradictions (simplify -> FALSE):
# - Conflicting units: A in {a} AND A in {b}
# - Unit propagation empties a clause
# - SSE creates contradicting units
# - Chain of restrictions leads to empty range
# - Near-contradictions that don't quite contradict
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

# === Direct contradicting units ===
cat("=== Direct contradicting units ===\n")
set.seed(124001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Two units with disjoint values for same symbol
  a1 = sample(dom, sample(1:3, 1))
  a2 = sample(setdiff(dom, a1), min(sample(1:2, 1), length(setdiff(dom, a1))))
  if (length(a2) == 0) next

  clauses = list(
    as.CnfClause(A %among% a1),
    as.CnfClause(A %among% a2)
  )
  # Add some noise clauses
  for (j in seq_len(sample(0:3, 1))) {
    clauses[[length(clauses) + 1]] = as.CnfClause(
      A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1))
    )
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [direct-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [direct-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Direct contradictions: %d tests, %d failures\n", n_tests, n_failures))

# === Unit propagation empties a clause ===
cat("\n=== Unit empties clause ===\n")
set.seed(124002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Unit: A in {0}
  # Clause: A in {1,2} | B in {vals} - A range gets emptied by unit, B remains
  # Or: A in {1,2} - gets emptied entirely -> contradiction
  unit_val = sample(dom, 1)
  other_vals = setdiff(dom, unit_val)

  clauses = list(as.CnfClause(A %among% unit_val))
  n_extra = sample(2:5, 1)
  for (j in 1:n_extra) {
    if (runif(1) > 0.3) {
      clauses[[length(clauses) + 1]] = as.CnfClause(A %among% sample(other_vals, sample(1:length(other_vals), 1)))
    } else {
      syms = list(A = A, B = B, C = C)
      chosen = sample(names(syms), sample(2:3, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
    }
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [unit-empty-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unit-empty-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Unit empties: %d tests, %d failures\n", n_tests, n_failures))

# === SSE creates contradicting units ===
cat("\n=== SSE creates contradicting units ===\n")
set.seed(124003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # SSE can shorten a clause to a unit
  # If that unit contradicts an existing unit, we get FALSE
  # e.g. A in {a} | B in {b}  AND  A in {a} | B in {a}
  # SSE: B in {b} intersect {a} = {} -> A unit, but A in {a} exists
  a_val = sample(dom, 1)
  b_val1 = sample(dom, 1)
  b_val2 = sample(setdiff(dom, b_val1), min(1, length(setdiff(dom, b_val1))))
  if (length(b_val2) == 0) next

  clauses = list(
    as.CnfClause(A %among% a_val | B %among% b_val1),
    as.CnfClause(A %among% a_val | B %among% b_val2)
  )
  # Add the contradiction trigger
  clauses[[3]] = as.CnfClause(A %among% sample(setdiff(dom, a_val), min(1, length(setdiff(dom, a_val)))))
  if (isTRUE(unclass(clauses[[3]])) || length(setdiff(dom, a_val)) == 0) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sse-contr-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sse-contr-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE contradictions: %d tests, %d failures\n", n_tests, n_failures))

# === Random formulas biased toward contradictions ===
cat("\n=== Random near-contradictions ===\n")
set.seed(124004)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("0", "1")  # binary domain makes contradictions more likely
  n_vars = sample(3:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Many unit clauses (high contradiction probability)
  n_clauses = sample(4:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    if (runif(1) > 0.4) {
      # unit clause
      s = sample(names(syms), 1)
      as.CnfClause(syms[[s]] %among% sample(dom, 1))
    } else {
      n_sym = sample(1:min(3, n_vars), 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
      as.CnfClause(Reduce(`|`, atoms))
    }
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [near-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [near-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Near-contradictions: %d tests, %d failures\n", n_tests, n_failures))

# === All-unit formulas ===
cat("\n=== All-unit formulas ===\n")
set.seed(124005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  n_vars = sample(2:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("U", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # All clauses are units
  n_clauses = sample(2:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    s = sample(names(syms), 1)
    as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [allunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [allunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  All-unit: %d tests, %d failures\n", n_tests, n_failures))

# === Formulas that simplify to exactly TRUE ===
cat("\n=== Simplify to TRUE ===\n")
set.seed(124006)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("x", "y", "z")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Create formulas guaranteed to be tautological
  # Strategy: create clauses that each cover everything
  clauses = lapply(1:sample(2:5, 1), function(j) {
    # A covers some, B covers rest
    a_vals = sample(dom, sample(1:2, 1))
    b_vals = dom  # full domain
    as.CnfClause(A %among% a_vals | B %among% b_vals)
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [true-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  if (!all(truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [true-%d]: expected all TRUE\n", trial))
  }
}
cat(sprintf("  Simplify to TRUE: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
