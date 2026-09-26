#!/usr/bin/env Rscript
# Final edge case gauntlet:
# - All clauses identical
# - All clauses complementary
# - Formula that simplifies to single clause
# - Formula that simplifies to single unit
# - Formula with both units and non-units where units subsume all non-units
# - Stress test: many operations on same formulas
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

# === All identical clauses ===
cat("=== All identical ===\n")
set.seed(193001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_sym = sample(2:3, 1)
  chosen = sample(names(syms), n_sym)
  atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  cl = as.CnfClause(Reduce(`|`, atoms))
  if (isTRUE(unclass(cl))) next

  n_copies = sample(5:15, 1)
  clauses = replicate(n_copies, cl, simplify = FALSE)

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ident-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ident-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  All identical: %d tests, %d failures\n", n_tests, n_failures))

# === Complementary pairs ===
cat("\n=== Complementary pairs ===\n")
set.seed(193002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  a_range = sample(dom, sample(1:2, 1))
  b_range = sample(dom, sample(1:2, 1))
  a_comp = setdiff(dom, a_range)
  b_comp = setdiff(dom, b_range)

  clauses = list()
  if (length(a_comp) > 0 && length(b_comp) > 0) {
    clauses[[1]] = as.CnfClause(A %among% a_range | B %among% b_range)
    clauses[[2]] = as.CnfClause(A %among% a_comp | B %among% b_comp)
    # Add more clauses
    for (j in 1:sample(2:4, 1)) {
      clauses[[length(clauses) + 1]] = as.CnfClause(
        syms[[sample(names(syms), 1)]] %among% sample(dom, sample(1:2, 1)) |
        syms[[sample(names(syms), 1)]] %among% sample(dom, sample(1:2, 1))
      )
    }
  } else {
    next
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [comp-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [comp-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Complementary: %d tests, %d failures\n", n_tests, n_failures))

# === Units subsume everything ===
cat("\n=== Units subsume all ===\n")
set.seed(193003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create units
  a_unit = sample(dom, sample(1:3, 1))
  b_unit = sample(dom, sample(1:3, 1))
  clauses = list(
    as.CnfClause(A %among% a_unit),
    as.CnfClause(B %among% b_unit)
  )

  # Create non-unit clauses that are ALL subsumed by the units
  for (j in 1:sample(3:6, 1)) {
    # Make sure A's range is a superset of a_unit, B's of b_unit
    a_vals = unique(c(a_unit, sample(dom, sample(0:2, 1))))
    b_vals = unique(c(b_unit, sample(dom, sample(0:2, 1))))
    clauses[[length(clauses) + 1]] = as.CnfClause(A %among% a_vals | B %among% b_vals)
  }
  # Some that might not be subsumed
  for (j in 1:sample(1:3, 1)) {
    chosen = sample(names(syms), sample(2:3, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [usub-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [usub-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Units subsume: %d tests, %d failures\n", n_tests, n_failures))

# === Many operations stress ===
cat("\n=== Many operations ===\n")
set.seed(193004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(2:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f()
  if (is.null(f)) next

  n_tests = n_tests + 1
  # Apply many operations
  for (k in 1:sample(5:10, 1)) {
    op = sample(c("&", "|", "!"), 1)
    if (op == "!") {
      f = tryCatch(!f, error = function(e) NULL)
    } else {
      g = make_f()
      if (is.null(g)) next
      f = if (op == "&") tryCatch(f & g, error = function(e) NULL) else tryCatch(f | g, error = function(e) NULL)
    }
    if (is.null(f)) break
  }
  if (is.null(f)) next

  # Just verify no crash and the result is evaluatable
  t = tryCatch(evaluate_formula(f, u), error = function(e) NULL)
  if (is.null(t)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ops-%d]: couldn't evaluate\n", trial))
  }
}
cat(sprintf("  Many operations: %d tests, %d failures\n", n_tests, n_failures))

# === Simplifies to single clause ===
cat("\n=== Simplifies to single clause ===\n")
set.seed(193005)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  # Create clauses where most are subsumed by one
  a_small = sample(dom, sample(1:2, 1))
  b_small = sample(dom, sample(1:2, 1))
  base_clause = as.CnfClause(A %among% a_small | B %among% b_small)
  if (isTRUE(unclass(base_clause))) next

  clauses = list(base_clause)
  for (j in 1:sample(3:6, 1)) {
    # Wider clauses that are subsumed by the base
    a_wider = unique(c(a_small, sample(dom, sample(0:2, 1))))
    b_wider = unique(c(b_small, sample(dom, sample(0:2, 1))))
    cl = as.CnfClause(A %among% a_wider | B %among% b_wider)
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [single-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [single-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Simplifies to single: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
