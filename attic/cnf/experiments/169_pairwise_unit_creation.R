#!/usr/bin/env Rscript
# Test scenarios where clauses become units during the pairwise comparison phase.
# This tests:
# - SSE that shortens a clause to length 1 (creating a unit)
# - Domain restriction during pairwise that creates a unit
# - The use_inso optimization in register_unit when is_not_subset_of exists
# - Interaction of new units with available/available_inverse arrays
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

# === SSE shortens clause to unit during pairwise ===
cat("=== SSE creates unit during pairwise ===\n")
set.seed(169001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Design: Two clauses where SSE on one symbol can eliminate another symbol
  # (A in {a,b} | B in {b,c}) and (A in {a,b,c} | B in {d}) -> SSE on A restricts B in second clause
  # If B becomes empty, second clause becomes unit for A
  a1 = sample(dom, sample(2:3, 1))
  a2 = sample(dom, sample(2:3, 1))
  b1 = sample(dom, sample(1:3, 1))
  b2 = sample(dom, sample(1:2, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a2 | B %among% b2),
    as.CnfClause(B %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:2, 1))),
    as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:2, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sse-unit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sse-unit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE creates unit: %d tests, %d failures\n", n_tests, n_failures))

# === Unit creation during pairwise with many clauses ===
cat("\n=== Dense pairwise unit creation ===\n")
set.seed(169002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(3:4, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create many 2-symbol clauses with overlapping symbols
  # This maximizes chances of SSE creating units
  n_cl = sample(6:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), 2)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dense-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dense-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Dense pairwise: %d tests, %d failures\n", n_tests, n_failures))

# === use_inso path: unit created after is_not_subset_of exists ===
cat("\n=== use_inso optimization path ===\n")
set.seed(169003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(3:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Need: no initial units, but pairwise creates units.
  # All clauses must be 2+ symbols.
  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:min(3, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  # Remove any accidental single-symbol clauses (units)
  clauses = clauses[sapply(clauses, function(x) length(unclass(x)) >= 2 || is.logical(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [inso-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [inso-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  use_inso: %d tests, %d failures\n", n_tests, n_failures))

# === Pairwise + subsequent operations ===
cat("\n=== Operations after pairwise simplification ===\n")
set.seed(169004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function(n_cl = NULL) {
    if (is.null(n_cl)) n_cl = sample(2:5, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(2:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f()
  f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  # f1 & f2: combines two simplified formulas
  n_tests = n_tests + 1
  r = tryCatch(f1 & f2, error = function(e) e)
  if (inherits(r, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [op-and-%d]: %s\n", trial, r$message)); next
  }
  tr = evaluate_formula(r, u)
  if (!all(tr == (t1 & t2))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [op-and-%d]: semantic mismatch\n", trial))
  }

  # !(f1 & f2)
  n_tests = n_tests + 1
  r2 = tryCatch(!(f1 & f2), error = function(e) e)
  if (inherits(r2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [op-neg-%d]: %s\n", trial, r2$message)); next
  }
  tr2 = evaluate_formula(r2, u)
  if (!all(tr2 == !(t1 & t2))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [op-neg-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Operations after pairwise: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
