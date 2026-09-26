#!/usr/bin/env Rscript
# Incremental formula building:
# - Build formulas clause by clause using & operator
# - Compare result of building incrementally vs all-at-once
# - Test that intermediate formulas are correct
# This exercises the & operator's interaction with simplify_cnf
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

# === Pattern 1: Incremental vs batch ===
cat("=== Incremental vs batch ===\n")
set.seed(189001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  # Build all at once
  batch = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(batch)) next

  # Build incrementally
  incremental = as.CnfFormula(clauses[[1]])
  ok = TRUE
  for (i in 2:length(clauses)) {
    incremental = tryCatch(incremental & as.CnfFormula(clauses[[i]]), error = function(e) NULL)
    if (is.null(incremental)) { ok = FALSE; break }
  }
  if (!ok) next

  n_tests = n_tests + 1
  t_batch = evaluate_formula(batch, u)
  t_incr = evaluate_formula(incremental, u)
  if (!all(t_batch == t_incr)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [incr-%d]: incremental != batch\n", trial))
  }
}
cat(sprintf("  Incremental vs batch: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Incremental with intermediate checks ===
cat("\n=== Incremental with checks ===\n")
set.seed(189002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  # Build incrementally, checking each step
  current = as.CnfFormula(clauses[[1]])
  all_ok = TRUE
  for (i in 2:length(clauses)) {
    current = tryCatch(current & as.CnfFormula(clauses[[i]]), error = function(e) NULL)
    if (is.null(current)) { all_ok = FALSE; break }

    n_tests = n_tests + 1
    # Check intermediate result
    raw_truth = evaluate_raw_clauses(clauses[1:i], u)
    curr_truth = evaluate_formula(current, u)
    if (!all(curr_truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [check-%d-step%d]: intermediate mismatch\n", trial, i))
      all_ok = FALSE
      break
    }
  }
}
cat(sprintf("  Incremental checks: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Incremental OR building ===
cat("\n=== Incremental OR ===\n")
set.seed(189003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create 3-4 small formulas
  n_formulas = sample(3:4, 1)
  formulas = list()
  for (i in 1:n_formulas) {
    n_cl = sample(2:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    f = if (length(cls) >= 1) tryCatch(CnfFormula(cls), error = function(e) NULL) else NULL
    if (!is.null(f)) formulas[[length(formulas) + 1]] = f
  }
  if (length(formulas) < 3) next

  # Build f1 | f2 | f3 incrementally
  n_tests = n_tests + 1
  result = formulas[[1]]
  for (i in 2:length(formulas)) {
    result = tryCatch(result | formulas[[i]], error = function(e) NULL)
    if (is.null(result)) break
  }
  if (is.null(result)) next

  # Calculate expected
  expected = evaluate_formula(formulas[[1]], u)
  for (i in 2:length(formulas)) {
    expected = expected | evaluate_formula(formulas[[i]], u)
  }

  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ior-%d]: incremental OR mismatch\n", trial))
  }
}
cat(sprintf("  Incremental OR: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Mixed incremental AND/OR ===
cat("\n=== Mixed incremental ===\n")
set.seed(189004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create 4-6 leaf formulas
  n_leaves = sample(4:6, 1)
  leaves = list()
  for (i in 1:n_leaves) {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    f = if (length(cls) >= 1) tryCatch(CnfFormula(cls), error = function(e) NULL) else NULL
    if (!is.null(f)) leaves[[length(leaves) + 1]] = f
  }
  if (length(leaves) < 4) next

  # Build: (f1 & f2) | (f3 & f4)
  n_tests = n_tests + 1
  f12 = tryCatch(leaves[[1]] & leaves[[2]], error = function(e) NULL)
  f34 = tryCatch(leaves[[3]] & leaves[[4]], error = function(e) NULL)
  if (is.null(f12) || is.null(f34)) next
  result = tryCatch(f12 | f34, error = function(e) NULL)
  if (is.null(result)) next

  t1 = evaluate_formula(leaves[[1]], u)
  t2 = evaluate_formula(leaves[[2]], u)
  t3 = evaluate_formula(leaves[[3]], u)
  t4 = evaluate_formula(leaves[[4]], u)
  expected = (t1 & t2) | (t3 & t4)
  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixed-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Mixed incremental: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
