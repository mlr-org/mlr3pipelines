#!/usr/bin/env Rscript
# Test eliminate_clause_update_sr interactions:
# - Clause eliminated during symbol_registry iteration
# - Multiple clauses eliminated in cascade
# - Eliminated clause's symbols properly cleaned from registry
# Also: meta_idx_outer boundary conditions
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

# === Many clauses with shared symbols -> dense elimination ===
cat("=== Dense symbol sharing ===\n")
set.seed(174001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  # Many clauses sharing the same two symbols -> many subsumption opportunities
  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

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
cat(sprintf("  Dense sharing: %d tests, %d failures\n", n_tests, n_failures))

# === Subsumption cascade: one elimination enables another ===
cat("\n=== Subsumption cascade ===\n")
set.seed(174002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create clauses where SSE on one pair enables subsumption on another pair
  a1 = sample(dom, 2)
  b1 = sample(dom, 2)
  b2 = sample(dom, sample(1:2, 1))
  c1 = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a1 | B %among% b2),  # SSE can restrict B
    as.CnfClause(B %among% sample(dom, sample(1:3, 1)) | C %among% c1),
    as.CnfClause(A %among% sample(dom, sample(2:3, 1)) | C %among% sample(dom, sample(1:2, 1)))
  )
  # Add extra clauses
  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(2:3, 1)
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
    cat(sprintf("ERROR [cascade-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cascade-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Subsumption cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Many variables with few clauses (sparse) ===
cat("\n=== Sparse (many vars, few clauses) ===\n")
set.seed(174003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b")
  n_vars = sample(5:8, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create 3-5 clauses each with 2-3 symbols
  n_cl = sample(3:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sparse-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sparse-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Sparse: %d tests, %d failures\n", n_tests, n_failures))

# === Dense (few vars, many clauses) ===
cat("\n=== Dense (few vars, many clauses) ===\n")
set.seed(174004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  n_cl = sample(10:25, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dense2v-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dense2v-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Dense 2-var: %d tests, %d failures\n", n_tests, n_failures))

# === Idempotence: simplify(simplify(f)) == simplify(f) ===
cat("\n=== Simplification idempotence ===\n")
set.seed(174005)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f) || is.logical(unclass(f))) next

  # Re-simplify
  n_tests = n_tests + 1
  f2 = tryCatch(CnfFormula(as.list(f)), error = function(e) e)
  if (inherits(f2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [idemp-%d]: %s\n", trial, f2$message)); next
  }
  t1 = evaluate_formula(f, u)
  t2 = evaluate_formula(f2, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [idemp-%d]: re-simplified differs semantically\n", trial))
  }
}
cat(sprintf("  Idempotence: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
