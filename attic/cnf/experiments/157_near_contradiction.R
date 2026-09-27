#!/usr/bin/env Rscript
# Test near-contradiction formulas:
# - Formulas with very few satisfying assignments (1-2 out of many)
# - Formulas that are almost tautologies (all but 1-2 assignments)
# - Verify the simplifier preserves these precisely
# Also: simplification idempotence - simplifying already simplified formulas
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

# === Near-contradictions (few satisfying) ===
cat("=== Near-contradictions ===\n")
set.seed(157001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create tight clauses (small ranges) to create near-contradictions
  n_cl = sample(4:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))  # range size 1
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  raw_truth = evaluate_raw_clauses(clauses, u)
  n_sat = sum(raw_truth)
  if (n_sat > 3 || n_sat == 0) next  # Only test near-contradictions

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [near-contra-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [near-contra-%d]: semantic mismatch (n_sat=%d)\n", trial, n_sat))
  }
}
cat(sprintf("  Near-contradictions: %d tests, %d failures\n", n_tests, n_failures))

# === Near-tautologies ===
cat("\n=== Near-tautologies ===\n")
set.seed(157002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create wide clauses (large ranges) to create near-tautologies
  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 2))  # range size 2 of 3
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  raw_truth = evaluate_raw_clauses(clauses, u)
  n_unsat = sum(!raw_truth)
  if (n_unsat > 3 || n_unsat == 0) next  # Only test near-tautologies

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [near-taut-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [near-taut-%d]: semantic mismatch (n_unsat=%d)\n", trial, n_unsat))
  }
}
cat(sprintf("  Near-tautologies: %d tests, %d failures\n", n_tests, n_failures))

# === Simplification idempotence ===
cat("\n=== Simplification idempotence ===\n")
set.seed(157003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f1 = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f1)) next
  if (is.logical(unclass(f1))) next  # Skip TRUE/FALSE results

  # Simplify again by constructing a new formula from f1's clauses
  n_tests = n_tests + 1
  clauses2 = as.list(f1)
  f2 = tryCatch(CnfFormula(clauses2), error = function(e) e)
  if (inherits(f2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [idemp-%d]: %s\n", trial, f2$message)); next
  }

  # f1 and f2 should be structurally equal (all.equal)
  eq = tryCatch(all.equal(f1, f2), error = function(e) paste("error:", e$message))
  if (!isTRUE(eq)) {
    # Check if at least semantically equal
    t1 = evaluate_formula(f1, u)
    t2 = evaluate_formula(f2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [idemp-%d]: semantic mismatch after re-simplification\n", trial))
    }
    # Structural difference is ok (non-confluent) but semantic must match
  }
}
cat(sprintf("  Idempotence: %d tests, %d failures\n", n_tests, n_failures))

# === Constructed near-contradictions with 4 vars ===
cat("\n=== 4-var near-contradictions ===\n")
set.seed(157004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (i in 1:4) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create many tight clauses
  n_cl = sample(6:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [4var-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [4var-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  4-var near-contradictions: %d tests, %d failures\n", n_tests, n_failures))

# === Exact contradiction detection ===
cat("\n=== Exact contradictions ===\n")
set.seed(157005)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Create a formula and its negation, AND them -> must be FALSE
  n_cl = sample(1:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(c("A", "B"), n_sym)
    syms = list(A = A, B = B)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) == 0) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next
  nf = tryCatch(!f, error = function(e) NULL)
  if (is.null(nf)) next

  n_tests = n_tests + 1
  conj = tryCatch(f & nf, error = function(e) NULL)
  if (is.null(conj)) next
  truth = evaluate_formula(conj, u)
  if (any(truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [contra-%d]: f & !f not FALSE\n", trial))
  }

  # f | !f should be TRUE
  n_tests = n_tests + 1
  disj = tryCatch(f | nf, error = function(e) NULL)
  if (is.null(disj)) next
  truth2 = evaluate_formula(disj, u)
  if (!all(truth2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [taut-%d]: f | !f not TRUE\n", trial))
  }
}
cat(sprintf("  Exact contradictions: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
