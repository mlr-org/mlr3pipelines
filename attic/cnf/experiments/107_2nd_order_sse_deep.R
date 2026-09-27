#!/usr/bin/env Rscript
# Deep test of 2nd-order SSE:
# - Specifically constructed patterns that should trigger handle_sse_2nd_order_oneend
# - Patterns that trigger handle_sse_2nd_order_twoend
# - Cascading 2nd-order SSE (one SSE triggers another)
# - try_sse_2nd_order disjointness check
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

# === Constructed 2nd-order SSE: oneend pattern ===
cat("=== 2nd-order SSE oneend ===\n")
set.seed(107001)

# Pattern for 2nd-order SSE (oneend):
# Clause C1 is subset of target C3 on all except symbol S.
# Clause C2 is subset of target C3 on all except symbols S and T.
# C1 and C2 are disjoint on S outside C3.
# Then target C3 can have T restricted to union of C1[T] and C2[T].

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("0", "1", "2", "3")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Make specific patterns
  a1 = sample(dom, sample(1:3, 1))
  a2 = sample(dom, sample(1:3, 1))
  b_vals = sample(dom, sample(1:3, 1))
  c_vals = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b_vals),
    as.CnfClause(A %among% a2 | B %among% b_vals | C %among% c_vals),
    as.CnfClause(A %among% dom | B %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1)))
  )
  # Filter out tautological clauses
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [oneend-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [oneend-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2nd-order oneend: %d tests, %d failures\n", n_tests, n_failures))

# === Constructed 2nd-order SSE: twoend pattern ===
cat("\n=== 2nd-order SSE twoend ===\n")
set.seed(107002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("0", "1", "2", "3")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  # twoend: meta_idx is not subset of target on 2 symbols
  a_vals = sample(dom, sample(1:3, 1))
  b_vals = sample(dom, sample(1:3, 1))
  c_vals = sample(dom, sample(1:3, 1))
  d_vals = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a_vals | B %among% b_vals),
    as.CnfClause(A %among% sample(dom, sample(2:3, 1)) | C %among% c_vals | D %among% d_vals),
    as.CnfClause(B %among% sample(dom, sample(2:3, 1)) | C %among% sample(dom, sample(1:3, 1)) | D %among% sample(dom, sample(1:3, 1))),
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | B %among% sample(dom, sample(1:2, 1)) | C %among% sample(dom, sample(1:3, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [twoend-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [twoend-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2nd-order twoend: %d tests, %d failures\n", n_tests, n_failures))

# === Random formulas designed to trigger 2nd-order SSE ===
cat("\n=== Random 2nd-order SSE patterns ===\n")
set.seed(107003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom_size = sample(3:5, 1)
  d = paste0("v", 1:dom_size)
  n_vars = sample(3:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  # 4-8 clauses with 2-3 symbols each (sweet spot for 2nd-order SSE)
  n_clauses = sample(4:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(2:min(3, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      n_vals = sample(1:(dom_size - 1), 1)
      syms[[s]] %among% sample(d, n_vals)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [rand2nd-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [rand2nd-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Random 2nd-order: %d tests, %d failures\n", n_tests, n_failures))

# === Disjointness requirement stress ===
cat("\n=== Disjointness stress ===\n")
set.seed(107004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Create formulas where disjointness check matters:
  # Multiple clauses with overlapping ranges on the intersection symbol
  n_clauses = sample(5:10, 1)
  clauses = lapply(1:n_clauses, function(j) {
    a_vals = sample(dom, sample(1:4, 1))
    b_vals = sample(dom, sample(1:4, 1))
    c_vals = sample(dom, sample(1:4, 1))
    chosen = sample(1:3, sample(2:3, 1))
    atoms = list()
    if (1 %in% chosen) atoms = c(atoms, list(A %among% a_vals))
    if (2 %in% chosen) atoms = c(atoms, list(B %among% b_vals))
    if (3 %in% chosen) atoms = c(atoms, list(C %among% c_vals))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [disj-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [disj-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Disjointness stress: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
