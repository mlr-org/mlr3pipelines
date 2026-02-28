#!/usr/bin/env Rscript
# Test the is_unit_propagation=FALSE path in apply_domain_restriction (lines 171-185).
# This path runs during SSE (self-subsumption elimination) when a non-unit clause
# restricts another clause. It updates the "reverse" direction of is_not_subset_of.
# We construct formulas that specifically trigger SSE chains where the reverse
# update matters for correctness.
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

# === SSE creates non-unit restriction that triggers reverse update ===
cat("=== SSE reverse update patterns ===\n")
set.seed(144001)

# Constructed pattern:
# Clause 1 (short): A in {a} | B in {b}
# Clause 2 (longer): A in {a,b} | B in {b,c} | C in {c}
# Clause 3 depends on A: A in {a,c} | C in {c,d}
# SSE between clause 1 and clause 2 on B should trigger reverse update
# for clause 3's relationship with the modified clause 2.
for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Random ranges but structured to create SSE opportunities
  a1 = sample(dom, sample(1:3, 1))
  b1 = sample(dom, sample(1:3, 1))
  a2 = sample(dom, sample(1:3, 1))
  b2 = sample(dom, sample(1:3, 1))
  c2 = sample(dom, sample(1:3, 1))
  a3 = sample(dom, sample(1:3, 1))
  c3 = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a2 | B %among% b2 | C %among% c2),
    as.CnfClause(A %among% a3 | C %among% c3)
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sse-rev-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sse-rev-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE reverse update: %d tests, %d failures\n", n_tests, n_failures))

# === SSE chains that cascade through reverse updates ===
cat("\n=== SSE chain with reverse cascade ===\n")
set.seed(144002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  # Many clauses sharing symbols, creating SSE chains
  n_cl = sample(4:8, 1)
  syms = list(A = A, B = B, C = C, D = D)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sse-chain-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sse-chain-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE chain reverse: %d tests, %d failures\n", n_tests, n_failures))

# === Non-unit SSE with overlapping symbol registries ===
cat("\n=== Non-unit SSE overlapping registries ===\n")
set.seed(144003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(3:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create clauses where each pair of symbols appears in at least 2 clauses
  # This maximizes the chance of reverse updates being needed
  n_cl = sample(6:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:min(3, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [overlap-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [overlap-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Overlapping registries: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
