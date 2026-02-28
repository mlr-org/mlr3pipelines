#!/usr/bin/env Rscript
# Targeted HLA testing: construct formulas that specifically trigger
# hidden tautology and hidden subsumption elimination paths.
# Also test unit HLA phase specifically.
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

# === HLA hidden tautology: non-unit phase ===
cat("=== HLA hidden tautology (non-unit) ===\n")
set.seed(136001)

# Construct: clause C is NOT a tautology, but after HLA from other clauses it becomes one
for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Target clause (candidate for hidden tautology)
  a_target = sample(dom, sample(1:3, 1))
  b_target = sample(dom, sample(1:3, 1))
  c_target = sample(dom, sample(1:3, 1))

  # HLA source clauses: each shares all-but-one symbol with target, contributing complement
  # Source 1: has A, B subset of target -> adds complement of C to target
  a1 = sample(a_target, min(length(a_target), sample(1:length(a_target), 1)))
  b1 = sample(b_target, min(length(b_target), sample(1:length(b_target), 1)))
  c1 = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a_target | B %among% b_target | C %among% c_target),
    as.CnfClause(A %among% a1 | B %among% b1 | C %among% c1),
    as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1)))
  )
  # Add more clauses to keep it interesting
  for (j in 1:sample(1:3, 1)) {
    clauses[[length(clauses) + 1]] = as.CnfClause(
      A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1))
    )
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [htaut-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [htaut-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA hidden taut (non-unit): %d tests, %d failures\n", n_tests, n_failures))

# === HLA hidden subsumption: non-unit phase ===
cat("\n=== HLA hidden subsumption (non-unit) ===\n")
set.seed(136002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Target clause (candidate for hidden subsumption)
  a_target = sample(dom, sample(2:3, 1))
  b_target = sample(dom, sample(2:3, 1))
  c_target = sample(dom, sample(2:3, 1))

  # Subsumer clause: has subset ranges for all symbols except one
  a_sub = sample(a_target, sample(1:length(a_target), 1))
  b_sub = sample(b_target, sample(1:length(b_target), 1))
  c_sub = sample(dom, sample(1:3, 1))  # not necessarily subset

  # HLA source: enables the subsumption by adding complement to target
  a_hla = sample(a_target, min(length(a_target), sample(1:length(a_target), 1)))
  b_hla = sample(b_target, min(length(b_target), sample(1:length(b_target), 1)))

  clauses = list(
    as.CnfClause(A %among% a_target | B %among% b_target | C %among% c_target),
    as.CnfClause(A %among% a_sub | B %among% b_sub | C %among% c_sub),
    as.CnfClause(A %among% a_hla | B %among% b_hla | C %among% sample(dom, sample(1:3, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [hsub-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hsub-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA hidden sub (non-unit): %d tests, %d failures\n", n_tests, n_failures))

# === HLA unit phase: hidden tautology ===
cat("\n=== HLA hidden tautology (unit phase) ===\n")
set.seed(136003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Unit clause for A
  a_unit = sample(dom, sample(1:3, 1))

  # Non-unit clauses that will serve as HLA sources
  # Source with A and B, where A is subset of unit domain -> adds complement of B
  # Source with A and C, where A is subset of unit domain -> adds complement of C
  b_src = sample(dom, sample(1:3, 1))
  c_src = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a_unit),
    as.CnfClause(A %among% sample(a_unit, min(length(a_unit), sample(1:length(a_unit), 1))) | B %among% b_src),
    as.CnfClause(A %among% sample(a_unit, min(length(a_unit), sample(1:length(a_unit), 1))) | C %among% c_src),
    as.CnfClause(B %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [uhla-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [uhla-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA unit taut: %d tests, %d failures\n", n_tests, n_failures))

# === Random formulas that stress HLA ===
cat("\n=== Random HLA stress ===\n")
set.seed(136004)

for (trial in 1:1000) {
  n_vars = sample(2:4, 1)
  dom_size = sample(3:5, 1)
  u = CnfUniverse()
  dom = paste0("v", 1:dom_size)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Generate 4-8 clauses with 2-3 symbols each (good for HLA)
  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:min(3, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:(dom_size - 1), 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [rand-hla-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [rand-hla-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Random HLA stress: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
