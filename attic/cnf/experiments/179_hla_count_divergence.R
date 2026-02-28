#!/usr/bin/env Rscript
# Test HLA not_subset_count_current divergence from is_not_subset_of matrix:
# - During HLA loop, apply_domain_restriction can modify is_not_subset_of
#   for clauses that are also in remaining_other_entries, but
#   not_subset_count_current won't be updated correspondingly
# - This could cause colnames()[TRUE_mask] to return multiple symbols
#   when count says 1
# Strategy: create formulas where HLA complement addition triggers
# cascading SSE that modifies another clause's is_not_subset_of entry
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

# === Pattern 1: Dense 3-var clauses, many HLA opportunities ===
cat("=== Dense HLA opportunities ===\n")
set.seed(179001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Many 2-symbol clauses with narrow ranges: maximize not_subset_count == 1 (HLA candidate)
  n_cl = sample(6:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
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
    cat(sprintf("ERROR [hla1-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hla1-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Dense HLA: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Many similar clauses differing in one symbol ===
# This creates many not_subset_count == 1 pairs (HLA precondition)
cat("\n=== Many HLA-1 pairs ===\n")
set.seed(179002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Base clause with 3 symbols
  base_syms = sample(names(syms), 3)
  base_ranges = lapply(base_syms, function(s) sample(dom, sample(2:3, 1)))
  names(base_ranges) = base_syms

  clauses = list()
  # Create variants that differ in exactly one symbol's range
  for (j in 1:sample(4:8, 1)) {
    sym_to_vary = sample(base_syms, 1)
    variant_ranges = base_ranges
    variant_ranges[[sym_to_vary]] = sample(dom, sample(1:3, 1))
    atoms = lapply(names(variant_ranges), function(s) syms[[s]] %among% variant_ranges[[s]])
    clauses[[j]] = as.CnfClause(Reduce(`|`, atoms))
  }
  # Add some extra independent clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [hla2-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hla2-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Many HLA-1 pairs: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Clauses designed to trigger HLA complement then SSE cascade ===
cat("\n=== HLA + SSE cascade ===\n")
set.seed(179003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # HLA-friendly: clause pair where one is a subset of the other except for one symbol
  a1 = sample(dom, 2)
  b1 = sample(dom, 2)
  c1 = sample(dom, 2)
  c2 = sample(dom, 3)

  clauses = list(
    # Clause 1 and 2 differ only in C: HLA can extend C's range in clause 1
    as.CnfClause(A %among% a1 | B %among% b1 | C %among% c1),
    as.CnfClause(A %among% a1 | B %among% b1 | C %among% c2)
  )
  # Add clauses that can interact with the extended C range
  for (j in 1:sample(3:6, 1)) {
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
    cat(sprintf("ERROR [hla3-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hla3-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA + SSE cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: 4 variables, deep HLA chains ===
cat("\n=== Deep HLA chains 4v ===\n")
set.seed(179004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Create clauses with 3-4 symbols and narrow ranges
  n_cl = sample(8:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(3:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 6) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [hla4-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hla4-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Deep HLA 4v: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
