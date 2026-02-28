#!/usr/bin/env Rscript
# on_update_range cascade during apply_domain_restriction:
# When a domain restriction triggers on_update_range (2nd-order SSE),
# which in turn triggers more domain restrictions via try_sse_2nd_order,
# creating a deep cascade chain.
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

# === Pattern 1: SSE chain through apply_domain_restriction ===
cat("=== SSE cascade through restriction ===\n")
set.seed(206001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Create clauses where SSE on one pair restricts a clause,
  # which triggers 2nd-order SSE on another pair
  a1 = sample(dom, sample(2:3, 1))
  a2 = sample(dom, sample(2:3, 1))
  b1 = sample(dom, sample(2:3, 1))
  b2 = sample(dom, sample(2:3, 1))
  c1 = sample(dom, sample(2:3, 1))
  d1 = sample(dom, sample(2:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a2 | B %among% b2),
    as.CnfClause(B %among% b1 | C %among% c1),
    as.CnfClause(B %among% b2 | C %among% c1 | D %among% d1),
    as.CnfClause(C %among% c1 | D %among% d1)
  )

  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ssecasc-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ssecasc-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: apply_domain_restriction creating units during pairwise ===
cat("\n=== Restriction creates unit during pairwise ===\n")
set.seed(206002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # SSE restricts a 2-symbol clause so much that one symbol is eliminated,
  # turning it into a unit
  a1 = sample(dom, 2)
  a2 = sample(dom, 2)
  b1 = sample(dom, sample(1:2, 1))
  c1 = sample(dom, sample(1:2, 1))

  # (A in a1 | B in b1) and (A in a2 | B in b1) where SSE restricts
  # a clause with (A in .. | C in c1) such that A is eliminated -> C becomes unit
  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a2 | B %among% b1),
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | C %among% c1),
    as.CnfClause(B %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1)))
  )

  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [restrunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [restrunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Restriction creates unit: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Domain restriction with meta_idx > meta_idx_outer ===
cat("\n=== Beyond meta_idx_outer ===\n")
set.seed(206003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Create many clauses. The pairwise loop processes them in order.
  # If SSE on early clauses creates a unit that propagates to later clauses
  # (beyond meta_idx_outer), the code should handle the early return correctly.
  n_cl = sample(8:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 6) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [beyond-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [beyond-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Beyond meta_idx_outer: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Multiple restrictions on same clause ===
cat("\n=== Multiple restrictions same clause ===\n")
set.seed(206004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create clauses where a target clause gets restricted on multiple symbols
  # by different SSE operations
  target_a = sample(dom, sample(2:3, 1))
  target_b = sample(dom, sample(2:3, 1))
  target_c = sample(dom, sample(2:3, 1))

  # Pairs that restrict A in target
  a1 = sample(dom, sample(1:2, 1))
  a2 = sample(dom, sample(1:2, 1))

  # Pairs that restrict B in target
  b1 = sample(dom, sample(1:2, 1))
  b2 = sample(dom, sample(1:2, 1))

  clauses = list(
    as.CnfClause(A %among% target_a | B %among% target_b | C %among% target_c),
    as.CnfClause(A %among% a1 | C %among% sample(dom, sample(1:3, 1))),
    as.CnfClause(A %among% a2 | C %among% sample(dom, sample(1:3, 1))),
    as.CnfClause(B %among% b1 | C %among% sample(dom, sample(1:3, 1))),
    as.CnfClause(B %among% b2 | C %among% sample(dom, sample(1:3, 1)))
  )

  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multrestr-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multrestr-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multiple restrictions: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
