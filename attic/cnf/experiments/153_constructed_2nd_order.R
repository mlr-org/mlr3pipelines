#!/usr/bin/env Rscript
# Construct SPECIFIC 2nd-order SSE patterns where:
# 1. Oneend clause doesn't have the target symbol at all
# 2. Twoend clause overlaps with oneend on both non-subset symbols
# 3. The disjointness check at line 456 barely passes/fails
# 4. Multiple 2nd-order SSE attempts on same target
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

# === Oneend without target symbol ===
cat("=== Oneend without target symbol ===\n")
set.seed(153001)

# Construct: oneend has only A (symbol_intersect)
# twoend has A and C (symbol_intersect and symbol_target)
# target has A, B, C
# oneend does NOT have C (symbol_target)
for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Oneend: single symbol A, not subset of target on A
  a_oneend = sample(dom, sample(1:2, 1))
  # Twoend: A and C, not subset of target on both
  a_twoend = sample(dom, sample(1:3, 1))
  c_twoend = sample(dom, sample(1:3, 1))
  # Target: A, B, C
  a_target = sample(dom, sample(1:3, 1))
  b_target = sample(dom, sample(1:3, 1))
  c_target = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a_oneend),  # oneend: only A
    as.CnfClause(A %among% a_twoend | C %among% c_twoend),  # twoend: A and C
    as.CnfClause(A %among% a_target | B %among% b_target | C %among% c_target)  # target
  )
  # Add 1-2 extra clauses for variety
  for (j in 1:sample(1:2, 1)) {
    clauses[[length(clauses) + 1]] = as.CnfClause(
      B %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1))
    )
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [no-target-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [no-target-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Oneend no target symbol: %d tests, %d failures\n", n_tests, n_failures))

# === Disjointness check boundary ===
cat("\n=== Disjointness boundary ===\n")
set.seed(153002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Create oneend and twoend that overlap on A at the boundary
  # They share some values of A, but only within target's A range
  a_target = sample(dom, sample(2:4, 1))
  # Choose oneend A values: some inside target, some outside
  a_oneend = sample(dom, sample(1:3, 1))
  # Choose twoend A values: some inside target, some outside
  a_twoend = sample(dom, sample(1:3, 1))

  b_target = sample(dom, sample(2:4, 1))
  c_twoend = sample(dom, sample(1:4, 1))
  c_target = sample(dom, sample(2:4, 1))

  clauses = list(
    as.CnfClause(A %among% a_oneend | B %among% sample(dom, sample(1:3, 1))),
    as.CnfClause(A %among% a_twoend | C %among% c_twoend),
    as.CnfClause(A %among% a_target | B %among% b_target | C %among% c_target)
  )
  # Extra clauses
  clauses[[4]] = as.CnfClause(B %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1)))
  clauses[[5]] = as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1)))

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [disjoint-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [disjoint-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Disjointness boundary: %d tests, %d failures\n", n_tests, n_failures))

# === Multiple 2nd-order SSE attempts on same target ===
cat("\n=== Multiple 2nd-order on same target ===\n")
set.seed(153003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  # Target has many symbols, multiple pairs of oneend/twoend can attempt SSE
  a_target = sample(dom, sample(2:3, 1))
  b_target = sample(dom, sample(2:3, 1))
  c_target = sample(dom, sample(2:3, 1))
  d_target = sample(dom, sample(2:3, 1))

  clauses = list(
    as.CnfClause(A %among% a_target | B %among% b_target | C %among% c_target | D %among% d_target),
    # Multiple oneend/twoend candidates
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | B %among% sample(dom, sample(1:2, 1))),
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | C %among% sample(dom, sample(1:2, 1))),
    as.CnfClause(B %among% sample(dom, sample(1:2, 1)) | D %among% sample(dom, sample(1:2, 1))),
    as.CnfClause(C %among% sample(dom, sample(1:2, 1)) | D %among% sample(dom, sample(1:2, 1))),
    as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1)) | D %among% sample(dom, sample(1:3, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multi-2nd-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multi-2nd-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multiple 2nd-order: %d tests, %d failures\n", n_tests, n_failures))

# === 2nd-order with 5 variables domain 3 ===
cat("\n=== 2nd-order 5v3d ===\n")
set.seed(153004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (i in 1:5) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Mix of 2 and 3 symbol clauses
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
    cat(sprintf("ERROR [5v3d-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [5v3d-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2nd-order 5v3d: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
