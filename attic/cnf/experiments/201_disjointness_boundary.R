#!/usr/bin/env Rscript
# try_sse_2nd_order disjointness boundary testing:
# The 2nd-order SSE requires that oneend and twoend are "disjoint outside target"
# w.r.t. symbol_intersect. This test creates patterns at exactly that boundary.
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

# === Pattern 1: Overlapping oneend/twoend on intersect symbol ===
cat("=== Overlapping intersect symbol ===\n")
set.seed(201001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Construct clauses that might trigger 2nd-order SSE with overlapping ranges
  # on the intersect symbol
  a_range_oneend = sample(dom, sample(2:3, 1))
  a_range_twoend = sample(dom, sample(2:3, 1))
  # Ensure some overlap on A (the intersect symbol)
  overlap = intersect(a_range_oneend, a_range_twoend)

  b_range_oneend = sample(dom, sample(1:3, 1))
  b_range_twoend = sample(dom, sample(1:3, 1))

  # Target clause with both A and B
  a_range_target = sample(dom, sample(2:4, 1))
  b_range_target = sample(dom, sample(2:4, 1))
  c_range_target = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a_range_oneend | B %among% b_range_oneend),
    as.CnfClause(A %among% a_range_twoend | B %among% b_range_twoend),
    as.CnfClause(A %among% a_range_target | B %among% b_range_target | C %among% c_range_target)
  )

  # Add more clauses to create interaction
  for (j in 1:sample(2:4, 1)) {
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
    cat(sprintf("ERROR [overlap-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [overlap-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Overlapping intersect: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Disjoint except in target ===
cat("\n=== Disjoint except in target ===\n")
set.seed(201002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e", "f")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Construct oneend and twoend where they overlap on A only within target's A range
  # This is the exact boundary condition for try_sse_2nd_order
  target_a = sample(dom, sample(2:4, 1))
  overlap_vals = sample(target_a, min(2, length(target_a)))
  non_target_a = setdiff(dom, target_a)

  if (length(non_target_a) < 2) next

  # oneend A-range: some overlap values + some non-target values
  oneend_a = unique(c(sample(overlap_vals, 1), sample(non_target_a, 1)))
  # twoend A-range: some overlap values + different non-target values
  twoend_a = unique(c(sample(overlap_vals, 1), sample(non_target_a, 1)))

  oneend_b = sample(dom, sample(1:3, 1))
  twoend_b = sample(dom, sample(1:3, 1))
  target_b = sample(dom, sample(2:4, 1))

  clauses = list(
    as.CnfClause(A %among% oneend_a | B %among% oneend_b),
    as.CnfClause(A %among% twoend_a | B %among% twoend_b),
    as.CnfClause(A %among% target_a | B %among% target_b)
  )

  for (j in 1:sample(2:4, 1)) {
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
    cat(sprintf("ERROR [disjbnd-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [disjbnd-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Disjoint boundary: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Multiple 2nd-order SSE opportunities same target ===
cat("\n=== Multiple 2nd-order same target ===\n")
set.seed(201003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Target with A, B, C
  target_a = sample(dom, sample(2:3, 1))
  target_b = sample(dom, sample(2:3, 1))
  target_c = sample(dom, sample(2:3, 1))

  # Pair 1: oneend/twoend for symbol A, targeting B
  p1_a1 = sample(dom, sample(1:3, 1))
  p1_b1 = sample(dom, sample(1:3, 1))
  p1_a2 = sample(dom, sample(1:3, 1))
  p1_b2 = sample(dom, sample(1:3, 1))

  # Pair 2: oneend/twoend for symbol A, targeting C
  p2_a1 = sample(dom, sample(1:3, 1))
  p2_c1 = sample(dom, sample(1:3, 1))
  p2_a2 = sample(dom, sample(1:3, 1))
  p2_c2 = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% target_a | B %among% target_b | C %among% target_c),
    as.CnfClause(A %among% p1_a1 | B %among% p1_b1),
    as.CnfClause(A %among% p1_a2 | B %among% p1_b2),
    as.CnfClause(A %among% p2_a1 | C %among% p2_c1),
    as.CnfClause(A %among% p2_a2 | C %among% p2_c2)
  )

  for (j in 1:sample(1:3, 1)) {
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
    cat(sprintf("ERROR [multi2nd-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multi2nd-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multiple 2nd-order: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: 2nd-order SSE where oneend has 3 symbols ===
cat("\n=== 3-symbol oneend ===\n")
set.seed(201004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # 3-symbol oneend clause: (A | B | C) where only A is non-subset of target
  a1 = sample(dom, sample(1:2, 1))
  b1 = sample(dom, sample(1:3, 1))
  c1 = sample(dom, sample(1:3, 1))

  # twoend: (A | D) or (A | B | D)
  a2 = sample(dom, sample(1:2, 1))
  d2 = sample(dom, sample(1:2, 1))

  # target must contain B, C, D ranges that subsume b1, c1 and have D
  target_a = sample(dom, sample(1:3, 1))
  target_b = sample(dom, sample(2:3, 1))
  target_c = sample(dom, sample(2:3, 1))
  target_d = sample(dom, sample(2:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1 | C %among% c1),
    as.CnfClause(A %among% a2 | D %among% d2),
    as.CnfClause(A %among% target_a | B %among% target_b | C %among% target_c | D %among% target_d)
  )

  for (j in 1:sample(2:5, 1)) {
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
    cat(sprintf("ERROR [3sym-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3sym-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  3-symbol oneend: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
