#!/usr/bin/env Rscript
# Dense clause interactions: all clauses share all symbols.
# This maximizes pairwise comparisons and forces all code paths in
# is_not_subset_of, not_subset_count, and cascading to exercise fully.
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

# === Pattern 1: 3 vars, all clauses have all 3 symbols ===
cat("=== 3 vars, all 3 symbols per clause ===\n")
set.seed(202001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    a_range = sample(dom, sample(1:3, 1))
    b_range = sample(dom, sample(1:3, 1))
    c_range = sample(dom, sample(1:3, 1))
    as.CnfClause(A %among% a_range | B %among% b_range | C %among% c_range)
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [3v3s-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3v3s-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  3v all symbols: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: 4 vars, all clauses have all 4 symbols ===
cat("\n=== 4 vars, all 4 symbols per clause ===\n")
set.seed(202002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  n_cl = sample(5:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    a_range = sample(dom, sample(1:2, 1))
    b_range = sample(dom, sample(1:2, 1))
    c_range = sample(dom, sample(1:2, 1))
    d_range = sample(dom, sample(1:2, 1))
    as.CnfClause(A %among% a_range | B %among% b_range | C %among% c_range | D %among% d_range)
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [4v4s-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [4v4s-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  4v all symbols: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Dense + unit (unit forces many restrictions) ===
cat("\n=== Dense + unit ===\n")
set.seed(202003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Unit for A
  a_val = sample(dom, sample(1:2, 1))
  clauses = list(as.CnfClause(A %among% a_val))

  # Dense clauses with all 3 symbols
  for (j in 1:sample(5:10, 1)) {
    a_range = sample(dom, sample(1:3, 1))
    b_range = sample(dom, sample(1:3, 1))
    c_range = sample(dom, sample(1:3, 1))
    cl = as.CnfClause(A %among% a_range | B %among% b_range | C %among% c_range)
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dense_u-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dense_u-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Dense + unit: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: 5 vars all shared, domain 3, many clauses ===
cat("\n=== 5 vars dense ===\n")
set.seed(202004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  E = CnfSymbol(u, "E", dom)
  syms = list(A = A, B = B, C = C, D = D, E = E)

  n_cl = sample(6:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    # Each clause has 3-5 symbols
    n_sym = sample(3:5, 1)
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
    cat(sprintf("ERROR [5vdense-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [5vdense-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  5v dense: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
