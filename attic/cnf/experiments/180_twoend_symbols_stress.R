#!/usr/bin/env Rscript
# Test handle_sse_2nd_order_twoend with focus on:
# - The 1:2 hardcoded loop (symbols_twoend must have exactly 2 elements)
# - Consistency between not_subset_count and is_not_subset_of matrix
# - Cascading during twoend that modifies the matrix
# - Clauses with many symbols (high chance of not_subset_count == 2)
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

# === Pattern 1: Clauses with exactly 2 non-subset symbols (twoend) ===
cat("=== Twoend-rich formulas ===\n")
set.seed(180001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Create clauses that share many symbols but differ in exactly 2
  # This maximizes not_subset_count == 2 opportunities
  n_cl = sample(6:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(3:4, 1)
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
    cat(sprintf("ERROR [twoend1-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [twoend1-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Twoend-rich: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: 5 variables, many clauses - maximize twoend interactions ===
cat("\n=== 5-var twoend stress ===\n")
set.seed(180002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = 5
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(8:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(3:5, 1)
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
    cat(sprintf("ERROR [twoend2-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [twoend2-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  5-var twoend: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Constructed twoend with cascading SSE ===
cat("\n=== Twoend + cascade ===\n")
set.seed(180003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create pairs that should trigger twoend resolution
  a1 = sample(dom, sample(1:2, 1))
  a2 = sample(dom, sample(1:2, 1))
  b1 = sample(dom, sample(1:3, 1))
  b2 = sample(dom, sample(1:3, 1))
  c1 = sample(dom, sample(1:3, 1))
  c2 = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1 | C %among% c1),
    as.CnfClause(A %among% a2 | B %among% b2 | C %among% c2)
  )
  # Add more clauses to create cascade opportunities
  for (j in 1:sample(3:7, 1)) {
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
    cat(sprintf("ERROR [twoend3-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [twoend3-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Twoend + cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Many clauses with same 4 symbols, varying ranges ===
cat("\n=== Same symbols, varying ranges ===\n")
set.seed(180004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # All clauses use the same 4 symbols, but with different ranges
  n_cl = sample(8:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    atoms = lapply(names(syms), function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 6) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [same4-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [same4-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Same 4 symbols: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
