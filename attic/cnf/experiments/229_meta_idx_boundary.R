#!/usr/bin/env Rscript
# meta_idx_outer boundary testing:
# The variable meta_idx_outer controls how much of the is_not_subset_of
# matrix has been built. When unit propagation cascades beyond this
# boundary, code returns early (FALSE). This tests formulas where
# cascading crosses this boundary.
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

# === Pattern 1: Short clauses first (processed early), long clauses affected later ===
cat("=== Short first, long affected ===\n")
set.seed(229001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:6)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Short clauses (will be processed first in pairwise since entries are sorted by length)
  clauses = list()
  # Units
  unit_sym = sample(sym_names, 1)
  unit_val = sample(dom, sample(1:2, 1))
  clauses[[1]] = as.CnfClause(syms[[unit_sym]] %among% unit_val)

  # 2-symbol clauses
  for (j in 1:sample(3:5, 1)) {
    chosen = sample(sym_names, 2)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # Long clauses (will be processed late)
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(4:6, 1)
    chosen = sample(sym_names, n_sym)
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
    cat(sprintf("ERROR [shortlong-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [shortlong-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Short first long affected: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Early SSE creates unit that affects unbuilt matrix entries ===
cat("\n=== Early SSE cascades beyond boundary ===\n")
set.seed(229002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # SSE pair (2-symbol, will be processed early)
  v1_range = sample(dom, 2)
  v2_range_small = sample(dom, 1)
  v2_range_large = unique(c(v2_range_small, sample(dom, 1)))

  clauses = list(
    as.CnfClause(syms[[sym_names[1]]] %among% v1_range | syms[[sym_names[2]]] %among% v2_range_small),
    as.CnfClause(syms[[sym_names[1]]] %among% v1_range | syms[[sym_names[2]]] %among% v2_range_large)
  )

  # Medium clauses
  for (j in 1:sample(3:5, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # Long clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(3:5, 1)
    chosen = sample(sym_names, n_sym)
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
    cat(sprintf("ERROR [boundary-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [boundary-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE cascades beyond boundary: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Many variables, sparse clauses ===
cat("\n=== Many variables sparse ===\n")
set.seed(229003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(8:12, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Sparse: each clause uses only 2-3 out of many vars
  n_cl = sample(6:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sparse-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sparse-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Many vars sparse: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Same clause structure, different ranges ===
cat("\n=== Same structure different ranges ===\n")
set.seed(229004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # All clauses use same structure (A|B|C) but different ranges
  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    a_range = sample(dom, sample(1:4, 1))
    b_range = sample(dom, sample(1:4, 1))
    c_range = sample(dom, sample(1:4, 1))
    as.CnfClause(A %among% a_range | B %among% b_range | C %among% c_range)
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [samestruct-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [samestruct-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Same structure: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
