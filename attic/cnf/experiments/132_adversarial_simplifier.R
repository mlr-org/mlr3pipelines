#!/usr/bin/env Rscript
# Adversarial tests: construct formulas that stress the simplifier's
# internal state management and matrix operations.
# Focus on:
# 1. Clauses that change length during pairwise (SSE shortens them)
# 2. Chains where SSE creates units that then cascade
# 3. 2nd-order SSE where the oneend/twoend conditions are barely met
# 4. Formulas with many variables but few clauses (sparse)
# 5. Formulas with few variables but many clauses (dense)
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

# === SSE shortens clauses during pairwise ===
cat("=== SSE shortens during pairwise ===\n")
set.seed(132001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  syms = list(A = A, B = B, C = C, D = D)

  # Construct clauses so that during pairwise comparison,
  # SSE shortens a clause from 3 symbols to 2 or 1 symbol
  # This tests that the is_not_subset_of matrix is properly updated
  # when colnames change (via eliminate_symbol_from_clause)
  a_val = sample(dom, sample(1:2, 1))
  b_val = sample(dom, sample(1:2, 1))
  c_val = sample(dom, sample(1:2, 1))
  d_val = sample(dom, sample(1:2, 1))

  clauses = list(
    as.CnfClause(A %among% a_val | B %among% b_val),
    as.CnfClause(A %among% a_val | B %among% sample(dom, sample(1:2, 1)) | C %among% c_val),
    as.CnfClause(B %among% b_val | C %among% c_val | D %among% d_val),
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | D %among% sample(dom, sample(1:2, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [shorten-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [shorten-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE shortens: %d tests, %d failures\n", n_tests, n_failures))

# === SSE creates unit cascade ===
cat("\n=== SSE unit cascade ===\n")
set.seed(132002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # SSE on first pair creates a unit for B
  # That unit then propagates and creates a unit for C
  # etc.
  a1 = sample(dom, 1)
  b1 = sample(dom, 1)
  b2 = setdiff(dom, b1)

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a1 | B %among% sample(b2, min(length(b2), 1)))
  )
  # After SSE: B is restricted, potentially becoming a unit
  # Add clauses that will be affected by the new unit
  clauses[[3]] = as.CnfClause(B %among% sample(dom, sample(1:2, 1)) | C %among% sample(dom, sample(1:2, 1)))
  clauses[[4]] = as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | C %among% sample(dom, sample(1:2, 1)))

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cascade-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cascade-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE unit cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Sparse: many variables, few clauses ===
cat("\n=== Sparse formulas ===\n")
set.seed(132003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(6:10, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("S", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Only 3-5 clauses but each uses many variables
  n_clauses = sample(3:5, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(3:min(6, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

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
cat(sprintf("  Sparse: %d tests, %d failures\n", n_tests, n_failures))

# === Dense: few variables, many clauses ===
cat("\n=== Dense formulas ===\n")
set.seed(132004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2", "3")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # 10-25 clauses with just 2 variables
  n_clauses = sample(10:25, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(1:2, 1)
    if (n_sym == 1) {
      s = sample(c("A", "B"), 1)
      syms = list(A = A, B = B)
      as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
    } else {
      as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1)))
    }
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dense-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dense-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Dense: %d tests, %d failures\n", n_tests, n_failures))

# === Clauses with maximally overlapping symbols ===
cat("\n=== Max overlap ===\n")
set.seed(132005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # All clauses use all 3 symbols (maximum pairwise interaction)
  n_clauses = sample(4:10, 1)
  clauses = lapply(1:n_clauses, function(j) {
    as.CnfClause(
      A %among% sample(dom, sample(1:3, 1)) |
      B %among% sample(dom, sample(1:3, 1)) |
      C %among% sample(dom, sample(1:3, 1))
    )
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [maxol-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [maxol-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Max overlap: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
