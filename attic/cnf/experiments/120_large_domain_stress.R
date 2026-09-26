#!/usr/bin/env Rscript
# Stress test with very large domains (15-30 values)
# - Tests char_intersect, char_setdiff, char_union with many values
# - Tests subset checking with large value sets
# - Tests simplification with many possible range combinations
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

# === 2 vars, domain 20 ===
cat("=== 2 vars, domain 20 ===\n")
set.seed(120001)

for (trial in 1:200) {
  u = CnfUniverse()
  d = paste0("v", 1:20)
  A = CnfSymbol(u, "A", d)
  B = CnfSymbol(u, "B", d)

  n_clauses = sample(2:6, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_a = sample(1:19, 1)
    n_b = sample(1:19, 1)
    as.CnfClause(A %among% sample(d, n_a) | B %among% sample(d, n_b))
  })

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [d20-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [d20-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2v-d20: %d tests, %d failures\n", n_tests, n_failures))

# === 2 vars, domain 30 (large cross-product: 900 combos) ===
cat("\n=== 2 vars, domain 30 ===\n")
set.seed(120002)

for (trial in 1:100) {
  u = CnfUniverse()
  d = paste0("val", 1:30)
  A = CnfSymbol(u, "A", d)
  B = CnfSymbol(u, "B", d)

  n_clauses = sample(3:10, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_a = sample(1:29, 1)
    n_b = sample(1:29, 1)
    as.CnfClause(A %among% sample(d, n_a) | B %among% sample(d, n_b))
  })

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [d30-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [d30-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2v-d30: %d tests, %d failures\n", n_tests, n_failures))

# === 3 vars, domain 8 (512 combos) ===
cat("\n=== 3 vars, domain 8 ===\n")
set.seed(120003)

for (trial in 1:200) {
  u = CnfUniverse()
  d = paste0("x", 1:8)
  A = CnfSymbol(u, "A", d)
  B = CnfSymbol(u, "B", d)
  C = CnfSymbol(u, "C", d)

  n_clauses = sample(3:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(c("A", "B", "C"), n_sym)
    syms = list(A = A, B = B, C = C)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(d, sample(1:7, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [3v8-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3v8-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  3v-d8: %d tests, %d failures\n", n_tests, n_failures))

# === Large domain with SSE-inducing patterns ===
cat("\n=== SSE with large domains ===\n")
set.seed(120004)

for (trial in 1:200) {
  u = CnfUniverse()
  d = paste0("d", 1:15)
  A = CnfSymbol(u, "A", d)
  B = CnfSymbol(u, "B", d)

  # Create clauses that share many values to trigger SSE
  base_a = sample(d, sample(5:10, 1))
  base_b = sample(d, sample(5:10, 1))

  clauses = list()
  for (j in 1:sample(3:6, 1)) {
    # Slightly modify base values
    a_vals = if (runif(1) > 0.5) base_a else sample(d, sample(3:12, 1))
    b_vals = if (runif(1) > 0.5) base_b else sample(d, sample(3:12, 1))
    clauses[[j]] = as.CnfClause(A %among% a_vals | B %among% b_vals)
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sse15-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sse15-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE d15: %d tests, %d failures\n", n_tests, n_failures))

# === Mixed domain sizes: one large (20), one small (2) ===
cat("\n=== Mixed large+small domains ===\n")
set.seed(120005)

for (trial in 1:200) {
  u = CnfUniverse()
  d_large = paste0("L", 1:20)
  d_small = c("0", "1")
  A = CnfSymbol(u, "A", d_large)
  B = CnfSymbol(u, "B", d_small)

  n_clauses = sample(3:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    use_both = runif(1) > 0.3
    if (use_both) {
      as.CnfClause(A %among% sample(d_large, sample(1:19, 1)) | B %among% sample(d_small, 1))
    } else {
      as.CnfClause(A %among% sample(d_large, sample(1:19, 1)))
    }
  })

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mixed-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixed-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Mixed: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
