#!/usr/bin/env Rscript
# Test with exotic domain values:
# - Empty strings
# - Very similar strings (substrings, case variations)
# - Single-character strings
# - Multi-byte / unicode-like strings
# - Numeric-looking strings ("1", "01", "1.0")
# Verify that string comparison works correctly in all cases
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

# === Empty string in domain ===
cat("=== Empty string domain ===\n")
set.seed(126001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("", "a", "ab")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  n_clauses = sample(2:5, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(1:2, 1)
    syms = list(A = A, B = B)
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
    cat(sprintf("ERROR [empty-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [empty-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Empty string: %d tests, %d failures\n", n_tests, n_failures))

# === Substring/prefix domains ===
cat("\n=== Substring domains ===\n")
set.seed(126002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "ab", "abc", "b", "bc")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  n_clauses = sample(2:5, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(1:2, 1)
    syms = list(A = A, B = B)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [substr-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [substr-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Substring: %d tests, %d failures\n", n_tests, n_failures))

# === Numeric-looking strings ===
cat("\n=== Numeric-looking strings ===\n")
set.seed(126003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("1", "01", "1.0", "10", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  n_clauses = sample(2:5, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(1:2, 1)
    syms = list(A = A, B = B)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [num-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [num-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Numeric strings: %d tests, %d failures\n", n_tests, n_failures))

# === Variable names that are substrings of each other ===
cat("\n=== Confusing variable names ===\n")
set.seed(126004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("x", "y", "z")
  V1 = CnfSymbol(u, "V1", dom)
  V10 = CnfSymbol(u, "V10", dom)
  V100 = CnfSymbol(u, "V100", dom)

  n_clauses = sample(2:4, 1)
  syms = list(V1 = V1, V10 = V10, V100 = V100)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(1:3, 1)
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
    cat(sprintf("ERROR [varname-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [varname-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Confusing names: %d tests, %d failures\n", n_tests, n_failures))

# === Domains with special characters ===
cat("\n=== Special character domains ===\n")
set.seed(126005)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a.b", "a_b", "a-b", "a b")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  n_clauses = sample(2:5, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(1:2, 1)
    syms = list(A = A, B = B)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [special-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [special-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Special chars: %d tests, %d failures\n", n_tests, n_failures))

# === Singleton domains (only 1 possible value per symbol) ===
cat("\n=== Singleton domains ===\n")
set.seed(126006)

for (trial in 1:200) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", "only")
  B = CnfSymbol(u, "B", c("x", "y"))
  C = CnfSymbol(u, "C", c("p", "q", "r"))

  syms = list(A = A, B = B, C = C)
  doms = list(A = "only", B = c("x", "y"), C = c("p", "q", "r"))

  n_clauses = sample(2:4, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:max(1, length(d) - 1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [singleton-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [singleton-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Singleton domains: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
