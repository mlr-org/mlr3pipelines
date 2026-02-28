#!/usr/bin/env Rscript
# Test with domain values that could cause string-matching issues:
# - Substrings of each other
# - Values with special characters
# - Very long strings
# - Numeric-looking strings
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Substring domain values ===
cat("=== Substring domains ===\n")
set.seed(291001)

for (trial in 1:200) {
  u = CnfUniverse()
  # Domain with values that are substrings of each other
  dom = c("a", "ab", "abc", "b", "bc")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
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
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [substr-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Substring domains: %d tests, %d failures\n", n_tests, n_failures))

# === Numeric-looking strings ===
cat("\n=== Numeric domains ===\n")
set.seed(291002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("1", "2", "10", "20", "100")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
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
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [num-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Numeric domains: %d tests, %d failures\n", n_tests, n_failures))

# === Long string values ===
cat("\n=== Long string domains ===\n")
set.seed(291003)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("alpha_value_one", "beta_value_two", "gamma_value_three", "delta_value_four")
  sym_names = paste0("Variable_", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [long-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [long-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Long string domains: %d tests, %d failures\n", n_tests, n_failures))

# === Mixed domain sizes ===
cat("\n=== Fresh mixed domain sizes ===\n")
set.seed(291004)

for (trial in 1:300) {
  u = CnfUniverse()
  # Different sized domains per variable
  d1 = sample(letters, sample(2:4, 1))
  d2 = sample(letters, sample(3:6, 1))
  d3 = sample(letters, sample(2:5, 1))

  V1 = CnfSymbol(u, "V1", d1)
  V2 = CnfSymbol(u, "V2", d2)
  V3 = CnfSymbol(u, "V3", d3)

  n_cl = sample(3:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    syms_map = list(V1 = list(sym = V1, dom = d1), V2 = list(sym = V2, dom = d2), V3 = list(sym = V3, dom = d3))
    n_sym = sample(1:3, 1)
    chosen = sample(c("V1", "V2", "V3"), n_sym)
    atoms = lapply(chosen, function(s) {
      sinfo = syms_map[[s]]
      sinfo$sym %among% sample(sinfo$dom, sample(1:max(1, length(sinfo$dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mxd-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mxd-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Fresh mixed domain sizes: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
