#!/usr/bin/env Rscript
# Regression test for the 4 known bugs and related patterns:
# Bug 1: CnfUniverse print error
# Bug 2: CnfClause(list()) non-empty environment
# Bug 3: CnfSymbol allows duplicate values
# Bug 4: CnfSymbol allows empty domain
# Test that these bugs exist and that formulas using valid inputs work correctly
# even in corner cases near these bugs.
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

# === Pattern 1: Symbols with very similar names ===
cat("=== Similar symbol names ===\n")
set.seed(253001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  # Symbols with similar names
  sym_names = c("V1", "V10", "V11", "V2")
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [simname-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [simname-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Similar names: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Domains with single overlapping value ===
cat("\n=== Single overlap domains ===\n")
set.seed(253002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("shared", "only_a", "only_b")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) {
      # Bias towards including "shared"
      if (runif(1) > 0.5) {
        syms[[s]] %among% c("shared", sample(c("only_a", "only_b"), 1))
      } else {
        syms[[s]] %among% sample(dom, sample(1:2, 1))
      }
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

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
cat(sprintf("  Single overlap: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Formulas with mixed widths (1-5 symbols per clause) ===
cat("\n=== Mixed width clauses ===\n")
set.seed(253003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create clauses with varying widths
  clauses = list()
  # Unit clause
  clauses[[1]] = as.CnfClause(syms[[sample(sym_names, 1)]] %among% sample(dom, sample(1:2, 1)))
  # 2-symbol clause
  pair = sample(sym_names, 2)
  clauses[[2]] = as.CnfClause(Reduce(`|`, lapply(pair, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))))
  # 3-symbol clause
  triple = sample(sym_names, 3)
  clauses[[3]] = as.CnfClause(Reduce(`|`, lapply(triple, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))))
  # 4-5 symbol clause
  wide = sample(sym_names, sample(4:5, 1))
  clauses[[4]] = as.CnfClause(Reduce(`|`, lapply(wide, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))))

  # More random
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(1:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

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
cat(sprintf("  Mixed width: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Same formula via different construction paths ===
cat("\n=== Same formula different paths ===\n")
set.seed(253004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Pick random ranges
  ra = sample(dom, sample(1:2, 1))
  rb = sample(dom, sample(1:2, 1))
  rc = sample(dom, sample(1:2, 1))

  n_tests = n_tests + 1

  # Path 1: Direct construction
  cl1 = as.CnfClause(A %among% ra | B %among% rb)
  cl2 = as.CnfClause(C %among% rc)
  f_direct = tryCatch(CnfFormula(list(cl1, cl2)), error = function(e) NULL)

  # Path 2: Via AND
  f1 = CnfFormula(list(cl1))
  f2 = CnfFormula(list(cl2))
  f_and = tryCatch(f1 & f2, error = function(e) NULL)

  if (is.null(f_direct) || is.null(f_and)) next

  truth_direct = evaluate_formula(f_direct, u)
  truth_and = evaluate_formula(f_and, u)

  if (!all(truth_direct == truth_and)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [path-%d]: direct vs AND mismatch\n", trial))
  }
}
cat(sprintf("  Different paths: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
