#!/usr/bin/env Rscript
# All-units formula testing:
# Formulas where all or most clauses are units.
# Tests the unit processing loop and unit register/intersection logic.
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

# === Pattern 1: All unit clauses (one symbol each) ===
cat("=== All unit clauses ===\n")
set.seed(241001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  n_vars = sample(3:6, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # All clauses are units
  n_cl = sample(n_vars:(n_vars + 4), 1)
  clauses = lapply(1:n_cl, function(j) {
    s = sample(sym_names, 1)
    as.CnfClause(syms[[s]] %among% sample(dom, sample(1:4, 1)))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [allunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [allunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  All units: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Many overlapping units per symbol ===
cat("\n=== Many overlapping units ===\n")
set.seed(241002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  clauses = list()
  # Multiple units for each symbol
  for (s in sym_names) {
    n_units = sample(1:4, 1)
    for (k in 1:n_units) {
      clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(2:4, 1)))
    }
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [manyunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [manyunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Many overlapping units: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Units mixed with 2-symbol clauses that become units ===
cat("\n=== Units + clauses that become units ===\n")
set.seed(241003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  clauses = list()
  # Some direct units
  for (k in 1:sample(2:3, 1)) {
    s = sample(sym_names, 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
  }

  # 2-symbol clauses where unit propagation might eliminate one symbol
  for (k in 1:sample(3:6, 1)) {
    pair = sample(sym_names, 2)
    atoms = lapply(pair, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [unitbecome-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unitbecome-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Units becoming: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Unit operations ===
cat("\n=== Unit formula operations ===\n")
set.seed(241004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Create unit-only formulas
  f1 = CnfFormula(list(as.CnfClause(A %among% sample(dom, sample(1:3, 1)))))
  f2 = CnfFormula(list(as.CnfClause(B %among% sample(dom, sample(1:3, 1)))))

  n_tests = n_tests + 1

  # AND of unit formulas
  r_and = tryCatch(f1 & f2, error = function(e) e)
  if (inherits(r_and, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [unitop-and-%d]: %s\n", trial, r_and$message)); next
  }
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  if (!all(evaluate_formula(r_and, u) == (t1 & t2))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unitop-and-%d]: mismatch\n", trial)); next
  }

  # OR of unit formulas (produces distribution)
  r_or = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(r_or, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [unitop-or-%d]: %s\n", trial, r_or$message)); next
  }
  if (!all(evaluate_formula(r_or, u) == (t1 | t2))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unitop-or-%d]: mismatch\n", trial)); next
  }

  # NOT of unit formula
  r_not = tryCatch(!f1, error = function(e) e)
  if (inherits(r_not, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [unitop-not-%d]: %s\n", trial, r_not$message)); next
  }
  if (!all(evaluate_formula(r_not, u) == !t1)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unitop-not-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Unit ops: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
