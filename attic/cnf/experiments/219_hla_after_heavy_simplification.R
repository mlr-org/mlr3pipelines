#!/usr/bin/env Rscript
# HLA phase after heavy pairwise simplification:
# Test formulas where the pairwise phase does significant work
# (many SSE operations, unit propagations, eliminations),
# and then HLA needs to work on the remaining clauses.
# This tests the remaining_entries computation and HLA correctness
# after complex pairwise modifications.
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

# === Pattern 1: Many clauses with high SSE potential + HLA-detectable hidden tautologies ===
cat("=== Heavy SSE then HLA ===\n")
set.seed(219001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Many overlapping clauses that will trigger SSE
  clauses = list()
  for (j in 1:sample(6:12, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # Add clause pairs designed for HLA
  # HLA: clause X subsumes clause Y on all but one symbol
  # => complement of that symbol can be added to Y
  # => if that makes Y tautological, eliminate Y
  for (j in 1:sample(2:4, 1)) {
    a_range = sample(dom, sample(1:2, 1))
    b_range = sample(dom, sample(1:3, 1))
    c_range = sample(dom, sample(1:3, 1))
    # Create HLA pair
    cl1 = as.CnfClause(A %among% a_range | B %among% b_range)
    cl2 = as.CnfClause(A %among% a_range | B %among% b_range | C %among% c_range)
    if (!isTRUE(unclass(cl1))) clauses[[length(clauses) + 1]] = cl1
    if (!isTRUE(unclass(cl2))) clauses[[length(clauses) + 1]] = cl2
  }

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [heavysse-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [heavysse-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Heavy SSE then HLA: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Units eliminate many, leaving few for HLA ===
cat("\n=== Units then HLA ===\n")
set.seed(219002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Strong unit constraints
  a_val = sample(dom, sample(1:2, 1))
  b_val = sample(dom, sample(1:2, 1))
  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(B %among% b_val)
  )

  # Many clauses, some of which will be eliminated by units
  for (j in 1:sample(6:12, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [unithla-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unithla-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Units then HLA: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: HLA with unit phase specifically ===
cat("\n=== HLA unit phase stress ===\n")
set.seed(219003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create units
  clauses = list()
  n_units = sample(1:3, 1)
  unit_syms = sample(sym_names, n_units)
  for (s in unit_syms) {
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
  }

  # Non-unit clauses that survive unit propagation but might be detected by HLA unit phase
  for (j in 1:sample(5:10, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(sym_names, n_sym)
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
    cat(sprintf("ERROR [hlauph-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hlauph-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA unit phase: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Operations after heavy simplification ===
cat("\n=== Operations after heavy simplification ===\n")
set.seed(219004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_heavy_formula = function() {
    cls = list()
    for (j in 1:sample(4:8, 1)) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      cl = as.CnfClause(Reduce(`|`, atoms))
      if (!isTRUE(unclass(cl))) cls[[length(cls) + 1]] = cl
    }
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 2) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_heavy_formula()
  f2 = make_heavy_formula()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  # Test AND, OR, and NOT operations on heavily simplified formulas
  op = sample(1:3, 1)
  result = tryCatch({
    if (op == 1) f1 & f2
    else if (op == 2) f1 | f2
    else !f1
  }, error = function(e) e)

  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ops-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  expected = if (op == 1) t1 & t2 else if (op == 2) t1 | t2 else !t1
  actual = evaluate_formula(result, u)

  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ops-%d]: op=%d mismatch\n", trial, op))
  }
}
cat(sprintf("  Operations after heavy: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
