#!/usr/bin/env Rscript
# Heterogeneous domain stress:
# - Variables with very different domain sizes (2, 5, 10, ...)
# - Tests that domain-size-dependent optimizations work correctly
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

# === Pattern 1: Binary + large domain ===
cat("=== Binary + large domain ===\n")
set.seed(198001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom_bin = c("a", "b")
  dom_large = letters[1:8]
  A = CnfSymbol(u, "A", dom_bin)
  B = CnfSymbol(u, "B", dom_large)
  C = CnfSymbol(u, "C", dom_bin)
  syms = list(A = A, B = B, C = C)
  doms = list(A = dom_bin, B = dom_large, C = dom_bin)

  n_cl = sample(4:7, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(doms[[s]], sample(1:max(1, length(doms[[s]])-1), 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [binlarge-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [binlarge-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Binary + large: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Three different domain sizes ===
cat("\n=== Three different domain sizes ===\n")
set.seed(198002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom2 = c("x", "y")
  dom4 = c("a", "b", "c", "d")
  dom7 = letters[1:7]
  A = CnfSymbol(u, "A", dom2)
  B = CnfSymbol(u, "B", dom4)
  C = CnfSymbol(u, "C", dom7)
  syms = list(A = A, B = B, C = C)
  doms = list(A = dom2, B = dom4, C = dom7)

  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      max_r = max(1, length(doms[[s]]) - 1)
      syms[[s]] %among% sample(doms[[s]], sample(1:max_r, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [3dom-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3dom-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Three domains: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Unit with large domain ===
cat("\n=== Unit with large domain ===\n")
set.seed(198003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom2 = c("a", "b")
  dom10 = letters[1:10]
  A = CnfSymbol(u, "A", dom2)
  B = CnfSymbol(u, "B", dom10)
  C = CnfSymbol(u, "C", dom2)
  syms = list(A = A, B = B, C = C)
  doms = list(A = dom2, B = dom10, C = dom2)

  # Unit for the large domain variable
  b_unit_range = sample(dom10, sample(2:7, 1))
  clauses = list(
    as.CnfClause(B %among% b_unit_range)
  )

  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      max_r = max(1, length(doms[[s]]) - 1)
      syms[[s]] %among% sample(doms[[s]], sample(1:max_r, 1))
    })
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ularge-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ularge-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Unit large domain: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Operations between heterogeneous formulas ===
cat("\n=== Heterogeneous operations ===\n")
set.seed(198004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom2 = c("x", "y")
  dom5 = letters[1:5]
  dom8 = letters[1:8]
  A = CnfSymbol(u, "A", dom2)
  B = CnfSymbol(u, "B", dom5)
  C = CnfSymbol(u, "C", dom8)
  syms = list(A = A, B = B, C = C)
  doms = list(A = dom2, B = dom5, C = dom8)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) {
        max_r = max(1, length(doms[[s]]) - 1)
        syms[[s]] %among% sample(doms[[s]], sample(1:max_r, 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(); g = make_f()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1

  # Test all three operations
  for (op in c("&", "|", "!")) {
    if (op == "!") {
      result = tryCatch(!f, error = function(e) NULL)
      if (is.null(result)) next
      t_result = evaluate_formula(result, u)
      t_f = evaluate_formula(f, u)
      if (!all(t_result == !t_f)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [hetop-%d]: !f mismatch\n", trial))
      }
    } else {
      result = if (op == "&") tryCatch(f & g, error = function(e) NULL) else tryCatch(f | g, error = function(e) NULL)
      if (is.null(result)) next
      t_result = evaluate_formula(result, u)
      t_f = evaluate_formula(f, u)
      t_g = evaluate_formula(g, u)
      expected = if (op == "&") t_f & t_g else t_f | t_g
      if (!all(t_result == expected)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [hetop-%d]: %s mismatch\n", trial, op))
      }
    }
  }
}
cat(sprintf("  Heterogeneous operations: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
