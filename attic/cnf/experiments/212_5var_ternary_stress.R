#!/usr/bin/env Rscript
# 5-variable ternary stress test:
# The most complex configuration we can reasonably truth-table verify.
# 5 vars x 3 values = 3^5 = 243 truth table entries.
# Tests all phases of simplification with higher variable counts.
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

# === Pattern 1: Random 5v ternary ===
cat("=== Random 5v ternary ===\n")
set.seed(212001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  E = CnfSymbol(u, "E", dom)
  syms = list(A = A, B = B, C = C, D = D, E = E)

  n_cl = sample(5:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [5vt-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [5vt-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Random 5v ternary: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: 5v ternary with units ===
cat("\n=== 5v ternary with units ===\n")
set.seed(212002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  E = CnfSymbol(u, "E", dom)
  syms = list(A = A, B = B, C = C, D = D, E = E)

  # 1-2 units
  n_units = sample(1:2, 1)
  unit_syms = sample(names(syms), n_units)
  clauses = lapply(unit_syms, function(s) as.CnfClause(syms[[s]] %among% sample(dom, 1)))

  # Other clauses
  for (j in 1:sample(5:10, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
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
    cat(sprintf("ERROR [5vu-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [5vu-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  5v ternary units: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: 5v ternary operations ===
cat("\n=== 5v ternary operations ===\n")
set.seed(212003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  E = CnfSymbol(u, "E", dom)
  syms = list(A = A, B = B, C = C, D = D, E = E)

  make_f = function() {
    n_cl = sample(2:4, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(2:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  for (op in c("&", "|", "!")) {
    if (op == "!") {
      result = tryCatch(!f1, error = function(e) NULL)
      if (is.null(result)) next
      t_result = evaluate_formula(result, u)
      t_f1 = evaluate_formula(f1, u)
      if (!all(t_result == !t_f1)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [5vop-%d]: !f1 mismatch\n", trial))
      }
    } else {
      result = if (op == "&") tryCatch(f1 & f2, error = function(e) NULL) else tryCatch(f1 | f2, error = function(e) NULL)
      if (is.null(result)) next
      t_result = evaluate_formula(result, u)
      t_f1 = evaluate_formula(f1, u)
      t_f2 = evaluate_formula(f2, u)
      expected = if (op == "&") t_f1 & t_f2 else t_f1 | t_f2
      if (!all(t_result == expected)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [5vop-%d]: %s mismatch\n", trial, op))
      }
    }
  }
}
cat(sprintf("  5v ternary operations: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: 5v ternary many clauses ===
cat("\n=== 5v ternary many clauses ===\n")
set.seed(212004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  E = CnfSymbol(u, "E", dom)
  syms = list(A = A, B = B, C = C, D = D, E = E)

  n_cl = sample(15:30, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:5, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 10) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [5vm-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [5vm-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  5v ternary many: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
