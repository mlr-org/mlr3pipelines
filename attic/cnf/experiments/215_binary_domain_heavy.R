#!/usr/bin/env Rscript
# Binary domain (2 values) heavy testing:
# This maps directly to Boolean SAT. Every non-trivial atom has exactly 1 value,
# which means units are very common and cascading is aggressive.
# Also tests the boundary where length(range)==1 == length(universe)/2.
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

# === Pattern 1: 6 binary vars, many unit-producing clauses ===
cat("=== 6 binary vars, unit-heavy ===\n")
set.seed(215001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("T", "F")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  E = CnfSymbol(u, "E", dom)
  F = CnfSymbol(u, "F", dom)
  syms = list(A = A, B = B, C = C, D = D, E = E, F = F)

  n_cl = sample(6:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [6b-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [6b-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  6 binary unit-heavy: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: 8 binary vars, 3-SAT style ===
cat("\n=== 8 binary vars, 3-SAT ===\n")
set.seed(215002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1")
  sym_names = paste0("X", 1:8)
  syms = list()
  for (s in sym_names) {
    syms[[s]] = CnfSymbol(u, s, dom)
  }

  n_cl = sample(10:25, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(sym_names, 3)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 8) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [8b3sat-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [8b3sat-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  8 binary 3-SAT: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Binary with chain implications (A->B->C->...) ===
cat("\n=== Binary chain implications ===\n")
set.seed(215003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("T", "F")
  n_vars = sample(4:7, 1)
  syms = list()
  sym_names = paste0("V", 1:n_vars)
  for (s in sym_names) {
    syms[[s]] = CnfSymbol(u, s, dom)
  }

  # Implications: V1=T -> V2=T, V2=T -> V3=T, etc.
  # In CNF: (V1=F | V2=T), (V2=F | V3=T), etc.
  clauses = list()
  for (i in 1:(n_vars-1)) {
    cl = as.CnfClause(syms[[sym_names[i]]] %among% "F" | syms[[sym_names[i+1]]] %among% "T")
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  # Add a starting unit
  clauses[[length(clauses) + 1]] = as.CnfClause(syms[[sym_names[1]]] %among% "T")

  # Add some random clauses
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(2:min(4, n_vars), 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Binary chain: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Binary operations stress ===
cat("\n=== Binary operations ===\n")
set.seed(215004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  make_f = function() {
    n_cl = sample(2:4, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  # Test complex expression
  result = tryCatch({
    r = (f1 & f2) | (!f1 & !f2)
    !!r
  }, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [binops-%d]: %s\n", trial, result$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  expected = !!((t_f1 & t_f2) | (!t_f1 & !t_f2))

  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [binops-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Binary operations: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
