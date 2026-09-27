#!/usr/bin/env Rscript
# XOR and parity constraint testing:
# XOR in CNF is notoriously hard for simplification.
# Test that XOR-like formulas are handled correctly semantically.
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

# === Pattern 1: XOR via formula operations: (f1 & !f2) | (!f1 & f2) ===
cat("=== XOR via operations ===\n")
set.seed(237001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
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

  # XOR: (f1 & !f2) | (!f1 & f2)
  result = tryCatch({
    part1 = f1 & !f2
    part2 = !f1 & f2
    part1 | part2
  }, error = function(e) e)

  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [xor-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = xor(t1, t2)
  actual = evaluate_formula(result, u)

  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [xor-%d]: XOR mismatch\n", trial))
  }
}
cat(sprintf("  XOR via operations: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Binary XOR in CNF directly ===
cat("\n=== Binary XOR in CNF ===\n")
set.seed(237002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("T", "F")
  n_vars = sample(3:6, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # XOR(V1, V2) in CNF: (V1=T | V2=T) & (V1=F | V2=F)
  clauses = list()
  for (k in 1:sample(1:min(3, n_vars-1), 1)) {
    pair = sample(sym_names, 2)
    # XOR: exactly one is T
    cl1 = as.CnfClause(syms[[pair[1]]] %among% "T" | syms[[pair[2]]] %among% "T")
    cl2 = as.CnfClause(syms[[pair[1]]] %among% "F" | syms[[pair[2]]] %among% "F")
    if (!isTRUE(unclass(cl1))) clauses[[length(clauses) + 1]] = cl1
    if (!isTRUE(unclass(cl2))) clauses[[length(clauses) + 1]] = cl2
  }

  # Additional random clauses
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
    cat(sprintf("ERROR [bxor-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [bxor-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Binary XOR CNF: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: If-then-else via formulas: (cond & then) | (!cond & else) ===
cat("\n=== If-then-else ===\n")
set.seed(237003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  cond = make_f(); then_f = make_f(); else_f = make_f()
  if (any(sapply(list(cond, then_f, else_f), is.null))) next

  n_tests = n_tests + 1

  result = tryCatch({
    (cond & then_f) | (!cond & else_f)
  }, error = function(e) e)

  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ite-%d]: %s\n", trial, result$message)); next
  }

  t_cond = evaluate_formula(cond, u)
  t_then = evaluate_formula(then_f, u)
  t_else = evaluate_formula(else_f, u)
  expected = (t_cond & t_then) | (!t_cond & t_else)
  actual = evaluate_formula(result, u)

  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ite-%d]: ITE mismatch\n", trial))
  }
}
cat(sprintf("  If-then-else: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Many XOR constraints create hard problems ===
cat("\n=== Many XOR constraints ===\n")
set.seed(237004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1")
  n_vars = sample(5:8, 1)
  sym_names = paste0("B", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  clauses = list()
  # Multiple XOR constraints
  n_xors = sample(2:4, 1)
  for (k in 1:n_xors) {
    pair = sample(sym_names, 2)
    cl1 = as.CnfClause(syms[[pair[1]]] %among% "0" | syms[[pair[2]]] %among% "0")
    cl2 = as.CnfClause(syms[[pair[1]]] %among% "1" | syms[[pair[2]]] %among% "1")
    if (!isTRUE(unclass(cl1))) clauses[[length(clauses) + 1]] = cl1
    if (!isTRUE(unclass(cl2))) clauses[[length(clauses) + 1]] = cl2
  }

  # Some random clauses
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:min(4, n_vars), 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 6) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mxor-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mxor-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Many XOR: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
