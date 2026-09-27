#!/usr/bin/env Rscript
# Binary domain (2 values per variable) with many variables (6-10).
# Binary domains are special: complement of a single value IS a single value,
# so SSE, HLA, and unit propagation interact differently.
# Also tests with large numbers of clauses in binary setting.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === 6-variable binary formulas ===
cat("=== 6-var binary ===\n")
set.seed(255001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("0", "1")
  sym_names = paste0("V", 1:6)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(4:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) {
      # In binary domain, atom is either {0}, {1}, or {0,1} (tautology for that symbol)
      vals = sample(dom, sample(1:2, 1))
      syms[[s]] %among% vals
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [6var-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)

  # Evaluate raw clauses directly
  raw_truth = rep(TRUE, length(truth))
  for (cl in clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, length(truth)); break }
    varnames = ls(u)
    domains = lapply(varnames, function(v) get(v, u))
    names(domains) = varnames
    assignments = expand.grid(domains, stringsAsFactors = FALSE)
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [6var-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  6-var binary: %d tests, %d failures\n", n_tests, n_failures))

# === 8-variable binary formulas ===
cat("\n=== 8-var binary ===\n")
set.seed(255002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("0", "1")
  sym_names = paste0("V", 1:8)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(5:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:5, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, 1)  # single-value atoms only in binary domain
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [8var-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [8var-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  8-var binary: %d tests, %d failures\n", n_tests, n_failures))

# === Binary formulas with operations ===
cat("\n=== Binary operations ===\n")
set.seed(255003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1")
  sym_names = paste0("V", 1:6)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(2:5, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
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
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  op = sample(1:3, 1)
  if (op == 1) {
    r = tryCatch(f1 & f2, error = function(e) NULL)
    expected = t1 & t2
  } else if (op == 2) {
    r = tryCatch(f1 | f2, error = function(e) NULL)
    expected = t1 | t2
  } else {
    r = tryCatch(!f1, error = function(e) NULL)
    expected = !t1
  }
  if (!is.null(r) && !all(evaluate_formula(r, u) == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [binop-%d]: op=%d mismatch\n", trial, op))
  }
}
cat(sprintf("  Binary operations: %d tests, %d failures\n", n_tests, n_failures))

# === 10-variable binary, many clauses ===
cat("\n=== 10-var binary many clauses ===\n")
set.seed(255004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1")
  sym_names = paste0("V", 1:10)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(10:30, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:6, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [10var-%d]: %s\n", trial, result$message)); next
  }
  # 10 binary vars = 1024 assignments, manageable
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [10var-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  10-var binary many clauses: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
