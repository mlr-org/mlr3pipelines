#!/usr/bin/env Rscript
# CnfFormula construction edge cases:
# Test various ways of constructing CnfFormulas, including
# as.CnfFormula, the constructor, and c() operations.
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

# === Pattern 1: CnfFormula from single clause ===
cat("=== Single clause formulas ===\n")
set.seed(248001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_sym = sample(1:3, 1)
  chosen = sample(sym_names, n_sym)
  atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  cl = as.CnfClause(Reduce(`|`, atoms))
  if (isTRUE(unclass(cl))) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(list(cl)), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [single-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(list(cl), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [single-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Single clause: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: as.CnfFormula on various inputs ===
cat("\n=== as.CnfFormula conversions ===\n")
set.seed(248002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  n_tests = n_tests + 1

  # as.CnfFormula from CnfAtom
  atom = A %among% sample(dom, sample(1:2, 1))
  result = tryCatch(as.CnfFormula(atom), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [conv-atom-%d]: %s\n", trial, result$message)); next
  }
  # as.CnfFormula from CnfClause
  cl = as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | B %among% sample(dom, sample(1:2, 1)))
  result2 = tryCatch(as.CnfFormula(cl), error = function(e) e)
  if (inherits(result2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [conv-clause-%d]: %s\n", trial, result2$message)); next
  }

  # Verify they evaluate correctly
  t1 = evaluate_formula(result, u)
  t2 = evaluate_formula(result2, u)
  if (any(is.na(t1)) || any(is.na(t2))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [conv-%d]: NA in truth\n", trial))
  }
}
cat(sprintf("  as.CnfFormula: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: AND via c() and CnfFormula ===
cat("\n=== AND via c() + CnfFormula ===\n")
set.seed(248003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1

  # Method 1: Direct CnfFormula construction
  result1 = tryCatch(CnfFormula(clauses), error = function(e) e)

  # Method 2: Build by AND-ing individual formulas
  result2 = tryCatch({
    fs = lapply(clauses, function(cl) CnfFormula(list(cl)))
    Reduce(`&`, fs)
  }, error = function(e) e)

  if (inherits(result1, "error") || inherits(result2, "error")) {
    if (inherits(result1, "error") && inherits(result2, "error")) next  # both error is ok
    n_failures = n_failures + 1
    msg = if (inherits(result1, "error")) result1$message else result2$message
    cat(sprintf("ERROR [cvsand-%d]: %s\n", trial, msg)); next
  }

  raw_truth = evaluate_raw_clauses(clauses, u)
  truth1 = evaluate_formula(result1, u)
  truth2 = evaluate_formula(result2, u)

  if (!all(truth1 == raw_truth) || !all(truth2 == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cvsand-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  c() vs AND: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Formula with all same clauses ===
cat("\n=== Formula with identical clauses ===\n")
set.seed(248004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_sym = sample(1:3, 1)
  chosen = sample(sym_names, n_sym)
  atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  cl = as.CnfClause(Reduce(`|`, atoms))
  if (isTRUE(unclass(cl))) next

  # Repeat the same clause multiple times
  n_rep = sample(2:5, 1)
  clauses = rep(list(cl), n_rep)

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ident-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  single_truth = evaluate_formula(CnfFormula(list(cl)), u)

  if (!all(truth == single_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ident-%d]: repeated clause should equal single\n", trial))
  }
}
cat(sprintf("  Identical clauses: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 5: Empty-ish formulas ===
cat("\n=== Edge case formulas ===\n")
set.seed(248005)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)

  n_tests = n_tests + 1

  # TRUE formula
  f_true = as.CnfFormula(TRUE)
  if (!isTRUE(unclass(f_true))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [edge-%d]: TRUE formula not TRUE\n", trial)); next
  }

  # FALSE formula
  f_false = as.CnfFormula(FALSE)
  if (!isFALSE(unclass(f_false))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [edge-%d]: FALSE formula not FALSE\n", trial)); next
  }

  # Operations with TRUE/FALSE
  f = CnfFormula(list(as.CnfClause(A %among% sample(dom, sample(1:2, 1)))))

  # f & TRUE == f
  r = tryCatch(f & f_true, error = function(e) e)
  if (!inherits(r, "error")) {
    t_f = evaluate_formula(f, u)
    t_r = evaluate_formula(r, u)
    if (!all(t_f == t_r)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [edge-%d]: f & TRUE != f\n", trial)); next
    }
  }

  # f | TRUE == TRUE
  r = tryCatch(f | f_true, error = function(e) e)
  if (!inherits(r, "error")) {
    if (!isTRUE(unclass(r))) {
      t_r = evaluate_formula(r, u)
      if (!all(t_r)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [edge-%d]: f | TRUE != TRUE\n", trial)); next
      }
    }
  }

  # f & FALSE == FALSE
  r = tryCatch(f & f_false, error = function(e) e)
  if (!inherits(r, "error")) {
    if (!isFALSE(unclass(r))) {
      t_r = evaluate_formula(r, u)
      if (any(t_r)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [edge-%d]: f & FALSE != FALSE\n", trial)); next
      }
    }
  }

  # f | FALSE == f
  r = tryCatch(f | f_false, error = function(e) e)
  if (!inherits(r, "error")) {
    t_f = evaluate_formula(f, u)
    t_r = evaluate_formula(r, u)
    if (!all(t_f == t_r)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [edge-%d]: f | FALSE != f\n", trial)); next
    }
  }
}
cat(sprintf("  Edge cases: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
