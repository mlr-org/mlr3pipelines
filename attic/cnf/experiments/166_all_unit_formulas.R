#!/usr/bin/env Rscript
# Test formulas where all clauses are units (single symbol each):
# - All units same symbol (must intersect or contradict)
# - All units different symbols (must propagate across each other)
# - Mix of same and different symbol units
# - All units + one multi-symbol clause
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

# === All units same symbol ===
cat("=== All units same symbol ===\n")
set.seed(166001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)

  n_units = sample(2:5, 1)
  clauses = lapply(1:n_units, function(j) {
    as.CnfClause(A %among% sample(dom, sample(1:4, 1)))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [same-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [same-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Same symbol units: %d tests, %d failures\n", n_tests, n_failures))

# === All units different symbols ===
cat("\n=== All units different symbols ===\n")
set.seed(166002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(2:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  clauses = lapply(names(syms), function(s) {
    as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [diff-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [diff-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Different symbol units: %d tests, %d failures\n", n_tests, n_failures))

# === All units + one multi-symbol clause ===
cat("\n=== Units + one multi-symbol ===\n")
set.seed(166003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Some units
  n_units = sample(1:3, 1)
  unit_syms = sample(names(syms), n_units)
  clauses = lapply(unit_syms, function(s) {
    as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
  })

  # One multi-symbol clause
  n_sym = sample(2:3, 1)
  chosen = sample(names(syms), n_sym)
  atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [one-multi-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [one-multi-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Units + one multi: %d tests, %d failures\n", n_tests, n_failures))

# === Random formulas with exactly 1 satisfying assignment ===
cat("\n=== Single satisfying assignment ===\n")
set.seed(166004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create a formula that has very few satisfying assignments
  # Start with units that pin down the assignment
  target = sapply(names(syms), function(s) sample(dom, 1))

  # Unit for each variable (this gives exactly 1 satisfying assignment)
  clauses = lapply(names(syms), function(s) {
    as.CnfClause(syms[[s]] %among% target[[s]])
  })

  # Add some random wider clauses (should be subsumed)
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [single-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [single-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Single assignment: %d tests, %d failures\n", n_tests, n_failures))

# === Large OR distribution stress ===
cat("\n=== Large OR distribution ===\n")
set.seed(166005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(2:4, 1)
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) < 1) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)

  # (f1 | f2) & f3
  n_tests = n_tests + 1
  r = tryCatch((f1 | f2) & f3, error = function(e) NULL)
  if (!is.null(r)) {
    tr = evaluate_formula(r, u)
    if (!all(tr == ((t1 | t2) & t3))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [or-dist-%d]: (f1|f2)&f3\n", trial))
    }
  }

  # f1 | (f2 & f3)
  n_tests = n_tests + 1
  r2 = tryCatch(f1 | (f2 & f3), error = function(e) NULL)
  if (!is.null(r2)) {
    tr2 = evaluate_formula(r2, u)
    if (!all(tr2 == (t1 | (t2 & t3)))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [or-dist2-%d]: f1|(f2&f3)\n", trial))
    }
  }

  # (f1 | f2) | f3
  n_tests = n_tests + 1
  r3 = tryCatch((f1 | f2) | f3, error = function(e) NULL)
  if (!is.null(r3)) {
    tr3 = evaluate_formula(r3, u)
    if (!all(tr3 == (t1 | t2 | t3))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [or-assoc-%d]\n", trial))
    }
  }
}
cat(sprintf("  Large OR distribution: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
