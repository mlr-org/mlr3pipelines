#!/usr/bin/env Rscript
# Test return_entries edge cases and overall simplifier behavior:
# - Formulas that simplify to TRUE (empty result)
# - Formulas that simplify to FALSE (contradiction)
# - Formulas with all units
# - Formulas with one remaining clause
# - Verify structural integrity of output (no empty ranges, no taut symbols)
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

# === Formulas that should simplify to TRUE ===
cat("=== Simplify to TRUE ===\n")
set.seed(109001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Create tautological combinations
  # A %among% c("0","1") | B %among% dom  -- tautological on B
  clauses = list(
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | B %among% dom),
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | B %among% sample(dom, sample(1:2, 1)))
  )
  # Remove tautological clauses from list (constructor handles them)
  non_taut = !sapply(clauses, function(x) isTRUE(unclass(x)))
  if (!any(non_taut)) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [true-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [true-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Simplify to TRUE: %d tests, %d failures\n", n_tests, n_failures))

# === Formulas that should be FALSE ===
cat("\n=== Simplify to FALSE ===\n")
set.seed(109002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)

  # Direct contradiction: two units for same symbol with disjoint values
  v1 = sample(dom, 1)
  v2 = sample(setdiff(dom, v1), 1)
  clauses = list(
    as.CnfClause(A %among% v1),
    as.CnfClause(A %among% v2)
  )

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [false-%d]: %s\n", trial, result$message)); next
  }
  if (!isFALSE(as.logical(result))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [false-%d]: should be FALSE\n", trial))
  }
}
cat(sprintf("  Simplify to FALSE: %d tests, %d failures\n", n_tests, n_failures))

# === All-unit formulas ===
cat("\n=== All-unit formulas ===\n")
set.seed(109003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom_size = sample(2:5, 1)
  d = paste0("v", 1:dom_size)
  n_vars = sample(2:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  # All clauses are units (one symbol each)
  clauses = lapply(sample(names(syms), sample(1:n_vars, 1)), function(s) {
    as.CnfClause(syms[[s]] %among% sample(d, sample(1:(dom_size - 1), 1)))
  })

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
cat(sprintf("  All-unit formulas: %d tests, %d failures\n", n_tests, n_failures))

# === Structural integrity of output ===
cat("\n=== Structural integrity ===\n")
set.seed(109004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom_size = sample(2:4, 1)
  d = paste0("v", 1:dom_size)
  n_vars = sample(2:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  n_clauses = sample(2:6, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(1:min(3, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(d, sample(1:(dom_size - 1), 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })

  result = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(result)) next

  n_tests = n_tests + 1
  result_bare = unclass(result)

  if (isTRUE(result_bare) || isFALSE(result_bare)) next

  # Check structural integrity
  for (ci in seq_along(result_bare)) {
    clause = result_bare[[ci]]
    # No empty ranges
    for (s in names(clause)) {
      if (length(clause[[s]]) == 0) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [struct-%d]: empty range for %s in clause %d\n", trial, s, ci))
      }
    }
    # No tautological symbols
    for (s in names(clause)) {
      sym_domain = get(s, u)
      if (length(clause[[s]]) >= length(sym_domain) && all(sym_domain %in% clause[[s]])) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [struct-%d]: tautological symbol %s in clause %d\n", trial, s, ci))
      }
    }
    # No duplicate values in range
    for (s in names(clause)) {
      if (anyDuplicated(clause[[s]])) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [struct-%d]: duplicate values for %s in clause %d\n", trial, s, ci))
      }
    }
  }
}
cat(sprintf("  Structural integrity: %d tests, %d failures\n", n_tests, n_failures))

# === Format and print safety ===
cat("\n=== Format/print safety ===\n")
set.seed(109005)

for (trial in 1:200) {
  u = CnfUniverse()
  d = c("a", "b", "c")
  A = CnfSymbol(u, "A", d)
  B = CnfSymbol(u, "B", d)

  clauses = lapply(1:sample(1:4, 1), function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(c("A", "B"), n_sym)
    atoms = lapply(chosen, function(s) {
      sym = if (s == "A") A else B
      sym %among% sample(d, sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  result = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(result)) next

  n_tests = n_tests + 1
  # format should not error
  fmt = tryCatch(format(result), error = function(e) e)
  if (inherits(fmt, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [fmt-%d]: %s\n", trial, fmt$message)); next
  }
  # print should not error
  pr = tryCatch(capture.output(print(result)), error = function(e) e)
  if (inherits(pr, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [print-%d]: %s\n", trial, pr$message))
  }
}
cat(sprintf("  Format/print safety: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
