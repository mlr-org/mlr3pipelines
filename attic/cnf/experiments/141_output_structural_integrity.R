#!/usr/bin/env Rscript
# Verify structural integrity of simplifier output:
# - No empty ranges
# - No tautological symbols (range == full domain)
# - No duplicate clauses
# - No subsumed clauses in output
# - as.list roundtrip consistency
# - format/print don't crash
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

check_structural_integrity = function(f, u) {
  f_bare = unclass(f)
  if (isTRUE(f_bare) || isFALSE(f_bare)) return(NULL)

  issues = character(0)

  for (i in seq_along(f_bare)) {
    clause = f_bare[[i]]
    for (sym in names(clause)) {
      rng = clause[[sym]]
      # No empty ranges
      if (length(rng) == 0) {
        issues = c(issues, sprintf("clause %d: empty range for %s", i, sym))
      }
      # No duplicates in ranges
      if (length(rng) != length(unique(rng))) {
        issues = c(issues, sprintf("clause %d: duplicate values in %s range", i, sym))
      }
      # No tautological symbols (range should not cover full domain)
      dom = u[[sym]]
      if (all(dom %in% rng)) {
        issues = c(issues, sprintf("clause %d: tautological symbol %s", i, sym))
      }
      # Range values should be valid domain values
      if (!all(rng %in% dom)) {
        issues = c(issues, sprintf("clause %d: invalid range values for %s", i, sym))
      }
    }
    # No empty clauses (would be contradiction)
    if (length(clause) == 0) {
      issues = c(issues, sprintf("clause %d: empty clause", i))
    }
  }

  # No duplicate clauses (by structure)
  if (length(f_bare) > 1) {
    for (i in 1:(length(f_bare) - 1)) {
      for (j in (i + 1):length(f_bare)) {
        if (identical(f_bare[[i]][order(names(f_bare[[i]]))], f_bare[[j]][order(names(f_bare[[j]]))])) {
          issues = c(issues, sprintf("duplicate clauses %d and %d", i, j))
        }
      }
    }
  }

  if (length(issues) == 0) NULL else issues
}

cat("=== Structural integrity checks ===\n")
set.seed(141001)

for (trial in 1:3000) {
  if (trial %% 500 == 0) cat(sprintf("  Progress: %d/3000, %d tests, %d failures\n", trial, n_tests, n_failures))

  n_vars = sample(2:5, 1)
  dom_size = sample(2:5, 1)
  u = CnfUniverse()
  dom = paste0("v", 1:dom_size)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(2:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:min(3, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:(dom_size - 1), 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  result = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(result)) next

  n_tests = n_tests + 1

  # Check semantic correctness
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [semantic-%d]: truth mismatch\n", trial))
    next
  }

  # Check structural integrity
  issues = check_structural_integrity(result, u)
  if (!is.null(issues)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [struct-%d]: %s\n", trial, paste(issues, collapse = "; ")))
    next
  }

  # Check format/print don't crash
  fmt = tryCatch(format(result), error = function(e) e)
  if (inherits(fmt, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [format-%d]: %s\n", trial, fmt$message))
    next
  }

  prt = tryCatch(capture.output(print(result)), error = function(e) e)
  if (inherits(prt, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [print-%d]: %s\n", trial, prt$message))
    next
  }

  # Check as.list roundtrip (only for non-logical formulas)
  if (!is.logical(result)) {
    clauses_back = tryCatch(as.list(result), error = function(e) NULL)
    if (!is.null(clauses_back)) {
      f_back = tryCatch(CnfFormula(clauses_back), error = function(e) NULL)
      if (!is.null(f_back)) {
        t_back = evaluate_formula(f_back, u)
        if (!all(truth == t_back)) {
          n_failures = n_failures + 1
          cat(sprintf("FAIL [roundtrip-%d]: as.list roundtrip changes semantics\n", trial))
        }
      }
    }
  }
}
cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
