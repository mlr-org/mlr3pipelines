#!/usr/bin/env Rscript
# Test CnfFormula constructor with mixed inputs:
# - Mix of CnfClause and CnfFormula objects
# - Multiple CnfFormula objects combined
# - CnfFormula containing simplified formulas (post-simplification)
# - Roundtrip: construct -> simplify -> combine -> simplify again
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

# === Mix of CnfClause and CnfFormula ===
cat("=== Mixed CnfClause + CnfFormula ===\n")
set.seed(173001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create some CnfClauses
  cls = lapply(1:sample(2:3, 1), function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]

  # Create a CnfFormula
  f_cls = lapply(1:sample(1:2, 1), function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f_cls = f_cls[!sapply(f_cls, function(x) isTRUE(unclass(x)))]
  if (length(f_cls) < 1 || length(cls) < 1) next
  inner_f = tryCatch(CnfFormula(f_cls), error = function(e) NULL)
  if (is.null(inner_f)) next

  # Construct: mix of clauses and formula
  mixed = c(cls, list(inner_f))

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(mixed), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mixed-%d]: %s\n", trial, result$message)); next
  }

  # Verify semantics: should equal CnfFormula(all_clauses)
  all_clauses = c(cls, as.list(inner_f))
  all_clauses = all_clauses[!sapply(all_clauses, function(x) {
    b = unclass(x)
    isTRUE(b) || isFALSE(b)
  })]
  if (length(all_clauses) < 1) next

  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(all_clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixed-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Mixed inputs: %d tests, %d failures\n", n_tests, n_failures))

# === Multiple CnfFormulas combined ===
cat("\n=== Multiple CnfFormulas ===\n")
set.seed(173002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    cls = lapply(1:sample(1:3, 1), function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)

  # CnfFormula(list(f1, f2, f3)) should be AND of all
  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(list(f1, f2, f3)), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multi-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  if (!all(truth == (t1 & t2 & t3))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multi-%d]: semantic mismatch\n", trial))
  }

  # Should also equal f1 & f2 & f3
  n_tests = n_tests + 1
  result2 = tryCatch(f1 & f2 & f3, error = function(e) NULL)
  if (!is.null(result2)) {
    truth2 = evaluate_formula(result2, u)
    if (!all(truth2 == (t1 & t2 & t3))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [multi-op-%d]\n", trial))
    }
  }
}
cat(sprintf("  Multiple formulas: %d tests, %d failures\n", n_tests, n_failures))

# === Re-simplification: construct with pre-simplified formulas ===
cat("\n=== Re-simplification ===\n")
set.seed(173003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    cls = lapply(1:sample(2:4, 1), function(j) {
      n_sym = sample(1:3, 1)
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

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  # f1 & f2 creates a simplified result
  combined = tryCatch(f1 & f2, error = function(e) NULL)
  if (is.null(combined) || is.logical(unclass(combined))) next

  tc = evaluate_formula(combined, u)

  # Now use this in a constructor with another formula
  f3 = make_f()
  if (is.null(f3)) next
  t3 = evaluate_formula(f3, u)

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(list(combined, f3)), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [resimp-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  if (!all(truth == (tc & t3))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [resimp-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Re-simplification: %d tests, %d failures\n", n_tests, n_failures))

# === Formula from clause list + formula list interleaved ===
cat("\n=== Interleaved clause + formula ===\n")
set.seed(173004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  items = list()
  expected_clauses = list()

  for (j in 1:sample(3:6, 1)) {
    if (sample(c(TRUE, FALSE), 1)) {
      # Add a clause
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      cl = as.CnfClause(Reduce(`|`, atoms))
      if (!isTRUE(unclass(cl))) {
        items[[length(items) + 1]] = cl
        expected_clauses[[length(expected_clauses) + 1]] = cl
      }
    } else {
      # Add a formula
      cls = lapply(1:sample(1:2, 1), function(k) {
        n_sym = sample(1:3, 1)
        chosen = sample(names(syms), n_sym)
        atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
        as.CnfClause(Reduce(`|`, atoms))
      })
      cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
      if (length(cls) > 0) {
        f = tryCatch(CnfFormula(cls), error = function(e) NULL)
        if (!is.null(f) && !is.logical(unclass(f))) {
          items[[length(items) + 1]] = f
          for (cl in as.list(f)) {
            if (!isTRUE(unclass(cl)) && !isFALSE(unclass(cl))) {
              expected_clauses[[length(expected_clauses) + 1]] = cl
            }
          }
        }
      }
    }
  }

  if (length(items) < 2) next
  expected_clauses = expected_clauses[!sapply(expected_clauses, function(x) isTRUE(unclass(x)) || isFALSE(unclass(x)))]
  if (length(expected_clauses) < 1) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(items), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [interleave-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(expected_clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [interleave-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Interleaved: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
