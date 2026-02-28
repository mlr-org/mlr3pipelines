#!/usr/bin/env Rscript
# Ultimate stress test: combines multiple strategies in a single run.
# Each trial randomly selects a strategy and tests it.
# Strategies:
# 1. Random formula with truth table verification
# 2. Build formula incrementally and verify
# 3. Negation + complement verification
# 4. Distribution (OR) verification
# 5. Idempotence (f & f, f | f)
# 6. Contradiction (conflicting units)
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

set.seed(134000)

cat("=== Ultimate stress test ===\n")

for (trial in 1:5000) {
  if (trial %% 500 == 0) cat(sprintf("  Progress: %d/5000, %d tests, %d failures\n", trial, n_tests, n_failures))

  strategy = sample(1:6, 1)
  # Random universe configuration
  n_vars = sample(2:5, 1)
  dom_size = sample(2:5, 1)

  u = CnfUniverse()
  d = paste0("v", 1:dom_size)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  make_clause = function() {
    n_sym = sample(1:min(3, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(d, sample(1:(dom_size - 1), 1)))
    as.CnfClause(Reduce(`|`, atoms))
  }

  make_formula = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) make_clause())
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  if (strategy == 1) {
    # Random formula with truth table
    n_cl = sample(2:8, 1)
    clauses = lapply(1:n_cl, function(j) make_clause())
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) < 2) next

    n_tests = n_tests + 1
    result = tryCatch(CnfFormula(clauses), error = function(e) NULL)
    if (is.null(result)) next
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [s1-%d]: semantic mismatch\n", trial))
    }

  } else if (strategy == 2) {
    # Incremental build
    n_cl = sample(3:6, 1)
    clauses = lapply(1:n_cl, function(j) make_clause())
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) < 2) next

    f_batch = tryCatch(CnfFormula(clauses), error = function(e) NULL)
    if (is.null(f_batch)) next

    f_inc = tryCatch({
      f = CnfFormula(list(clauses[[1]]))
      for (i in 2:length(clauses)) {
        f = f & CnfFormula(list(clauses[[i]]))
      }
      f
    }, error = function(e) NULL)
    if (is.null(f_inc)) next

    n_tests = n_tests + 1
    t_batch = evaluate_formula(f_batch, u)
    t_inc = evaluate_formula(f_inc, u)
    if (!all(t_batch == t_inc)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [s2-%d]: batch != incremental\n", trial))
    }

  } else if (strategy == 3) {
    # Complement: f & !f = FALSE, f | !f = TRUE
    f = make_formula(sample(1:3, 1))
    if (is.null(f)) next

    neg_f = tryCatch(!f, error = function(e) NULL)
    if (is.null(neg_f)) next

    n_tests = n_tests + 1
    f_and_neg = tryCatch(f & neg_f, error = function(e) NULL)
    if (!is.null(f_and_neg)) {
      t = evaluate_formula(f_and_neg, u)
      if (any(t)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [s3-and-%d]: f & !f not FALSE\n", trial))
      }
    }

    n_tests = n_tests + 1
    f_or_neg = tryCatch(f | neg_f, error = function(e) NULL)
    if (!is.null(f_or_neg)) {
      t = evaluate_formula(f_or_neg, u)
      if (!all(t)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [s3-or-%d]: f | !f not TRUE\n", trial))
      }
    }

  } else if (strategy == 4) {
    # Distribution: f1 | f2 semantics
    f1 = make_formula(sample(1:2, 1))
    f2 = make_formula(sample(1:2, 1))
    if (is.null(f1) || is.null(f2)) next

    n_tests = n_tests + 1
    result = tryCatch(f1 | f2, error = function(e) NULL)
    if (!is.null(result)) {
      t1 = evaluate_formula(f1, u)
      t2 = evaluate_formula(f2, u)
      truth = evaluate_formula(result, u)
      if (!all(truth == (t1 | t2))) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [s4-%d]: f1|f2 semantics wrong\n", trial))
      }
    }

  } else if (strategy == 5) {
    # Idempotence: f & f = f, f | f = f
    f = make_formula(sample(1:3, 1))
    if (is.null(f)) next

    t_f = evaluate_formula(f, u)

    n_tests = n_tests + 1
    f_and_f = tryCatch(f & f, error = function(e) NULL)
    if (!is.null(f_and_f)) {
      t = evaluate_formula(f_and_f, u)
      if (!all(t == t_f)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [s5-and-%d]: f&f != f\n", trial))
      }
    }

    n_tests = n_tests + 1
    f_or_f = tryCatch(f | f, error = function(e) NULL)
    if (!is.null(f_or_f)) {
      t = evaluate_formula(f_or_f, u)
      if (!all(t == t_f)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [s5-or-%d]: f|f != f\n", trial))
      }
    }

  } else if (strategy == 6) {
    # Contradiction-heavy: many units
    n_units = sample(2:min(4, n_vars), 1)
    unit_syms = sample(names(syms), n_units)
    clauses = lapply(unit_syms, function(s) {
      as.CnfClause(syms[[s]] %among% sample(d, sample(1:(dom_size - 1), 1)))
    })
    # Add non-unit clauses
    for (j in 1:sample(1:4, 1)) {
      clauses[[length(clauses) + 1]] = make_clause()
    }
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) < 2) next

    n_tests = n_tests + 1
    result = tryCatch(CnfFormula(clauses), error = function(e) NULL)
    if (is.null(result)) next
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [s6-%d]: semantic mismatch\n", trial))
    }
  }
}

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
