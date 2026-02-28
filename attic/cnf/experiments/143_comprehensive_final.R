#!/usr/bin/env Rscript
# Comprehensive final stress test: combines many patterns in high volume.
# 10000 trials with 8 strategies, varied configurations.
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

set.seed(143000)
cat("=== Comprehensive final stress test (10000 trials) ===\n")

for (trial in 1:10000) {
  if (trial %% 1000 == 0) cat(sprintf("  Progress: %d/10000, %d tests, %d failures\n", trial, n_tests, n_failures))

  strategy = sample(1:8, 1)
  n_vars = sample(2:6, 1)
  dom_size = sample(2:6, 1)

  u = CnfUniverse()
  dom = paste0("v", 1:dom_size)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_clause = function(min_sym = 1, max_sym = min(3, n_vars)) {
    n_sym = sample(min_sym:max_sym, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:(dom_size - 1), 1)))
    as.CnfClause(Reduce(`|`, atoms))
  }

  make_formula = function(n_cl = sample(1:3, 1)) {
    clauses = lapply(1:n_cl, function(j) make_clause())
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  if (strategy == 1) {
    # === Random formula truth table verification ===
    n_cl = sample(2:10, 1)
    clauses = lapply(1:n_cl, function(j) make_clause())
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) < 2) next

    n_tests = n_tests + 1
    result = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [s1-%d]: %s\n", trial, result$message)); next
    }
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [s1-%d]: semantic mismatch\n", trial))
    }

  } else if (strategy == 2) {
    # === Complement: f & !f = FALSE, f | !f = TRUE ===
    f = make_formula()
    if (is.null(f)) next
    neg_f = tryCatch(!f, error = function(e) NULL)
    if (is.null(neg_f)) next

    n_tests = n_tests + 1
    f_and_neg = tryCatch(f & neg_f, error = function(e) NULL)
    if (!is.null(f_and_neg)) {
      t = evaluate_formula(f_and_neg, u)
      if (any(t)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [s2-and-%d]: f & !f not FALSE\n", trial))
      }
    }

    n_tests = n_tests + 1
    f_or_neg = tryCatch(f | neg_f, error = function(e) NULL)
    if (!is.null(f_or_neg)) {
      t = evaluate_formula(f_or_neg, u)
      if (!all(t)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [s2-or-%d]: f | !f not TRUE\n", trial))
      }
    }

  } else if (strategy == 3) {
    # === Double negation: !!f = f ===
    f = make_formula()
    if (is.null(f)) next
    dbl_neg = tryCatch(!!f, error = function(e) NULL)
    if (is.null(dbl_neg)) next

    n_tests = n_tests + 1
    t_f = evaluate_formula(f, u)
    t_dbl = evaluate_formula(dbl_neg, u)
    if (!all(t_f == t_dbl)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [s3-%d]: !!f != f\n", trial))
    }

  } else if (strategy == 4) {
    # === Distribution: f1 | f2 and (f1 | f2) & f3 ===
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
    # === Absorption: f & (f | g) = f ===
    f = make_formula(sample(1:2, 1))
    g = make_formula(sample(1:2, 1))
    if (is.null(f) || is.null(g)) next

    t_f = evaluate_formula(f, u)

    n_tests = n_tests + 1
    f_or_g = tryCatch(f | g, error = function(e) NULL)
    if (!is.null(f_or_g)) {
      absorbed = tryCatch(f & f_or_g, error = function(e) NULL)
      if (!is.null(absorbed)) {
        t = evaluate_formula(absorbed, u)
        if (!all(t == t_f)) {
          n_failures = n_failures + 1
          cat(sprintf("FAIL [s5-%d]: f & (f|g) != f\n", trial))
        }
      }
    }

  } else if (strategy == 6) {
    # === Incremental vs batch construction ===
    n_cl = sample(3:8, 1)
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
      cat(sprintf("FAIL [s6-%d]: batch != incremental\n", trial))
    }

  } else if (strategy == 7) {
    # === Random operation chain ===
    f = make_formula()
    g = make_formula()
    if (is.null(f) || is.null(g)) next

    t_f = evaluate_formula(f, u)
    t_g = evaluate_formula(g, u)

    op = sample(c("and", "or", "neg"), 1)
    n_tests = n_tests + 1
    result = tryCatch({
      switch(op,
        and = f & g,
        or = f | g,
        neg = !f
      )
    }, error = function(e) NULL)
    if (is.null(result)) next

    expected = switch(op,
      and = t_f & t_g,
      or = t_f | t_g,
      neg = !t_f
    )
    truth = evaluate_formula(result, u)
    if (!all(truth == expected)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [s7-%s-%d]: semantic error\n", op, trial))
    }

  } else if (strategy == 8) {
    # === Units + non-units mixed ===
    n_units = sample(1:min(3, n_vars), 1)
    unit_syms = sample(names(syms), n_units)
    clauses = lapply(unit_syms, function(s) {
      as.CnfClause(syms[[s]] %among% sample(dom, sample(1:(dom_size - 1), 1)))
    })
    n_nonunit = sample(2:6, 1)
    for (j in 1:n_nonunit) {
      clauses[[length(clauses) + 1]] = make_clause(min_sym = 2)
    }
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) < 2) next

    n_tests = n_tests + 1
    result = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [s8-%d]: %s\n", trial, result$message)); next
    }
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [s8-%d]: semantic mismatch\n", trial))
    }
  }
}

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
