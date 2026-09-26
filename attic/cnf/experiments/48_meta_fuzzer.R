#!/usr/bin/env Rscript
# Meta-fuzzer: combines multiple strategies in a single large test
# Randomly picks strategies and tests them in parallel
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

check_fc = function(formula, raw_clauses, universe, label) {
  n_tests <<- n_tests + 1
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in raw_clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }

  simplified_truth = evaluate_formula(formula, universe)
  mismatches = which(raw_truth != simplified_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d (raw=%s, simp=%s)\n",
      label, idx, raw_truth[idx], simplified_truth[idx]))
    return(FALSE)
  }
  TRUE
}

set.seed(48001)

strategies = c("random", "chain", "dense", "units_heavy", "sse_focused",
               "hla_focused", "near_contra", "near_taut", "mixed_sizes",
               "or_distribution", "combined_ops")

cat("=== Meta-fuzzer: 2000 random strategy tests ===\n")

for (trial in 1:2000) {
  strategy = sample(strategies, 1)

  u = CnfUniverse()
  n_vars = switch(strategy,
    "random" = sample(2:5, 1),
    "chain" = sample(3:5, 1),
    "dense" = 3,
    "units_heavy" = sample(3:5, 1),
    "sse_focused" = sample(3:4, 1),
    "hla_focused" = 3,
    "near_contra" = sample(2:4, 1),
    "near_taut" = sample(2:3, 1),
    "mixed_sizes" = sample(4:5, 1),
    "or_distribution" = sample(2:3, 1),
    "combined_ops" = sample(2:3, 1),
    sample(2:4, 1)
  )

  dom_sizes = switch(strategy,
    "near_taut" = sample(4:6, n_vars, replace = TRUE),
    "dense" = sample(2:3, n_vars, replace = TRUE),
    sample(2:4, n_vars, replace = TRUE)
  )

  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  mk_clause = function(min_atoms = 1, max_atoms = n_vars, bias = "random") {
    n_atoms = sample(min_atoms:min(max_atoms, n_vars), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      n_vals = switch(bias,
        "small" = 1,
        "large" = max(1, length(dom) - 1),
        sample(1:max(1, length(dom)-1), 1)
      )
      syms[[s]] %among% sample(dom, n_vals)
    })
    as.CnfClause(Reduce(`|`, atoms))
  }

  clauses = switch(strategy,
    "random" = {
      lapply(1:sample(3:10, 1), function(i) mk_clause())
    },
    "chain" = {
      cl = list()
      for (i in 1:(n_vars-1)) {
        v1 = names(syms)[i]; v2 = names(syms)[i+1]
        d1 = u[[v1]]; d2 = u[[v2]]
        cl[[i]] = as.CnfClause(
          syms[[v1]] %among% sample(d1, sample(1:max(1, length(d1)-1), 1)) |
          syms[[v2]] %among% sample(d2, sample(1:max(1, length(d2)-1), 1))
        )
      }
      for (i in 1:sample(1:3, 1)) cl[[length(cl)+1]] = mk_clause()
      cl
    },
    "dense" = {
      lapply(1:sample(8:15, 1), function(i) mk_clause(1, 3))
    },
    "units_heavy" = {
      cl = list()
      for (i in 1:sample(2:min(4, n_vars), 1)) {
        s = sample(names(syms), 1)
        dom = u[[s]]
        cl[[length(cl)+1]] = as.CnfClause(syms[[s]] %among% sample(dom, max(1, length(dom)-1)))
      }
      for (i in 1:sample(3:6, 1)) cl[[length(cl)+1]] = mk_clause(2, n_vars)
      cl
    },
    "sse_focused" = {
      lapply(1:sample(4:8, 1), function(i) mk_clause(2, 2))
    },
    "hla_focused" = {
      cl = list()
      base_atoms = sample(names(syms), 2)
      bv = lapply(base_atoms, function(s) {
        dom = u[[s]]
        sample(dom, sample(1:max(1, length(dom)-1), 1))
      })
      names(bv) = base_atoms
      vary = setdiff(names(syms), base_atoms)[1]
      vd = u[[vary]]
      for (i in seq_along(vd)) {
        atoms = list()
        for (s in base_atoms) atoms[[length(atoms)+1]] = syms[[s]] %among% bv[[s]]
        atoms[[length(atoms)+1]] = syms[[vary]] %among% vd[i]
        cl[[i]] = as.CnfClause(Reduce(`|`, atoms))
      }
      for (i in 1:sample(1:3, 1)) cl[[length(cl)+1]] = mk_clause()
      cl
    },
    "near_contra" = {
      lapply(1:sample(4:8, 1), function(i) mk_clause(1, n_vars, "small"))
    },
    "near_taut" = {
      lapply(1:sample(3:6, 1), function(i) mk_clause(2, n_vars, "large"))
    },
    "mixed_sizes" = {
      cl = list()
      for (i in 1:sample(1:2, 1)) cl[[length(cl)+1]] = mk_clause(1, 1)  # units
      for (i in 1:sample(2:3, 1)) cl[[length(cl)+1]] = mk_clause(2, 2)  # 2-sym
      for (i in 1:sample(1:3, 1)) cl[[length(cl)+1]] = mk_clause(3, n_vars)  # 3+ sym
      cl
    },
    "or_distribution" = {
      lapply(1:sample(2:4, 1), function(i) mk_clause())
    },
    "combined_ops" = {
      lapply(1:sample(2:4, 1), function(i) mk_clause())
    },
    lapply(1:sample(3:6, 1), function(i) mk_clause())
  )

  if (strategy == "or_distribution") {
    # Build two formulas and OR them
    mid = length(clauses) %/% 2
    f1 = tryCatch(CnfFormula(clauses[1:max(1,mid)]), error = function(e) NULL)
    f2 = tryCatch(CnfFormula(clauses[(mid+1):length(clauses)]), error = function(e) NULL)
    if (is.null(f1) || is.null(f2)) next
    f = tryCatch(f1 | f2, error = function(e) e)
    if (inherits(f, "error")) {
      n_tests = n_tests + 1; n_failures = n_failures + 1
      cat(sprintf("ERROR [meta trial %d strategy=%s]: %s\n", trial, strategy, f$message)); next
    }
    # Verify OR semantics
    n_tests = n_tests + 1
    t_f = evaluate_formula(f, u)
    t1 = evaluate_formula(f1, u)
    t2 = evaluate_formula(f2, u)
    if (!all(t_f == (t1 | t2))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [meta trial %d strategy=%s]: OR semantic mismatch\n", trial, strategy))
    }
    next
  }

  if (strategy == "combined_ops") {
    # Build formula, negate, AND with another
    f1 = tryCatch(CnfFormula(clauses), error = function(e) NULL)
    if (is.null(f1)) next
    f2 = tryCatch(!f1, error = function(e) NULL)
    if (is.null(f2)) next
    # f1 & !f1 should be FALSE
    f3 = tryCatch(f1 & f2, error = function(e) NULL)
    if (is.null(f3)) next
    n_tests = n_tests + 1
    t_f = evaluate_formula(f3, u)
    if (any(t_f)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [meta trial %d strategy=%s]: f & !f not all FALSE\n", trial, strategy))
    }
    next
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [meta trial %d strategy=%s]: %s\n", trial, strategy, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("meta-%d-%s", trial, strategy))
}

cat(sprintf("\n=== META TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
