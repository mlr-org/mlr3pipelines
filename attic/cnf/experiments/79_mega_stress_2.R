#!/usr/bin/env Rscript
# Mega stress test 2: Random formulas with more emphasis on
# large domains, many variables, and mixed clause sizes.
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

set.seed(79001)

cat("=== Mega stress 2: 3000 diverse formulas ===\n")

configs = list(
  # (n_vars, max_dom_size, n_cl_range, atoms_per_clause)
  list(2, 6, c(3, 10), c(1, 2)),   # few vars, larger domains
  list(3, 4, c(3, 8), c(1, 3)),    # balanced
  list(4, 3, c(4, 12), c(1, 4)),   # more vars, smaller domains
  list(5, 3, c(5, 15), c(1, 4)),   # many vars
  list(6, 2, c(6, 20), c(1, 5)),   # lots of binary vars
  list(2, 8, c(2, 6), c(1, 2)),    # two vars very large domains
  list(3, 5, c(2, 5), c(2, 3)),    # all wide clauses
  list(4, 3, c(8, 15), c(1, 2)),   # many narrow clauses
  list(8, 2, c(10, 30), c(1, 4)),  # 8 binary vars, many clauses
  list(3, 3, c(3, 6), c(3, 3))     # all clauses full-width
)

for (config_idx in seq_along(configs)) {
  cfg = configs[[config_idx]]
  n_vars = cfg[[1]]
  max_dom = cfg[[2]]
  cl_range = cfg[[3]]
  atom_range = cfg[[4]]

  for (trial in 1:300) {
    u = CnfUniverse()
    syms = list()
    for (v in 1:n_vars) {
      vname = paste0("V", v)
      ds = sample(2:max_dom, 1)
      syms[[vname]] = CnfSymbol(u, vname, paste0("d", 1:ds))
    }

    n_cl = sample(cl_range[1]:cl_range[2], 1)
    clauses = lapply(1:n_cl, function(j) {
      n_atoms = sample(atom_range[1]:min(n_vars, atom_range[2]), 1)
      chosen = sample(names(syms), n_atoms)
      atoms = lapply(chosen, function(s) {
        dom = u[[s]]
        syms[[s]] %among% sample(dom, sample(1:max(1, length(dom) - 1), 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })

    f = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(f, "error")) {
      n_tests = n_tests + 1; n_failures = n_failures + 1
      cat(sprintf("ERROR [mega2-%d-%d]: %s\n", config_idx, trial, f$message)); next
    }
    check_fc(f, clauses, u, sprintf("mega2-%d-%d", config_idx, trial))
  }
  cat(sprintf("  Config %d (%d vars, dom≤%d): %d tests, %d failures\n",
    config_idx, n_vars, max_dom, n_tests, n_failures))
}

cat(sprintf("\n=== MEGA2 TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
