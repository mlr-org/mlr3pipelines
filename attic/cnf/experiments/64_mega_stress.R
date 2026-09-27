#!/usr/bin/env Rscript
# Massive randomized stress test - 5000 formulas with varied parameters
# Combines all strategies in a single comprehensive test
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

set.seed(64001)

cat("=== Mega stress test: 5000 random formulas ===\n")

for (trial in 1:5000) {
  # Random parameters
  n_vars = sample(2:7, 1)

  # Limit total assignments to keep runtime reasonable
  max_dom_size = switch(as.character(n_vars),
    "2" = 8, "3" = 5, "4" = 4, "5" = 3, "6" = 2, "7" = 2, 2
  )

  u = CnfUniverse()
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    dom_size = sample(2:max_dom_size, 1)
    syms[[vname]] = CnfSymbol(u, vname, paste0("d", 1:dom_size))
  }

  n_cl = sample(2:min(20, 3 + n_vars * 2), 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:min(n_vars, 4), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [mega %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("mega-%d", trial))

  if (trial %% 1000 == 0) cat(sprintf("  ... %d tests done, %d failures\n", n_tests, n_failures))
}

cat(sprintf("\n=== MEGA TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
