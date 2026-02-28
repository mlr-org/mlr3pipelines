#!/usr/bin/env Rscript
# Compare simplifier output against naive evaluation on random formulas.
# This uses a completely independent evaluation path that:
# 1. Takes the ORIGINAL clauses (before simplification)
# 2. Evaluates them naively against all possible assignments
# 3. Compares with the simplified formula's evaluation
# Key difference from other experiments: uses extremely diverse configurations
# and completely random clause generation with no structural bias.
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

configs = list(
  # Standard configs
  list(label = "2v-d2", n_vars = 2, dom_size = 2, n_clauses = c(2, 6), trials = 500),
  list(label = "2v-d3", n_vars = 2, dom_size = 3, n_clauses = c(2, 8), trials = 500),
  list(label = "3v-d2", n_vars = 3, dom_size = 2, n_clauses = c(2, 10), trials = 500),
  list(label = "3v-d3", n_vars = 3, dom_size = 3, n_clauses = c(3, 10), trials = 300),
  list(label = "4v-d2", n_vars = 4, dom_size = 2, n_clauses = c(3, 12), trials = 300),
  list(label = "2v-d5", n_vars = 2, dom_size = 5, n_clauses = c(2, 8), trials = 300),
  list(label = "3v-d4", n_vars = 3, dom_size = 4, n_clauses = c(3, 8), trials = 200),
  list(label = "5v-d2", n_vars = 5, dom_size = 2, n_clauses = c(4, 15), trials = 200),
  list(label = "4v-d3", n_vars = 4, dom_size = 3, n_clauses = c(3, 10), trials = 150),
  list(label = "2v-d8", n_vars = 2, dom_size = 8, n_clauses = c(3, 10), trials = 200),
  list(label = "6v-d2", n_vars = 6, dom_size = 2, n_clauses = c(5, 20), trials = 100),
  list(label = "3v-d5", n_vars = 3, dom_size = 5, n_clauses = c(3, 10), trials = 100)
)

for (cfg in configs) {
  cat(sprintf("\n=== Config: %s ===\n", cfg$label))
  set.seed(130000 + cfg$n_vars * 100 + cfg$dom_size)

  for (trial in 1:cfg$trials) {
    u = CnfUniverse()
    d = paste0("v", 1:cfg$dom_size)
    syms = list()
    for (i in 1:cfg$n_vars) {
      vname = paste0("X", i)
      syms[[vname]] = CnfSymbol(u, vname, d)
    }

    n_cl = sample(cfg$n_clauses[1]:cfg$n_clauses[2], 1)
    clauses = lapply(1:n_cl, function(j) {
      # Completely random clause: random number of symbols, random values
      n_sym = sample(1:cfg$n_vars, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) {
        n_vals = sample(1:(cfg$dom_size - 1), 1)
        syms[[s]] %among% sample(d, n_vals)
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) < 2) next

    n_tests = n_tests + 1
    result = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [%s-%d]: %s\n", cfg$label, trial, result$message)); next
    }
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [%s-%d]: semantic mismatch\n", cfg$label, trial))
    }
  }
  cat(sprintf("  %s: %d tests, %d failures\n", cfg$label, n_tests, n_failures))
}

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
