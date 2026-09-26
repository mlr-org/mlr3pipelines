#!/usr/bin/env Rscript
# Final sweep experiment - covers diverse configurations with truth-table verification:
# - 2 vars domain 6 (unusual domain size)
# - 3 vars domain 3 (well-studied but with many clauses)
# - 5 vars domain 2 (high var count, binary)
# - 4 vars mixed domains (asymmetric)
# - 6 vars domain 2 with 20 clauses
# - 3 vars domain 5 with 15 clauses
# Each configuration: generate random formulas, verify against truth table.
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

run_config = function(config_name, n_vars, domain_sizes, n_clauses_range, n_trials) {
  cat(sprintf("\n=== Config: %s ===\n", config_name))
  for (trial in 1:n_trials) {
    u = CnfUniverse()
    syms = list()
    for (i in 1:n_vars) {
      vname = paste0("X", i)
      ds = if (length(domain_sizes) == 1) domain_sizes else domain_sizes[i]
      d = paste0("v", 1:ds)
      syms[[vname]] = CnfSymbol(u, vname, d)
    }

    n_clauses = sample(n_clauses_range[1]:n_clauses_range[2], 1)
    clauses = list()
    for (j in 1:n_clauses) {
      n_sym = sample(1:min(3, n_vars), 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) {
        d = get(unclass(syms[[s]]), u)
        n_vals = sample(1:(length(d) - 1), 1)
        syms[[s]] %among% sample(d, n_vals)
      })
      clauses[[j]] = as.CnfClause(Reduce(`|`, atoms))
    }

    n_tests <<- n_tests + 1
    result = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures <<- n_failures + 1
      cat(sprintf("ERROR [%s-%d]: %s\n", config_name, trial, result$message)); next
    }
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s-%d]: semantic mismatch\n", config_name, trial))
    }
  }
  cat(sprintf("  %s: %d tests, %d failures\n", config_name, n_tests, n_failures))
}

set.seed(116001)

# Config 1: 2 vars, domain 6
run_config("2v-d6", 2, 6, c(3, 8), 500)

# Config 2: 3 vars, domain 3, many clauses
run_config("3v-d3-many", 3, 3, c(8, 20), 500)

# Config 3: 5 vars, binary
run_config("5v-binary", 5, 2, c(5, 15), 500)

# Config 4: 4 vars, mixed domains (2,3,4,5)
run_config("4v-mixed", 4, c(2, 3, 4, 5), c(4, 10), 500)

# Config 5: 6 vars, binary, 20 clauses
run_config("6v-binary-20cl", 6, 2, c(15, 25), 300)

# Config 6: 3 vars, domain 5, 15 clauses
run_config("3v-d5-15cl", 3, 5, c(10, 20), 300)

# Config 7: 7 vars, binary, 3-SAT style
run_config("7v-3sat", 7, 2, c(10, 30), 200)

# Config 8: 2 vars, domain 10 (large domain, few vars)
run_config("2v-d10", 2, 10, c(3, 8), 200)

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
