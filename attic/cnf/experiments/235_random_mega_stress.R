#!/usr/bin/env Rscript
# Mega random stress test: 5000 formulas with random configurations.
# Each trial picks random variable count, domain sizes, clause count,
# and clause widths. Pure semantic correctness check.
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

cat("=== Mega random stress ===\n")
set.seed(235000)

configs = list(
  list(n_vars = 3, dom_sizes = c(2, 2, 2), n_cl_range = c(3, 8)),   # 3 binary
  list(n_vars = 4, dom_sizes = c(2, 2, 2, 2), n_cl_range = c(4, 12)), # 4 binary
  list(n_vars = 3, dom_sizes = c(3, 3, 3), n_cl_range = c(3, 8)),   # 3 ternary
  list(n_vars = 4, dom_sizes = c(3, 3, 3, 3), n_cl_range = c(4, 10)), # 4 ternary
  list(n_vars = 2, dom_sizes = c(5, 5), n_cl_range = c(3, 10)),     # 2 quinternary
  list(n_vars = 3, dom_sizes = c(4, 4, 4), n_cl_range = c(4, 10)),  # 3 quaternary
  list(n_vars = 5, dom_sizes = c(2, 2, 2, 2, 2), n_cl_range = c(5, 15)), # 5 binary
  list(n_vars = 3, dom_sizes = c(2, 3, 5), n_cl_range = c(3, 8)),   # mixed
  list(n_vars = 4, dom_sizes = c(2, 2, 4, 4), n_cl_range = c(4, 10)), # mixed2
  list(n_vars = 6, dom_sizes = c(2, 2, 2, 2, 2, 2), n_cl_range = c(6, 18)), # 6 binary
  list(n_vars = 2, dom_sizes = c(8, 8), n_cl_range = c(3, 8)),     # 2 large domain
  list(n_vars = 3, dom_sizes = c(2, 4, 6), n_cl_range = c(3, 8))    # mixed large
)

for (config_idx in seq_along(configs)) {
  cfg = configs[[config_idx]]
  cat(sprintf("\n--- Config %d: %dv, domains %s ---\n", config_idx, cfg$n_vars,
              paste(cfg$dom_sizes, collapse=",")))

  for (trial in 1:500) {
    u = CnfUniverse()
    sym_names = paste0("V", 1:cfg$n_vars)
    syms = list()
    doms = list()
    for (i in 1:cfg$n_vars) {
      d = paste0("v", 1:cfg$dom_sizes[i])
      doms[[sym_names[i]]] = d
      syms[[sym_names[i]]] = CnfSymbol(u, sym_names[i], d)
    }

    n_cl = sample(cfg$n_cl_range[1]:cfg$n_cl_range[2], 1)
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:min(cfg$n_vars, 4), 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) {
        d = doms[[s]]
        syms[[s]] %among% sample(d, sample(1:max(1, length(d)-1), 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) < 2) next

    n_tests = n_tests + 1
    result = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [cfg%d-%d]: %s\n", config_idx, trial, result$message)); next
    }
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [cfg%d-%d]: semantic mismatch\n", config_idx, trial))
    }
  }
  cat(sprintf("  Config %d done: %d tests, %d failures\n", config_idx, n_tests, n_failures))
}

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
