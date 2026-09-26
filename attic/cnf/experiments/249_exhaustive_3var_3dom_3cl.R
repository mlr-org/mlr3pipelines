#!/usr/bin/env Rscript
# Exhaustive 3-variable, 3-domain, 3-clause test:
# Generate a large clause pool and test ALL 3-clause combinations.
# This extends experiment 230 to more clauses per formula.
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

cat("=== Generating clause pool ===\n")
u = CnfUniverse()
dom = c("a", "b", "c")
sym_names = paste0("V", 1:3)
syms = list()
for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

# Generate all possible non-tautological clauses
clause_pool = list()
# For each subset of symbols (1, 2, or 3 symbols)
for (n_sym in 1:3) {
  sym_combos = combn(sym_names, n_sym, simplify = FALSE)
  for (chosen in sym_combos) {
    # For each combination of ranges (1 or 2 values per symbol)
    range_options = lapply(chosen, function(s) {
      all_ranges = list()
      for (k in 1:2) {
        ranges = combn(dom, k, simplify = FALSE)
        all_ranges = c(all_ranges, ranges)
      }
      all_ranges
    })
    range_combos = expand.grid(lapply(range_options, seq_along))
    for (rc_idx in 1:nrow(range_combos)) {
      atoms = lapply(seq_along(chosen), function(i) {
        range_idx = range_combos[rc_idx, i]
        syms[[chosen[i]]] %among% range_options[[i]][[range_idx]]
      })
      cl = as.CnfClause(Reduce(`|`, atoms))
      if (!isTRUE(unclass(cl))) {
        clause_pool[[length(clause_pool) + 1]] = cl
      }
    }
  }
}

cat(sprintf("  Clause pool size: %d\n", length(clause_pool)))

# Test all 3-clause combinations (sample if too many)
n_pool = length(clause_pool)
total_combos = choose(n_pool, 3)
cat(sprintf("\n=== 3-clause combinations (total possible: %d) ===\n", total_combos))

if (total_combos > 500000) {
  # Sample combinations
  cat("  Sampling 100000 combinations\n")
  set.seed(249001)
  for (trial in 1:100000) {
    idx = sort(sample(n_pool, 3))
    clauses = clause_pool[idx]

    n_tests = n_tests + 1
    result = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [3cl-%d]: %s\n", trial, result$message)); next
    }
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [3cl-%d]: semantic mismatch (indices %s)\n", trial, paste(idx, collapse=",")))
    }
    if (trial %% 20000 == 0) {
      cat(sprintf("  Progress: %d tests, %d failures\n", n_tests, n_failures))
    }
  }
} else {
  # Enumerate all
  for (i in 1:(n_pool-2)) {
    for (j in (i+1):(n_pool-1)) {
      for (k in (j+1):n_pool) {
        clauses = clause_pool[c(i, j, k)]
        n_tests = n_tests + 1
        result = tryCatch(CnfFormula(clauses), error = function(e) e)
        if (inherits(result, "error")) {
          n_failures = n_failures + 1
          cat(sprintf("ERROR [3cl-%d,%d,%d]: %s\n", i, j, k, result$message)); next
        }
        truth = evaluate_formula(result, u)
        raw_truth = evaluate_raw_clauses(clauses, u)
        if (!all(truth == raw_truth)) {
          n_failures = n_failures + 1
          cat(sprintf("FAIL [3cl-%d,%d,%d]: semantic mismatch\n", i, j, k))
        }
      }
    }
    if (i %% 20 == 0) {
      cat(sprintf("  Progress: %d/%d outer, %d tests, %d failures\n", i, n_pool-2, n_tests, n_failures))
    }
  }
}

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
