#!/usr/bin/env Rscript
# Final mega-stress test: large-scale randomized testing across many configurations.
# 5000 tests with varied parameters.
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

cat("=== Final mega stress test ===\n")
set.seed(98001)

configs = list(
  # n_vars, dom_sizes (per var or single), n_cl_range, sym_per_clause
  list(nv = 3, ds = c(2, 2, 2), ncl = c(3, 8), spc = c(1, 3)),   # 3 binary
  list(nv = 4, ds = c(3, 3, 3, 3), ncl = c(3, 8), spc = c(1, 3)), # 4 ternary
  list(nv = 5, ds = c(2, 2, 2, 2, 2), ncl = c(5, 15), spc = c(2, 4)), # 5 binary, many clauses
  list(nv = 3, ds = c(5, 5, 5), ncl = c(3, 6), spc = c(1, 3)),    # 3 quinary
  list(nv = 2, ds = c(8, 8), ncl = c(2, 5), spc = c(1, 2)),       # 2 large-domain
  list(nv = 6, ds = c(2, 2, 2, 2, 2, 2), ncl = c(4, 12), spc = c(2, 4)), # 6 binary
  list(nv = 3, ds = c(1, 3, 5), ncl = c(2, 5), spc = c(1, 3)),    # asymmetric
  list(nv = 2, ds = c(10, 10), ncl = c(2, 4), spc = c(1, 2)),     # 2 large
  list(nv = 4, ds = c(2, 3, 4, 5), ncl = c(3, 7), spc = c(2, 4)), # varied domains
  list(nv = 8, ds = rep(2, 8), ncl = c(5, 20), spc = c(2, 5))     # 8 binary, high clause count
)

tests_per_config = 500

for (cfg_idx in seq_along(configs)) {
  cfg = configs[[cfg_idx]]

  for (trial in 1:tests_per_config) {
    u = CnfUniverse()
    syms = list()
    doms = list()
    for (v in 1:cfg$nv) {
      vname = paste0("V", v)
      d = paste0("d", 1:cfg$ds[v])
      syms[[vname]] = CnfSymbol(u, vname, d)
      doms[[vname]] = d
    }

    n_cl = sample(cfg$ncl[1]:cfg$ncl[2], 1)
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(cfg$spc[1]:min(cfg$spc[2], cfg$nv), 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) {
        d = doms[[s]]
        syms[[s]] %among% sample(d, sample(1:max(1, length(d) - 1), 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })

    f = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(f, "error")) {
      n_tests = n_tests + 1; n_failures = n_failures + 1
      cat(sprintf("ERROR [mega-c%d-t%d]: %s\n", cfg_idx, trial, f$message)); next
    }
    check_fc(f, clauses, u, sprintf("mega-c%d-t%d", cfg_idx, trial))
  }
  cat(sprintf("  Config %d (%d vars, domains %s): %d tests so far\n",
    cfg_idx, cfg$nv, paste(cfg$ds, collapse = ","), n_tests))
}

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
