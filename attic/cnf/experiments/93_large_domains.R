#!/usr/bin/env Rscript
# Test with larger domains (10+ values per variable).
# This stresses range intersection/union operations and subset checking.
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

# === 2 variables, domain size 10 ===
cat("=== 2 vars, domain 10 ===\n")
set.seed(93001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = paste0("d", 1:10)
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  syms = list(A = A, B = B)

  n_cl = sample(2:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:8, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [dom10-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("dom10-%d", trial))
}
cat(sprintf("  2 vars domain 10: %d tests, %d failures\n", n_tests, n_failures))

# === 3 variables, domain size 6 ===
cat("\n=== 3 vars, domain 6 ===\n")
set.seed(93002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = paste0("v", 1:6)
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(3:7, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:3, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:4, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [3v6-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("3v6-%d", trial))
}
cat(sprintf("  3 vars domain 6: %d tests, %d failures\n", n_tests, n_failures))

# === 2 variables, domain size 15 (large) ===
cat("\n=== 2 vars, domain 15 ===\n")
set.seed(93003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = paste0("d", 1:15)
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  syms = list(A = A, B = B)

  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:12, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [dom15-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("dom15-%d", trial))
}
cat(sprintf("  2 vars domain 15: %d tests, %d failures\n", n_tests, n_failures))

# === Mixed: one var domain 2, one var domain 12 ===
cat("\n=== Mixed domain 2 + 12 ===\n")
set.seed(93004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom_small = c("0", "1")
  dom_large = paste0("v", 1:12)
  B = CnfSymbol(u, "B", dom_small)
  L = CnfSymbol(u, "L", dom_large)

  syms = list(B = B, L = L)
  doms = list(B = dom_small, L = dom_large)

  n_cl = sample(2:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:max(1, length(d) - 2), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [mix2-12-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("mix2-12-%d", trial))
}
cat(sprintf("  Mixed 2+12: %d tests, %d failures\n", n_tests, n_failures))

# === Near-tautological ranges (range covers n-1 of n domain values) ===
cat("\n=== Near-tautological ranges ===\n")
set.seed(93005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = paste0("v", 1:8)
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:3, 1))
    atoms = lapply(chosen, function(s) {
      # Mostly near-tautological ranges (7 of 8 values)
      if (runif(1) < 0.6) {
        syms[[s]] %among% sample(dom, length(dom) - 1)
      } else {
        syms[[s]] %among% sample(dom, sample(1:4, 1))
      }
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [near-taut-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("near-taut-%d", trial))
}
cat(sprintf("  Near-tautological: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
