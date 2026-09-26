#!/usr/bin/env Rscript
# Test with many variables (7-8) to exercise more code paths
# Sparse clause coverage: not all variables appear in every clause
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
    cat(sprintf("  Assignment: %s\n",
      paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
    return(FALSE)
  }
  TRUE
}

set.seed(99991)

# Strategy 1: 7 binary variables (128 assignments)
cat("=== 7 binary variables ===\n")
for (trial in 1:300) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:7) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }

  n_clauses = sample(3:10, 1)
  clauses = lapply(1:n_clauses, function(j) {
    # Each clause mentions 1-4 variables (sparse)
    n_atoms = sample(1:4, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [7bin trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("7bin-%d", trial))
}
cat(sprintf("  7 binary: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 2: 8 binary variables (256 assignments) - fewer trials
cat("\n=== 8 binary variables ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:8) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }

  n_clauses = sample(4:12, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:5, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [8bin trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("8bin-%d", trial))
}
cat(sprintf("  8 binary: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 3: 5 variables with mixed domain sizes
cat("\n=== 5 vars mixed domains ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  dom_sizes = c(2, 2, 3, 3, 4)  # total: 2*2*3*3*4 = 144 assignments
  for (v in 1:5) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  n_clauses = sample(4:10, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      n_vals = sample(1:max(1, length(dom) - 1), 1)
      syms[[s]] %among% sample(dom, n_vals)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [5mix trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("5mix-%d", trial))
}
cat(sprintf("  5 mixed: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 4: Asymmetric clauses - some with 1 symbol, some with 5+
cat("\n=== Asymmetric clause sizes ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:6) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:sample(2:3, 1)))
  }

  clauses = list()
  # Add 1-2 unit clauses
  for (i in 1:sample(1:2, 1)) {
    s = sample(names(syms), 1)
    dom = u[[s]]
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1)))
  }
  # Add 2-3 wide clauses (4-5 symbols)
  for (i in 1:sample(2:3, 1)) {
    n_atoms = sample(4:min(5, length(syms)), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  # Add 2-3 narrow clauses (2 symbols)
  for (i in 1:sample(2:3, 1)) {
    chosen = sample(names(syms), 2)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [asym trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("asym-%d", trial))
}
cat(sprintf("  Asymmetric: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
