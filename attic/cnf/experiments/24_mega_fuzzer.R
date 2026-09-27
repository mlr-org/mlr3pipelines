#!/usr/bin/env Rscript
# Mega fuzzer: combines multiple strategies, runs many iterations
# Uses different seeds and variable configurations to maximize coverage
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

make_clause = function(syms, universe, max_atoms = 3) {
  sym_names = names(syms)
  n_atoms = min(sample(1:max_atoms, 1), length(sym_names))
  chosen = sample(sym_names, n_atoms)
  atoms = lapply(chosen, function(s) {
    dom = universe[[s]]
    n_vals = sample(1:max(1, length(dom) - 1), 1)
    syms[[s]] %among% sample(dom, n_vals)
  })
  as.CnfClause(Reduce(`|`, atoms))
}

set.seed(31415)

# Strategy 1: 6 binary variables (64 assignments)
cat("=== Strategy 1: 6 binary variables ===\n")
for (trial in 1:500) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:6) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }
  n_clauses = sample(3:12, 1)
  clauses = lapply(1:n_clauses, function(j) make_clause(syms, u, 5))
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [S1 trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("S1-%d", trial))
}
cat(sprintf("  S1 done: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 2: 3 variables with domain 4-5
cat("\n=== Strategy 2: 3 variables with large domains ===\n")
for (trial in 1:500) {
  u = CnfUniverse()
  ds = sample(3:5, 3, replace = TRUE)
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:ds[v]))
  }
  n_clauses = sample(3:10, 1)
  clauses = lapply(1:n_clauses, function(j) make_clause(syms, u, 3))
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [S2 trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("S2-%d", trial))
}
cat(sprintf("  S2 done: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 3: OR and AND combinations
cat("\n=== Strategy 3: OR and AND combinations ===\n")
for (trial in 1:300) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2"))
  C = CnfSymbol(u, "C", c("c1", "c2", "c3"))
  syms = list(A = A, B = B, C = C)

  # Build two sub-formulas and combine with | and &
  f1_clauses = lapply(1:sample(1:3, 1), function(j) make_clause(syms, u, 2))
  f2_clauses = lapply(1:sample(1:3, 1), function(j) make_clause(syms, u, 2))

  f1 = tryCatch(CnfFormula(f1_clauses), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(f2_clauses), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  # Test &
  f_and = tryCatch(f1 & f2, error = function(e) NULL)
  if (!is.null(f_and)) {
    n_tests <<- n_tests + 1
    t1 = evaluate_formula(f1, u)
    t2 = evaluate_formula(f2, u)
    t_and = evaluate_formula(f_and, u)
    if (!all(t_and == (t1 & t2))) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [S3-and trial %d]\n", trial))
    }
  }

  # Test |
  f_or = tryCatch(f1 | f2, error = function(e) NULL)
  if (!is.null(f_or)) {
    n_tests <<- n_tests + 1
    t1 = evaluate_formula(f1, u)
    t2 = evaluate_formula(f2, u)
    t_or = evaluate_formula(f_or, u)
    if (!all(t_or == (t1 | t2))) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [S3-or trial %d]\n", trial))
    }
  }

  # Test !
  neg_f1 = tryCatch(!f1, error = function(e) NULL)
  if (!is.null(neg_f1)) {
    n_tests <<- n_tests + 1
    t1 = evaluate_formula(f1, u)
    t_neg = evaluate_formula(neg_f1, u)
    if (!all(t_neg == !t1)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [S3-neg trial %d]\n", trial))
    }
  }
}
cat(sprintf("  S3 done: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 4: Many single-symbol clauses (unit-heavy)
cat("\n=== Strategy 4: Unit-heavy formulas ===\n")
for (trial in 1:500) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:sample(2:4, 1)))
  }
  # Create mix of units and multi-symbol clauses
  n_clauses = sample(4:10, 1)
  clauses = lapply(1:n_clauses, function(j) {
    if (runif(1) < 0.4) {
      # Unit clause
      s = sample(names(syms), 1)
      dom = u[[s]]
      as.CnfClause(syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1)))
    } else {
      make_clause(syms, u, 3)
    }
  })
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [S4 trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("S4-%d", trial))
}
cat(sprintf("  S4 done: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 5: Clauses that share many symbols (maximizes SSE/HLA opportunities)
cat("\n=== Strategy 5: High-overlap clauses ===\n")
for (trial in 1:500) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("x1", "x2", "x3"))
  Y = CnfSymbol(u, "Y", c("y1", "y2", "y3"))
  syms = list(X = X, Y = Y)

  # All clauses mention BOTH symbols
  n_clauses = sample(3:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    as.CnfClause(
      X %among% sample(c("x1", "x2", "x3"), sample(1:2, 1)) |
      Y %among% sample(c("y1", "y2", "y3"), sample(1:2, 1))
    )
  })
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [S5 trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("S5-%d", trial))
}
cat(sprintf("  S5 done: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 6: Contradictory formulas (should simplify to FALSE)
cat("\n=== Strategy 6: Contradiction-prone formulas ===\n")
for (trial in 1:300) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b"))
  Y = CnfSymbol(u, "Y", c("c", "d"))
  syms = list(X = X, Y = Y)

  # Create formulas that are likely contradictory
  clauses = list(
    as.CnfClause(X %among% sample(c("a", "b"), 1)),
    as.CnfClause(X %among% sample(c("a", "b"), 1)),
    as.CnfClause(Y %among% sample(c("c", "d"), 1)),
    as.CnfClause(Y %among% sample(c("c", "d"), 1))
  )
  # Add some multi-symbol clauses
  for (i in 1:sample(0:3, 1)) {
    clauses[[length(clauses) + 1]] = make_clause(syms, u, 2)
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [S6 trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("S6-%d", trial))
}
cat(sprintf("  S6 done: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== MEGA TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
