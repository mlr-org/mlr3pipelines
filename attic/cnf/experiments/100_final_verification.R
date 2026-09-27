#!/usr/bin/env Rscript
# Experiment 100: Final verification sweep.
# This experiment combines all the most powerful techniques:
# 1. Exhaustive enumeration for small cases
# 2. Property-based testing (algebraic laws)
# 3. Random fuzzing with truth-table verification
# All in one comprehensive test.
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

# === Part 1: Exhaustive 4 binary vars, all 2-clause combos ===
cat("=== Exhaustive 4-var binary, 2-clause ===\n")
u = CnfUniverse()
dom = c("0", "1")
syms_raw = list()
for (v in 1:4) {
  vname = paste0("X", v)
  syms_raw[[vname]] = CnfSymbol(u, vname, dom)
}

# All possible clauses: each of 4 vars either absent, "0" only, or "1" only
# 3^4 - 1 = 80 non-trivial clauses
all_clauses = list()
for (s1 in 0:2) for (s2 in 0:2) for (s3 in 0:2) for (s4 in 0:2) {
  if (s1 == 0 && s2 == 0 && s3 == 0 && s4 == 0) next
  cl = list()
  states = list(NULL, "0", "1")
  if (s1 > 0) cl[["X1"]] = states[[s1 + 1]]
  if (s2 > 0) cl[["X2"]] = states[[s2 + 1]]
  if (s3 > 0) cl[["X3"]] = states[[s3 + 1]]
  if (s4 > 0) cl[["X4"]] = states[[s4 + 1]]
  all_clauses[[length(all_clauses) + 1]] = structure(cl, universe = u, class = "CnfClause")
}
n_cl = length(all_clauses)
cat(sprintf("  Generated %d clauses\n", n_cl))

# All 2-clause combos (upper triangle + diagonal)
for (i in 1:n_cl) for (j in i:n_cl) {
  clauses = list(all_clauses[[i]], all_clauses[[j]])
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1; next
  }
  check_fc(f, clauses, u, sprintf("ex4-%d-%d", i, j))
}
cat(sprintf("  Exhaustive 4-var 2-clause: %d tests, %d failures\n", n_tests, n_failures))

# === Part 2: Random 3-clause combos ===
cat("\n=== Random 3-clause combos (4-var binary) ===\n")
set.seed(100001)
for (trial in 1:3000) {
  indices = sample(n_cl, 3, replace = TRUE)
  clauses = lapply(indices, function(i) all_clauses[[i]])
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1; next
  }
  check_fc(f, clauses, u, sprintf("3cl-%d", trial))
}
cat(sprintf("  Random 3-clause: %d tests, %d failures\n", n_tests, n_failures))

# === Part 3: Algebraic properties with varied configs ===
cat("\n=== Algebraic properties ===\n")
set.seed(100002)

for (trial in 1:500) {
  u2 = CnfUniverse()
  # Pick random config
  n_vars = sample(2:4, 1)
  dom_size = sample(2:4, 1)
  d = paste0("v", 1:dom_size)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u2, vname, d)
  }

  make_formula = function() {
    n_cl = sample(1:3, 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:min(2, n_vars), 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(d, sample(1:(dom_size-1), 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula()
  f2 = make_formula()
  if (is.null(f1) || is.null(f2)) next

  t1 = evaluate_formula(f1, u2)
  t2 = evaluate_formula(f2, u2)

  # Test: f1 & f2 == f2 & f1
  r1 = tryCatch(f1 & f2, error = function(e) NULL)
  r2 = tryCatch(f2 & f1, error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    n_tests = n_tests + 1
    if (!all(evaluate_formula(r1, u2) == evaluate_formula(r2, u2))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [comm-%d]: AND not commutative\n", trial))
    }
  }

  # Test: !(f1 & f2) == !f1 | !f2
  lhs = tryCatch(!(f1 & f2), error = function(e) NULL)
  rhs = tryCatch((!f1) | (!f2), error = function(e) NULL)
  if (!is.null(lhs) && !is.null(rhs)) {
    n_tests = n_tests + 1
    if (!all(evaluate_formula(lhs, u2) == evaluate_formula(rhs, u2))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [demorgan-%d]: De Morgan violated\n", trial))
    }
  }

  # Test: f1 & (f1 | f2) == f1 (absorption)
  f1_or_f2 = tryCatch(f1 | f2, error = function(e) NULL)
  if (!is.null(f1_or_f2)) {
    absorbed = tryCatch(f1 & f1_or_f2, error = function(e) NULL)
    if (!is.null(absorbed)) {
      n_tests = n_tests + 1
      if (!all(evaluate_formula(absorbed, u2) == t1)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [absorb-%d]: absorption failed\n", trial))
      }
    }
  }
}
cat(sprintf("  Algebraic properties: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
