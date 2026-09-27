#!/usr/bin/env Rscript
# Regression-style tests: specific formulas that exercise exact code paths.
# Each test is designed to trigger a specific internal condition.
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

# === Test 1: Formula that simplifies to exactly one unit ===
cat("=== Simplifies to single unit ===\n")
u = CnfUniverse()
dom = c("a", "b", "c")
A = CnfSymbol(u, "A", dom)
B = CnfSymbol(u, "B", dom)

# A %among% a is the unit, and the other clause is subsumed by it
clauses = list(
  as.CnfClause(A %among% "a"),
  as.CnfClause(A %among% c("a", "b") | B %among% "c")
)
f = CnfFormula(clauses)
check_fc(f, clauses, u, "single-unit")
cat(sprintf("  Single unit: %d tests, %d failures\n", n_tests, n_failures))

# === Test 2: Formula with all clauses identical ===
cat("\n=== All identical clauses ===\n")
set.seed(97001)
for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  v1 = sample(dom, sample(1:2, 1))
  v2 = sample(dom, sample(1:2, 1))
  cl = as.CnfClause(A %among% v1 | B %among% v2)
  clauses = replicate(sample(3:8, 1), cl, simplify = FALSE)

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [dup-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("dup-%d", trial))
}
cat(sprintf("  Identical clauses: %d tests, %d failures\n", n_tests, n_failures))

# === Test 3: Single variable, many clauses (effectively intersecting ranges) ===
cat("\n=== Single variable intersection chain ===\n")
set.seed(97002)
for (trial in 1:200) {
  u = CnfUniverse()
  dom = paste0("v", 1:6)
  A = CnfSymbol(u, "A", dom)

  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    as.CnfClause(A %among% sample(dom, sample(2:5, 1)))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [1var-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("1var-%d", trial))
}
cat(sprintf("  Single var chain: %d tests, %d failures\n", n_tests, n_failures))

# === Test 4: Two variables, one has domain 1 (always tautological in clauses) ===
cat("\n=== One variable with domain 1 ===\n")
set.seed(97003)
for (trial in 1:200) {
  u = CnfUniverse()
  S = CnfSymbol(u, "S", c("only"))
  A = CnfSymbol(u, "A", c("a", "b", "c"))

  clauses = lapply(1:sample(2:4, 1), function(j) {
    if (runif(1) < 0.5) {
      as.CnfClause(S %among% "only" | A %among% sample(c("a", "b", "c"), sample(1:2, 1)))
    } else {
      as.CnfClause(A %among% sample(c("a", "b", "c"), sample(1:2, 1)))
    }
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [dom1-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("dom1-%d", trial))
}
cat(sprintf("  Domain 1: %d tests, %d failures\n", n_tests, n_failures))

# === Test 5: Exactly 2 clauses, every possible combination for 2 vars dom 4 ===
cat("\n=== Exhaustive 2-clause 2-var dom 4 ===\n")
u = CnfUniverse()
dom = c("a", "b", "c", "d")
A = CnfSymbol(u, "A", dom)
B = CnfSymbol(u, "B", dom)

# Generate all possible clauses: for each variable, subset of domain (1-3 values)
# Plus: A only, B only, or both
all_clauses = list()
for (a_subset in 1:15) {  # 2^4 - 1 = 15 non-empty subsets of domain
  a_vals = dom[as.logical(intToBits(a_subset)[1:4])]
  if (length(a_vals) == 4) next  # skip full domain (tautological for A)
  if (length(a_vals) == 0) next
  # A-only clause
  cl = structure(list(A = a_vals), universe = u, class = "CnfClause")
  all_clauses[[length(all_clauses) + 1]] = cl
}
for (b_subset in 1:15) {
  b_vals = dom[as.logical(intToBits(b_subset)[1:4])]
  if (length(b_vals) == 4) next
  if (length(b_vals) == 0) next
  cl = structure(list(B = b_vals), universe = u, class = "CnfClause")
  all_clauses[[length(all_clauses) + 1]] = cl
}
for (a_subset in 1:15) {
  a_vals = dom[as.logical(intToBits(a_subset)[1:4])]
  if (length(a_vals) == 4 || length(a_vals) == 0) next
  for (b_subset in 1:15) {
    b_vals = dom[as.logical(intToBits(b_subset)[1:4])]
    if (length(b_vals) == 4 || length(b_vals) == 0) next
    cl = structure(list(A = a_vals, B = b_vals), universe = u, class = "CnfClause")
    all_clauses[[length(all_clauses) + 1]] = cl
  }
}
n_clauses = length(all_clauses)
cat(sprintf("  Generated %d clauses\n", n_clauses))

# Test all 2-clause combinations (upper triangle)
for (i in 1:min(n_clauses, 50)) {
  for (j in i:min(n_clauses, 50)) {
    clauses = list(all_clauses[[i]], all_clauses[[j]])
    f = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(f, "error")) {
      n_tests = n_tests + 1; n_failures = n_failures + 1; next
    }
    check_fc(f, clauses, u, sprintf("2v4-%d-%d", i, j))
  }
}
cat(sprintf("  Exhaustive 2-clause: %d tests, %d failures\n", n_tests, n_failures))

# === Test 6: Formula from list of CnfFormulas (constructor path) ===
cat("\n=== CnfFormula from CnfFormula list ===\n")
set.seed(97004)
for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create 2-3 formulas, then combine via CnfFormula(list(f1, f2, f3))
  formulas = lapply(1:sample(2:3, 1), function(j) {
    clauses = lapply(1:sample(1:2, 1), function(k) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  })
  if (any(sapply(formulas, is.null))) next

  n_tests = n_tests + 1
  combined = tryCatch(CnfFormula(formulas), error = function(e) e)
  if (inherits(combined, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [from-formula-%d]: %s\n", trial, combined$message)); next
  }

  # Should be equivalent to Reduce(&, formulas)
  and_combined = tryCatch(Reduce(`&`, formulas), error = function(e) NULL)
  if (!is.null(and_combined)) {
    t1 = evaluate_formula(combined, u)
    t2 = evaluate_formula(and_combined, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [from-formula-%d]: CnfFormula(list) != Reduce(&)\n", trial))
    }
  }
}
cat(sprintf("  From formula list: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
