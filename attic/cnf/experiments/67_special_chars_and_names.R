#!/usr/bin/env Rscript
# Test symbols and values with special characters, spaces, dots, etc.
# These could trip up name-based indexing, %in%, match(), etc.
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

# === Symbols with dots in names ===
cat("=== Dotted symbol names ===\n")
set.seed(67001)

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "a.b", c("x.1", "x.2", "x.3"))
  B = CnfSymbol(u, "c.d", c("y.1", "y.2"))

  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    atoms = list()
    if (sample(c(TRUE, FALSE), 1)) {
      atoms[[length(atoms) + 1]] = A %among% sample(c("x.1", "x.2", "x.3"), sample(1:2, 1))
    }
    if (sample(c(TRUE, FALSE), 1) || !length(atoms)) {
      atoms[[length(atoms) + 1]] = B %among% sample(c("y.1", "y.2"), 1)
    }
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [dotted-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("dotted-%d", trial))
}
cat(sprintf("  Dotted names: %d tests, %d failures\n", n_tests, n_failures))

# === Symbols with spaces ===
cat("\n=== Spaced symbol names ===\n")
set.seed(67002)

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "my var", c("val 1", "val 2", "val 3"))
  B = CnfSymbol(u, "other var", c("opt a", "opt b"))

  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    atoms = list()
    if (sample(c(TRUE, FALSE), 1)) {
      atoms[[length(atoms) + 1]] = A %among% sample(c("val 1", "val 2", "val 3"), sample(1:2, 1))
    }
    if (sample(c(TRUE, FALSE), 1) || !length(atoms)) {
      atoms[[length(atoms) + 1]] = B %among% sample(c("opt a", "opt b"), 1)
    }
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [spaced-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("spaced-%d", trial))
}
cat(sprintf("  Spaced names: %d tests, %d failures\n", n_tests, n_failures))

# === Values that are substrings of each other ===
cat("\n=== Substring-like values ===\n")
set.seed(67003)

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a", "ab", "abc", "abcd"))
  B = CnfSymbol(u, "B", c("x", "xy", "xyz"))

  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    atoms = list()
    if (sample(c(TRUE, FALSE), 1)) {
      atoms[[length(atoms) + 1]] = A %among% sample(c("a", "ab", "abc", "abcd"), sample(1:3, 1))
    }
    if (sample(c(TRUE, FALSE), 1) || !length(atoms)) {
      atoms[[length(atoms) + 1]] = B %among% sample(c("x", "xy", "xyz"), sample(1:2, 1))
    }
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [substr-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("substr-%d", trial))
}
cat(sprintf("  Substring values: %d tests, %d failures\n", n_tests, n_failures))

# === Empty string values ===
cat("\n=== Empty string in domain ===\n")
set.seed(67004)

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("", "a", "b"))
  B = CnfSymbol(u, "B", c("", "x"))

  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    atoms = list()
    if (sample(c(TRUE, FALSE), 1)) {
      atoms[[length(atoms) + 1]] = A %among% sample(c("", "a", "b"), sample(1:2, 1))
    }
    if (sample(c(TRUE, FALSE), 1) || !length(atoms)) {
      atoms[[length(atoms) + 1]] = B %among% sample(c("", "x"), 1)
    }
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [empty-str-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("empty-str-%d", trial))
}
cat(sprintf("  Empty string: %d tests, %d failures\n", n_tests, n_failures))

# === Numeric-like string values ===
cat("\n=== Numeric-like string values ===\n")
set.seed(67005)

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("1", "2", "10", "11"))
  B = CnfSymbol(u, "B", c("TRUE", "FALSE", "NA"))

  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    atoms = list()
    if (sample(c(TRUE, FALSE), 1)) {
      atoms[[length(atoms) + 1]] = A %among% sample(c("1", "2", "10", "11"), sample(1:3, 1))
    }
    if (sample(c(TRUE, FALSE), 1) || !length(atoms)) {
      atoms[[length(atoms) + 1]] = B %among% sample(c("TRUE", "FALSE", "NA"), sample(1:2, 1))
    }
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [numlike-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("numlike-%d", trial))
}
cat(sprintf("  Numeric-like: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
