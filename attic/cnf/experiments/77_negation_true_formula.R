#!/usr/bin/env Rscript
# Test !.CnfFormula on TRUE formula.
# TRUE formula is internally list() which is NOT logical,
# so it doesn't hit the is.logical shortcut.
# Then lapply(list(), ...) = list(), and Reduce(f, list()) may error.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== !.CnfFormula on TRUE formula ===\n")

# Test 1: !TRUE formula
cat("Test 1: !(as.CnfFormula(TRUE))\n")
n_tests = n_tests + 1
f_true = as.CnfFormula(TRUE)
cat(sprintf("  is.logical(f_true): %s\n", is.logical(f_true)))
cat(sprintf("  is.logical(unclass(f_true)): %s\n", is.logical(unclass(f_true))))
cat(sprintf("  unclass(f_true) class: %s\n", class(unclass(f_true))))

r = tryCatch(!f_true, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR: %s\n", r$message))
  cat(sprintf("  Error call: %s\n", deparse(r$call)))
} else {
  cat(sprintf("  Result: %s, as.logical = %s\n", class(r), as.logical(r)))
  if (!isFALSE(as.logical(r))) {
    n_failures = n_failures + 1
    cat("  FAIL: !TRUE should be FALSE\n")
  }
}

# Test 2: Check the Reduce path
cat("\nTest 2: Reduce behavior with empty list\n")
n_tests = n_tests + 1
r = tryCatch(Reduce(function(x, y) x | y, list()), error = function(e) e)
cat(sprintf("  Reduce(f, list()): %s\n",
  if (inherits(r, "error")) r$message else "no error"))

# Test 3: !FALSE formula
cat("\nTest 3: !(as.CnfFormula(FALSE))\n")
n_tests = n_tests + 1
f_false = as.CnfFormula(FALSE)
r = tryCatch(!f_false, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR: %s\n", r$message))
} else {
  cat(sprintf("  Result as.logical = %s\n", as.logical(r)))
  if (!isTRUE(as.logical(r))) {
    n_failures = n_failures + 1
    cat("  FAIL: !FALSE should be TRUE\n")
  }
}

# Test 4: !TRUE formula constructed from tautological clause
cat("\nTest 4: !(formula from tautological clause)\n")
n_tests = n_tests + 1
u = CnfUniverse()
A = CnfSymbol(u, "A", c("a", "b", "c"))
f_taut = CnfFormula(list(as.CnfClause(A %among% c("a", "b", "c"))))
cat(sprintf("  f_taut as.logical: %s\n", as.logical(f_taut)))
cat(sprintf("  is.logical(f_taut): %s\n", is.logical(f_taut)))
cat(sprintf("  unclass type: %s, length: %d\n", class(unclass(f_taut)), length(unclass(f_taut))))

r = tryCatch(!f_taut, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR: %s\n", r$message))
} else {
  cat(sprintf("  Result as.logical = %s\n", as.logical(r)))
}

# Test 5: Chain: !(!normal) where !normal is multi-clause
cat("\nTest 5: !!normal formula\n")
n_tests = n_tests + 1
f_norm = CnfFormula(list(
  as.CnfClause(A %among% c("a", "b")),
  as.CnfClause(A %among% c("b", "c"))
))
r = tryCatch(!!f_norm, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR: %s\n", r$message))
} else {
  t1 = evaluate_formula(f_norm, u)
  t2 = evaluate_formula(r, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat("  FAIL: !!f != f\n")
  } else {
    cat("  OK: !!f == f\n")
  }
}

# Test 6: What does is.logical do with CnfFormula objects?
cat("\nTest 6: is.logical behavior on CnfFormula objects\n")
cat(sprintf("  is.logical(as.CnfFormula(TRUE)): %s\n", is.logical(as.CnfFormula(TRUE))))
cat(sprintf("  is.logical(as.CnfFormula(FALSE)): %s\n", is.logical(as.CnfFormula(FALSE))))
cat(sprintf("  is.logical(f_norm): %s\n", is.logical(f_norm)))

# Test 7: What is the structure of TRUE formula?
cat("\nTest 7: TRUE formula structure\n")
f_t = as.CnfFormula(TRUE)
cat(sprintf("  class: %s\n", paste(class(f_t), collapse=", ")))
cat(sprintf("  typeof: %s\n", typeof(f_t)))
cat(sprintf("  is.list: %s\n", is.list(f_t)))
cat(sprintf("  length: %d\n", length(f_t)))
# The key: is.logical checks the underlying type
cat(sprintf("  is.logical: %s\n", is.logical(f_t)))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
