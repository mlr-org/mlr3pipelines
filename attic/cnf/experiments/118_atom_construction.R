#!/usr/bin/env Rscript
# Test CnfAtom and %among% edge cases:
# - Full domain -> TRUE atom
# - Empty domain -> FALSE atom
# - Single value
# - Values not in domain (should they be silently filtered?)
# - Duplicate values
# - Atom operations: !, |, &
# - Atom used in formula construction
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Full domain atom -> TRUE ===
cat("=== Full domain atom ===\n")

u = CnfUniverse()
dom = c("a", "b", "c")
A = CnfSymbol(u, "A", dom)

n_tests = n_tests + 1
full_atom = A %among% dom
if (!isTRUE(unclass(full_atom))) {
  n_failures = n_failures + 1
  cat("FAIL: full domain atom should be TRUE\n")
}

# Full domain in different order
n_tests = n_tests + 1
full_atom2 = A %among% c("c", "a", "b")
if (!isTRUE(unclass(full_atom2))) {
  n_failures = n_failures + 1
  cat("FAIL: full domain (reordered) should be TRUE\n")
}

cat(sprintf("  Full domain: %d tests, %d failures\n", n_tests, n_failures))

# === Empty domain atom -> FALSE ===
cat("\n=== Empty domain atom ===\n")

n_tests = n_tests + 1
empty_atom = A %among% character(0)
if (!isFALSE(unclass(empty_atom))) {
  n_failures = n_failures + 1
  cat("FAIL: empty domain atom should be FALSE\n")
}

cat(sprintf("  Empty domain: %d tests, %d failures\n", n_tests, n_failures))

# === Single value ===
cat("\n=== Single value atom ===\n")

for (v in dom) {
  n_tests = n_tests + 1
  atom = A %among% v
  if (isTRUE(unclass(atom)) || isFALSE(unclass(atom))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL: A %%among%% '%s' should be a proper atom\n", v))
    next
  }
  if (atom$symbol != "A" || !identical(atom$values, v)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL: A %%among%% '%s' has wrong structure\n", v))
  }
}

cat(sprintf("  Single value: %d tests, %d failures\n", n_tests, n_failures))

# === Values not in domain ===
cat("\n=== Values not in domain ===\n")

# %among% with values not in domain - should silently filter
n_tests = n_tests + 1
atom_bad = tryCatch(A %among% c("a", "z"), error = function(e) e)
if (inherits(atom_bad, "error")) {
  # It's valid if it errors - depends on implementation
  cat(sprintf("  Note: A %%among%% c('a','z') errors: %s\n", atom_bad$message))
} else {
  # If it doesn't error, check that it's equivalent to A %among% "a"
  if (!isTRUE(unclass(atom_bad)) && !isFALSE(unclass(atom_bad))) {
    if (!identical(sort(atom_bad$values), "a")) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL: A %%among%% c('a','z') should filter to just 'a', got %s\n", paste(atom_bad$values, collapse = ",")))
    }
  }
}

cat(sprintf("  Values not in domain: %d tests, %d failures\n", n_tests, n_failures))

# === Duplicate values ===
cat("\n=== Duplicate values ===\n")

n_tests = n_tests + 1
atom_dup = tryCatch(A %among% c("a", "a", "b"), error = function(e) e)
if (inherits(atom_dup, "error")) {
  cat(sprintf("  Note: dup values errors: %s\n", atom_dup$message))
} else {
  # Should be equivalent to A %among% c("a", "b")
  if (!identical(sort(atom_dup$values), c("a", "b"))) {
    n_failures = n_failures + 1
    cat("FAIL: duplicate values not properly handled\n")
  }
}

cat(sprintf("  Duplicate values: %d tests, %d failures\n", n_tests, n_failures))

# === Atom negation with various domain sizes ===
cat("\n=== Atom negation ===\n")
set.seed(118001)

for (trial in 1:300) {
  u2 = CnfUniverse()
  dom_size = sample(2:6, 1)
  d = paste0("v", 1:dom_size)
  X = CnfSymbol(u2, "X", d)

  n_vals = sample(1:(dom_size - 1), 1)
  vals = sample(d, n_vals)
  atom = X %among% vals

  n_tests = n_tests + 1
  neg = tryCatch(!atom, error = function(e) e)
  if (inherits(neg, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neg-%d]: %s\n", trial, neg$message)); next
  }

  # Check: negation values should be complement
  expected_neg = setdiff(d, vals)
  if (isTRUE(unclass(neg))) {
    if (length(expected_neg) != length(d)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [neg-%d]: negation is TRUE but shouldn't be\n", trial))
    }
  } else if (isFALSE(unclass(neg))) {
    if (length(expected_neg) != 0) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [neg-%d]: negation is FALSE but shouldn't be\n", trial))
    }
  } else {
    if (!identical(sort(neg$values), sort(expected_neg))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [neg-%d]: wrong complement\n", trial))
    }
  }
}
cat(sprintf("  Atom negation: %d tests, %d failures\n", n_tests, n_failures))

# === Atom operations in formula context ===
cat("\n=== Atom operations in formulas ===\n")
set.seed(118002)

for (trial in 1:300) {
  u3 = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u3, "A", dom)
  B = CnfSymbol(u3, "B", dom)

  a_vals = sample(dom, sample(1:2, 1))
  b_vals = sample(dom, sample(1:2, 1))

  atom_a = A %among% a_vals
  atom_b = B %among% b_vals

  # atom & atom -> formula
  n_tests = n_tests + 1
  f1 = tryCatch(atom_a & atom_b, error = function(e) e)
  if (inherits(f1, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [and-%d]: %s\n", trial, f1$message)); next
  }
  if (!inherits(f1, "CnfFormula")) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [and-%d]: atom & atom should be CnfFormula\n", trial))
    next
  }

  varnames = ls(u3)
  domains = lapply(varnames, function(v) get(v, u3))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  expected = (assignments[["A"]] %in% a_vals) & (assignments[["B"]] %in% b_vals)
  truth = evaluate_formula(f1, u3)
  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [and-%d]: atom & atom semantic error\n", trial))
  }

  # !atom in formula context
  n_tests = n_tests + 1
  neg_a = !atom_a
  f2 = tryCatch(neg_a & atom_b, error = function(e) e)
  if (inherits(f2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neg-and-%d]: %s\n", trial, f2$message)); next
  }
  expected2 = !(assignments[["A"]] %in% a_vals) & (assignments[["B"]] %in% b_vals)
  truth2 = evaluate_formula(f2, u3)
  if (!all(truth2 == expected2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [neg-and-%d]: !atom & atom semantic error\n", trial))
  }
}
cat(sprintf("  Atom ops in formulas: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
