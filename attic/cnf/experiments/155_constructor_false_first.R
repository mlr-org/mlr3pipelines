#!/usr/bin/env Rscript
# Test constructor edge cases specifically around FALSE/TRUE first elements:
# - CnfClause() with FALSE atom first (Bug #3 pattern)
# - CnfFormula() with various orderings of TRUE/FALSE/real elements
# - Conversion methods with edge cases
# - as.CnfFormula.CnfClause on TRUE/FALSE clauses
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === CnfClause with FALSE atom first ===
cat("=== CnfClause: FALSE atom first ===\n")
set.seed(155001)

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a", "b", "c"))
B = CnfSymbol(u, "B", c("d", "e", "f"))

# CnfClause(list(FALSE_atom_with_universe, real_atom))
# FALSE atom created via %among% character(0) HAS a universe
n_tests = n_tests + 1
result = tryCatch({
  false_atom = A %among% character(0)
  real_atom = B %among% c("d", "e")
  CnfClause(list(false_atom, real_atom))
}, error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: CnfClause(FALSE_with_univ, real): %s\n", result$message))
} else {
  # Should be B %among% c("d","e") since FALSE | X = X
  if (isFALSE(unclass(result)) || isTRUE(unclass(result))) {
    cat(sprintf("  Unexpected logical result for FALSE_atom | real_atom\n"))
  }
}

# CnfClause(list(bare_FALSE_atom, real_atom)) - Bug #3 pattern
n_tests = n_tests + 1
result = tryCatch({
  bare_false = as.CnfAtom(FALSE)
  real_atom = B %among% c("d", "e")
  CnfClause(list(bare_false, real_atom))
}, error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("BUG3-CLAUSE: CnfClause(bare_FALSE, real): %s\n", result$message))
}

# CnfClause(list(bare_TRUE_atom, real_atom))
n_tests = n_tests + 1
result = tryCatch({
  bare_true = as.CnfAtom(TRUE)
  real_atom = B %among% c("d", "e")
  CnfClause(list(bare_true, real_atom))
}, error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: CnfClause(bare_TRUE, real): %s\n", result$message))
} else {
  if (!isTRUE(unclass(result))) {
    n_failures = n_failures + 1
    cat("FAIL: TRUE | real should be TRUE\n")
  }
}

# CnfClause(list(FALSE_with_universe, FALSE_with_universe, real))
n_tests = n_tests + 1
result = tryCatch({
  CnfClause(list(A %among% character(0), B %among% character(0), A %among% "a"))
}, error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: CnfClause(FALSE, FALSE, real): %s\n", result$message))
}

cat(sprintf("  CnfClause FALSE first: %d tests, %d failures\n", n_tests, n_failures))

# === CnfFormula with various TRUE/FALSE orderings ===
cat("\n=== CnfFormula: TRUE/FALSE ordering ===\n")
set.seed(155002)

# TRUE clause first (Bug #3)
n_tests = n_tests + 1
result = tryCatch({
  true_clause = as.CnfClause(TRUE)
  real_clause = as.CnfClause(A %among% "a" | B %among% "d")
  CnfFormula(list(true_clause, real_clause))
}, error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("BUG3-FORMULA: CnfFormula(bare_TRUE_clause, real): %s\n", result$message))
}

# FALSE clause first
n_tests = n_tests + 1
result = tryCatch({
  false_clause = as.CnfClause(FALSE)
  real_clause = as.CnfClause(A %among% "a" | B %among% "d")
  CnfFormula(list(false_clause, real_clause))
}, error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: CnfFormula(bare_FALSE, real): %s\n", result$message))
} else {
  if (!isFALSE(unclass(result))) {
    n_failures = n_failures + 1
    cat("FAIL: FALSE & real should be FALSE\n")
  }
}

# FALSE CnfFormula in list (Bug #4 related)
n_tests = n_tests + 1
result = tryCatch({
  f1 = A %among% "a" & B %among% "d"
  f_false = as.CnfFormula(FALSE)
  CnfFormula(list(f1, f_false))
}, error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("BUG4: CnfFormula(real_formula, FALSE_formula): %s\n", result$message))
}

# Multiple TRUE/FALSE formulas mixed
n_tests = n_tests + 1
result = tryCatch({
  f1 = A %among% "a" & B %among% "d"
  CnfFormula(list(as.CnfFormula(TRUE), f1))
}, error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: CnfFormula(TRUE_formula, real): %s\n", result$message))
}

cat(sprintf("  CnfFormula ordering: %d tests, %d failures\n", n_tests, n_failures))

# === Conversion method stress ===
cat("\n=== Conversion methods ===\n")
set.seed(155003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  # Create a random clause
  n_sym = sample(1:2, 1)
  chosen = sample(names(syms), n_sym)
  atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  clause = as.CnfClause(Reduce(`|`, atoms))

  if (isTRUE(unclass(clause)) || isFALSE(unclass(clause))) next

  # Test: as.CnfFormula(clause) should be same as CnfFormula(list(clause))
  n_tests = n_tests + 1
  f1 = tryCatch(as.CnfFormula(clause), error = function(e) e)
  f2 = tryCatch(CnfFormula(list(clause)), error = function(e) e)
  if (inherits(f1, "error") || inherits(f2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [conv-%d]\n", trial)); next
  }
  eq = tryCatch(all.equal(f1, f2), error = function(e) "error")
  if (!isTRUE(eq)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [conv-%d]: as.CnfFormula(clause) != CnfFormula(list(clause))\n", trial))
  }

  # Test: as.list then reconstruct
  n_tests = n_tests + 1
  clauses_list = as.list(f1)
  f3 = tryCatch(CnfFormula(clauses_list), error = function(e) e)
  if (inherits(f3, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [roundtrip-%d]\n", trial)); next
  }

  # Test: format and print don't crash
  n_tests = n_tests + 1
  tryCatch({
    format(f1)
    format(clause)
    for (a in atoms) format(a)
    capture.output(print(f1))
    capture.output(print(clause))
    for (a in atoms) capture.output(print(a))
  }, error = function(e) {
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR [format-%d]: %s\n", trial, e$message))
  })
}

cat(sprintf("  Conversion methods: %d tests, %d failures\n", n_tests, n_failures))

# === as.logical on various objects ===
cat("\n=== as.logical edge cases ===\n")

n_tests = n_tests + 1
# as.logical on TRUE/FALSE formula
if (!isTRUE(as.logical(as.CnfFormula(TRUE)))) {
  n_failures = n_failures + 1; cat("FAIL: as.logical(TRUE_formula) != TRUE\n")
}
n_tests = n_tests + 1
if (!identical(as.logical(as.CnfFormula(FALSE)), FALSE)) {
  n_failures = n_failures + 1; cat("FAIL: as.logical(FALSE_formula) != FALSE\n")
}
n_tests = n_tests + 1
# as.logical on normal formula should be NA
f = A %among% "a" & B %among% "d"
if (!is.na(as.logical(f))) {
  n_failures = n_failures + 1; cat("FAIL: as.logical(normal_formula) != NA\n")
}

# as.logical on TRUE/FALSE clause
n_tests = n_tests + 1
if (!isTRUE(as.logical(as.CnfClause(TRUE)))) {
  n_failures = n_failures + 1; cat("FAIL: as.logical(TRUE_clause) != TRUE\n")
}
n_tests = n_tests + 1
if (!identical(as.logical(as.CnfClause(FALSE)), FALSE)) {
  n_failures = n_failures + 1; cat("FAIL: as.logical(FALSE_clause) != FALSE\n")
}

# as.logical on TRUE/FALSE atom
n_tests = n_tests + 1
if (!isTRUE(as.logical(as.CnfAtom(TRUE)))) {
  n_failures = n_failures + 1; cat("FAIL: as.logical(TRUE_atom) != TRUE\n")
}
n_tests = n_tests + 1
if (!identical(as.logical(as.CnfAtom(FALSE)), FALSE)) {
  n_failures = n_failures + 1; cat("FAIL: as.logical(FALSE_atom) != FALSE\n")
}

cat(sprintf("  as.logical: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
