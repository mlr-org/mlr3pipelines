#!/usr/bin/env Rscript
# Test CnfClause subsetting ([) and other clause operations
# These exercise less-used code paths in CnfClause.R
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== CnfClause subsetting ===\n")
set.seed(71001)

# Test numeric subsetting
u = CnfUniverse()
A = CnfSymbol(u, "A", c("a", "b", "c"))
B = CnfSymbol(u, "B", c("x", "y"))
C = CnfSymbol(u, "C", c("1", "2", "3"))

cl = A %among% c("a", "b") | B %among% "x" | C %among% c("1", "2")

# Subset by name
n_tests = n_tests + 1
cl_A = cl["A"]
if (!isTRUE(all.equal(cl_A[["A"]], c("a", "b")))) {
  n_failures = n_failures + 1
  cat("FAIL: cl['A'] should give A values\n")
}

# Subset by numeric index
n_tests = n_tests + 1
cl_1 = cl[1]
if (length(cl_1) != 1) {
  n_failures = n_failures + 1
  cat("FAIL: cl[1] should have length 1\n")
}

# Subset to empty
n_tests = n_tests + 1
cl_empty = cl[integer(0)]
if (!isFALSE(unclass(cl_empty))) {
  n_failures = n_failures + 1
  cat("FAIL: cl[integer(0)] should be FALSE\n")
}

# Subset with 0
n_tests = n_tests + 1
cl_0 = cl[0]
if (!isFALSE(unclass(cl_0))) {
  n_failures = n_failures + 1
  cat("FAIL: cl[0] should be FALSE\n")
}

# Subset with logical
n_tests = n_tests + 1
cl_log = cl[c(TRUE, FALSE, TRUE)]
if (length(cl_log) != 2) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: cl[c(T,F,T)] should have length 2, got %d\n", length(cl_log)))
}

# Subset by multiple names
n_tests = n_tests + 1
cl_AB = cl[c("A", "B")]
if (length(cl_AB) != 2 || !all(names(cl_AB) == c("A", "B"))) {
  n_failures = n_failures + 1
  cat("FAIL: cl[c('A','B')] should have A and B\n")
}

cat(sprintf("  Basic subsetting: %d tests, %d failures\n", n_tests, n_failures))

# === CnfClause [.CnfClause with FALSE clause ===
cat("\n=== FALSE clause subsetting ===\n")
cl_false = as.CnfClause(FALSE)

# Subset FALSE clause with integer(0) - should work
n_tests = n_tests + 1
result = tryCatch(cl_false[integer(0)], error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: cl_false[integer(0)] error: %s\n", result$message))
} else if (!isFALSE(unclass(result))) {
  n_failures = n_failures + 1
  cat("FAIL: cl_false[integer(0)] should be FALSE\n")
}

# Subset FALSE clause with 0 - should work
n_tests = n_tests + 1
result = tryCatch(cl_false[0], error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: cl_false[0] error: %s\n", result$message))
}

# Subset FALSE clause with 1 - should error
n_tests = n_tests + 1
result = tryCatch(cl_false[1], error = function(e) "expected_error")
if (!identical(result, "expected_error")) {
  n_failures = n_failures + 1
  cat("FAIL: cl_false[1] should error\n")
}

cat(sprintf("  FALSE clause subsetting: %d tests, %d failures\n", n_tests, n_failures))

# === TRUE clause subsetting ===
cat("\n=== TRUE clause subsetting ===\n")
cl_true = as.CnfClause(TRUE)

# TRUE[1] should return TRUE
n_tests = n_tests + 1
result = tryCatch(cl_true[1], error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: cl_true[1] error: %s\n", result$message))
} else if (!isTRUE(unclass(result))) {
  n_failures = n_failures + 1
  cat("FAIL: cl_true[1] should be TRUE\n")
}

# TRUE[TRUE] should return TRUE
n_tests = n_tests + 1
result = tryCatch(cl_true[TRUE], error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: cl_true[TRUE] error: %s\n", result$message))
} else if (!isTRUE(unclass(result))) {
  n_failures = n_failures + 1
  cat("FAIL: cl_true[TRUE] should be TRUE\n")
}

# TRUE[0] should return FALSE
n_tests = n_tests + 1
result = tryCatch(cl_true[0], error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: cl_true[0] error: %s\n", result$message))
} else if (!isFALSE(unclass(result))) {
  n_failures = n_failures + 1
  cat("FAIL: cl_true[0] should be FALSE\n")
}

# TRUE[2] should error
n_tests = n_tests + 1
result = tryCatch(cl_true[2], error = function(e) "expected_error")
if (!identical(result, "expected_error")) {
  n_failures = n_failures + 1
  cat("FAIL: cl_true[2] should error\n")
}

cat(sprintf("  TRUE clause subsetting: %d tests, %d failures\n", n_tests, n_failures))

# === as.list.CnfClause ===
cat("\n=== as.list.CnfClause ===\n")

# Normal clause
cl_normal = A %among% c("a", "b") | B %among% "x"
n_tests = n_tests + 1
cl_list = as.list(cl_normal)
if (length(cl_list) != 2) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: as.list(normal) should have 2 atoms, got %d\n", length(cl_list)))
}

# FALSE clause
n_tests = n_tests + 1
cl_false_list = as.list(as.CnfClause(FALSE))
if (length(cl_false_list) != 0) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: as.list(FALSE) should be empty, got %d\n", length(cl_false_list)))
}

# TRUE clause - this is the known Bug #1
n_tests = n_tests + 1
result = tryCatch(as.list(as.CnfClause(TRUE)), error = function(e) "known_bug")
if (identical(result, "known_bug")) {
  cat("  Note: as.list(TRUE clause) crashes - known Bug #1\n")
} else {
  cat("  Note: as.list(TRUE clause) did NOT crash - Bug #1 may be fixed\n")
}

cat(sprintf("  as.list.CnfClause: %d tests, %d failures\n", n_tests, n_failures))

# === all.equal.CnfClause ===
cat("\n=== all.equal.CnfClause ===\n")

# Same clause, built differently
cl1 = A %among% c("a", "b") | B %among% "x"
cl2 = B %among% "x" | A %among% c("b", "a")

n_tests = n_tests + 1
if (!isTRUE(all.equal(cl1, cl2))) {
  n_failures = n_failures + 1
  cat("FAIL: reordered clause should be equal\n")
}

# Different clauses
cl3 = A %among% "a" | B %among% "x"
n_tests = n_tests + 1
if (isTRUE(all.equal(cl1, cl3))) {
  n_failures = n_failures + 1
  cat("FAIL: different clauses should not be equal\n")
}

# TRUE vs TRUE
n_tests = n_tests + 1
if (!isTRUE(all.equal(as.CnfClause(TRUE), as.CnfClause(TRUE)))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE == TRUE should hold\n")
}

# FALSE vs FALSE
n_tests = n_tests + 1
if (!isTRUE(all.equal(as.CnfClause(FALSE), as.CnfClause(FALSE)))) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE == FALSE should hold\n")
}

# TRUE vs FALSE
n_tests = n_tests + 1
if (isTRUE(all.equal(as.CnfClause(TRUE), as.CnfClause(FALSE)))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE != FALSE should hold\n")
}

cat(sprintf("  all.equal.CnfClause: %d tests, %d failures\n", n_tests, n_failures))

# === CnfClause construction edge cases ===
cat("\n=== CnfClause construction ===\n")

# Empty list -> FALSE
n_tests = n_tests + 1
result = CnfClause(list())
if (!isFALSE(unclass(result))) {
  n_failures = n_failures + 1
  cat("FAIL: CnfClause(list()) should be FALSE\n")
}

# All FALSE atoms
n_tests = n_tests + 1
false_atom = A %among% character(0)
result = CnfClause(list(false_atom))
if (!isFALSE(unclass(result))) {
  n_failures = n_failures + 1
  cat("FAIL: CnfClause with all FALSE atoms should be FALSE\n")
}

# Mix of FALSE and real atom
n_tests = n_tests + 1
result = CnfClause(list(false_atom, A %among% "a"))
if (isFALSE(unclass(result)) || isTRUE(unclass(result))) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE | A should give proper clause\n")
}

# Duplicate values in %among% should be handled
n_tests = n_tests + 1
atom_dup = A %among% c("a", "a", "b")
result = CnfClause(list(atom_dup))
vals = unclass(result)[["A"]]
if (length(vals) != length(unique(vals))) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: duplicate values not deduplicated, got %s\n", paste(vals, collapse = ",")))
}

# Two atoms for same symbol -> union
n_tests = n_tests + 1
a1 = A %among% "a"
a2 = A %among% "b"
result = CnfClause(list(a1, a2))
vals = sort(unclass(result)[["A"]])
if (!identical(vals, c("a", "b"))) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: union of atoms should give {a,b}, got %s\n", paste(vals, collapse = ",")))
}

# Tautology via union
n_tests = n_tests + 1
a1 = A %among% c("a", "b")
a2 = A %among% c("b", "c")
result = CnfClause(list(a1, a2))
if (!isTRUE(unclass(result))) {
  n_failures = n_failures + 1
  cat("FAIL: A %among% {a,b} | A %among% {b,c} with domain {a,b,c} should be TRUE\n")
}

# CnfClause from another CnfClause
n_tests = n_tests + 1
cl_inner = A %among% c("a", "b") | B %among% "x"
result = CnfClause(list(cl_inner, C %among% "1"))
if (length(unclass(result)) != 3) {
  n_failures = n_failures + 1
  cat("FAIL: clause from clause should merge all symbols\n")
}

cat(sprintf("  CnfClause construction: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
