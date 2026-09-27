#!/usr/bin/env Rscript
# Deep testing of [.CnfClause subsetting:
# - Numeric indexing
# - Character indexing
# - Logical indexing
# - Edge cases: 0, empty, out-of-range
# - Consistency between [.CnfClause and as.list
# - The minor issue: FALSE_clause[FALSE] should it error?
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a", "b", "c"))
B = CnfSymbol(u, "B", c("d", "e", "f"))
C = CnfSymbol(u, "C", c("x", "y", "z"))

# Create various clauses to test
clause_1sym = as.CnfClause(A %among% c("a", "b"))
clause_2sym = A %among% c("a", "b") | B %among% c("d", "e")
clause_3sym = A %among% "a" | B %among% "d" | C %among% "x"
clause_true = as.CnfClause(TRUE)
clause_false = as.CnfClause(FALSE)

# === Numeric indexing ===
cat("=== Numeric indexing ===\n")

# Positive numeric on normal clause
n_tests = n_tests + 1
r = tryCatch(clause_2sym[1], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: clause_2sym[1]: %s\n", r$message))
} else if (!inherits(r, "CnfClause")) {
  n_failures = n_failures + 1
  cat("FAIL: clause_2sym[1] not CnfClause\n")
}

n_tests = n_tests + 1
r = tryCatch(clause_2sym[c(1, 2)], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: clause_2sym[c(1,2)]: %s\n", r$message))
}

# 0 index should return FALSE clause
n_tests = n_tests + 1
r = tryCatch(clause_2sym[0], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: clause_2sym[0]: %s\n", r$message))
} else if (!isFALSE(unclass(r))) {
  n_failures = n_failures + 1
  cat("FAIL: clause_2sym[0] should be FALSE\n")
}

# Empty numeric index
n_tests = n_tests + 1
r = tryCatch(clause_2sym[integer(0)], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: clause_2sym[integer(0)]: %s\n", r$message))
} else if (!isFALSE(unclass(r))) {
  n_failures = n_failures + 1
  cat("FAIL: clause_2sym[integer(0)] should be FALSE\n")
}

# === Character indexing ===
cat("\n=== Character indexing ===\n")

n_tests = n_tests + 1
r = tryCatch(clause_2sym["A"], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: clause_2sym['A']: %s\n", r$message))
} else {
  # Result should be a clause with only A
  if (length(names(unclass(r))) != 1 || names(unclass(r)) != "A") {
    n_failures = n_failures + 1
    cat("FAIL: clause_2sym['A'] should have only symbol A\n")
  }
}

n_tests = n_tests + 1
r = tryCatch(clause_3sym[c("A", "C")], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: clause_3sym[c('A','C')]: %s\n", r$message))
} else {
  syms = names(unclass(r))
  if (!setequal(syms, c("A", "C"))) {
    n_failures = n_failures + 1
    cat("FAIL: clause_3sym[c('A','C')] wrong symbols\n")
  }
}

# Empty character index
n_tests = n_tests + 1
r = tryCatch(clause_2sym[character(0)], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: clause_2sym[character(0)]: %s\n", r$message))
} else if (!isFALSE(unclass(r))) {
  n_failures = n_failures + 1
  cat("FAIL: clause_2sym[character(0)] should be FALSE\n")
}

# === Logical indexing ===
cat("\n=== Logical indexing ===\n")

n_tests = n_tests + 1
r = tryCatch(clause_2sym[c(TRUE, FALSE)], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: clause_2sym[c(TRUE,FALSE)]: %s\n", r$message))
} else {
  if (length(unclass(r)) != 1) {
    n_failures = n_failures + 1
    cat("FAIL: clause_2sym[c(TRUE,FALSE)] should have 1 symbol\n")
  }
}

n_tests = n_tests + 1
r = tryCatch(clause_2sym[c(TRUE, TRUE)], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: clause_2sym[c(TRUE,TRUE)]: %s\n", r$message))
}

n_tests = n_tests + 1
r = tryCatch(clause_2sym[c(FALSE, FALSE)], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: clause_2sym[c(FALSE,FALSE)]: %s\n", r$message))
} else if (!isFALSE(unclass(r))) {
  n_failures = n_failures + 1
  cat("FAIL: clause_2sym[c(FALSE,FALSE)] should be FALSE\n")
}

# === TRUE/FALSE clause subsetting ===
cat("\n=== TRUE/FALSE clause subsetting ===\n")

# TRUE clause[1] should work
n_tests = n_tests + 1
r = tryCatch(clause_true[1], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: TRUE[1]: %s\n", r$message))
} else if (!isTRUE(unclass(r))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE[1] should be TRUE\n")
}

# TRUE clause[TRUE] should work
n_tests = n_tests + 1
r = tryCatch(clause_true[TRUE], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: TRUE[TRUE]: %s\n", r$message))
}

# TRUE clause[0] should be FALSE
n_tests = n_tests + 1
r = tryCatch(clause_true[0], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: TRUE[0]: %s\n", r$message))
} else if (!isFALSE(unclass(r))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE[0] should be FALSE\n")
}

# FALSE clause[0] should work (returns FALSE)
n_tests = n_tests + 1
r = tryCatch(clause_false[0], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: FALSE[0]: %s\n", r$message))
} else if (!isFALSE(unclass(r))) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE[0] should be FALSE\n")
}

# FALSE clause[integer(0)] should work
n_tests = n_tests + 1
r = tryCatch(clause_false[integer(0)], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: FALSE[integer(0)]: %s\n", r$message))
}

# FALSE clause[1] should error
n_tests = n_tests + 1
r = tryCatch(clause_false[1], error = function(e) "expected_error")
if (!identical(r, "expected_error")) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE[1] should error\n")
}

# FALSE clause[FALSE] - the known minor issue
n_tests = n_tests + 1
r = tryCatch(clause_false[FALSE], error = function(e) "error")
if (identical(r, "error")) {
  cat("  Note: FALSE[FALSE] errors (known minor issue)\n")
} else {
  cat("  Note: FALSE[FALSE] works (returns something)\n")
}

# === Missing index ===
cat("\n=== Missing index ===\n")

n_tests = n_tests + 1
r = tryCatch(clause_2sym[], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: clause[]: %s\n", r$message))
} else {
  eq = tryCatch(all.equal(r, clause_2sym), error = function(e) "error")
  if (!isTRUE(eq)) {
    n_failures = n_failures + 1
    cat("FAIL: clause[] should return itself\n")
  }
}

# === [[ accessor ===
cat("\n=== [[ accessor ===\n")

# Normal clause [[ by name
n_tests = n_tests + 1
r = tryCatch(clause_2sym[["A"]], error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: clause[['A']]: %s\n", r$message))
} else {
  if (!setequal(r, c("a", "b"))) {
    n_failures = n_failures + 1
    cat("FAIL: clause[['A']] should be c('a','b')\n")
  }
}

# [[ with non-existent name - silently returns NULL (minor issue)
n_tests = n_tests + 1
r = tryCatch(clause_2sym[["Z"]], error = function(e) e)
if (inherits(r, "error")) {
  cat(sprintf("  Note: clause[['Z']] errors: %s\n", r$message))
} else if (!is.null(r)) {
  cat("  Note: clause[['Z']] returns something unexpected\n")
} else {
  cat("  Note: clause[['Z']] silently returns NULL\n")
}

# === Consistency: [ then as.list ===
cat("\n=== Consistency checks ===\n")
set.seed(158001)

for (trial in 1:200) {
  u2 = CnfUniverse()
  dom = c("a", "b", "c", "d")
  X = CnfSymbol(u2, "X", dom)
  Y = CnfSymbol(u2, "Y", dom)
  Z = CnfSymbol(u2, "Z", dom)
  syms = list(X = X, Y = Y, Z = Z)

  n_sym = sample(1:3, 1)
  chosen = sample(names(syms), n_sym)
  atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  clause = as.CnfClause(Reduce(`|`, atoms))
  if (isTRUE(unclass(clause)) || isFALSE(unclass(clause))) next

  # Test: names() should match
  n_tests = n_tests + 1
  nms = names(clause)
  if (!all(nms %in% c("X", "Y", "Z"))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [names-%d]: unexpected symbol name\n", trial))
  }

  # Test: length should match number of symbols
  n_tests = n_tests + 1
  if (length(clause) != length(nms)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [length-%d]: length != names length\n", trial))
  }

  # Test: subsetting each symbol individually
  for (nm in nms) {
    n_tests = n_tests + 1
    sub = tryCatch(clause[nm], error = function(e) e)
    if (inherits(sub, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [sub-%d-%s]: %s\n", trial, nm, sub$message)); next
    }
    if (!inherits(sub, "CnfClause")) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [sub-%d-%s]: not CnfClause\n", trial, nm)); next
    }
    # Values should match
    if (!setequal(sub[[nm]], clause[[nm]])) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [sub-%d-%s]: values mismatch\n", trial, nm))
    }
  }
}
cat(sprintf("  Consistency: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
