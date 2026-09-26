#!/usr/bin/env Rscript
# Test edge cases around how c() works with CnfFormula objects,
# and other subtle interactions.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a", "b", "c"))
B = CnfSymbol(u, "B", c("x", "y"))

cat("=== c() behavior with CnfFormulas ===\n")

# Normal concatenation
f1 = CnfFormula(list(as.CnfClause(A %among% c("a", "b"))))
f2 = CnfFormula(list(as.CnfClause(B %among% "x")))

n_tests = n_tests + 1
r = c(f1, f2)
if (length(r) != 2) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: c(f1, f2) should have length 2, got %d\n", length(r)))
}

# c() with TRUE formula
f_true = as.CnfFormula(TRUE)
n_tests = n_tests + 1
r = c(f1, f_true)
cat(sprintf("  c(f1, TRUE_formula) class: %s, length: %d\n", class(r), length(r)))
# TRUE formula is stored as logical TRUE, so c(list(...), TRUE) creates mixed list
# This would be passed to simplify_cnf which expects only lists
# But this only matters if used in &.CnfFormula

# c() with FALSE formula
f_false = as.CnfFormula(FALSE)
n_tests = n_tests + 1
r = c(f1, f_false)
cat(sprintf("  c(f1, FALSE_formula) class: %s, length: %d\n", class(r), length(r)))
# c(list(...), FALSE) also creates mixed content

# Check what &.CnfFormula actually passes to simplify_cnf
cat("\n=== What & passes to simplify_cnf ===\n")

# Case: normal & normal
n_tests = n_tests + 1
r = tryCatch(f1 & f2, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: f1 & f2 error: %s\n", r$message))
} else {
  t_result = evaluate_formula(r, u)
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  if (!all(t_result == (t1 & t2))) {
    n_failures = n_failures + 1
    cat("FAIL: f1 & f2 semantics wrong\n")
  }
}

# Case: normal & TRUE formula
# isTRUE(unclass(f_true)) = TRUE, so shortcut returns e2 = as.CnfFormula(f_true) = f_true
# Wait - line 314: if (isTRUE(e1_bare) || isFALSE(e2_bare)) return(e2)
# e1 = f1, e1_bare = unclass(f1) = list(...), isTRUE(list()) = FALSE
# e2 = f_true, e2_bare = unclass(f_true) = TRUE, isTRUE(TRUE) = TRUE
# So line 312: isTRUE(e2_bare) = TRUE -> return(e1) which is as.CnfFormula(f1) = f1
# Correct!
n_tests = n_tests + 1
r = tryCatch(f1 & f_true, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: f1 & TRUE error: %s\n", r$message))
}

# Case: TRUE & normal
# e1_bare = TRUE, isTRUE(e2_bare)? e2_bare = unclass(f1) = list(...), isTRUE(list()) = FALSE
# isFALSE(e1_bare) = FALSE
# So line 312 doesn't fire.
# Line 314: isTRUE(e1_bare) = isTRUE(TRUE) = TRUE -> return(e2) = as.CnfFormula(f1) = f1
# Wait - e2 was f1, and e2 = as.CnfFormula(e2) = as.CnfFormula(f1) = f1
# Correct!
n_tests = n_tests + 1
r = tryCatch(f_true & f1, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: TRUE & f1 error: %s\n", r$message))
}

cat(sprintf("  c() and & tests: %d tests, %d failures\n", n_tests, n_failures))

# === Test the CnfFormula -> CnfFormula conversion in constructor ===
cat("\n=== CnfFormula containing CnfFormula ===\n")

# CnfFormula(list(formula1, formula2)) - uses the other_entries path
set.seed(80001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Build sub-formulas
  clauses1 = lapply(1:sample(1:2, 1), function(j) {
    s = sample(names(syms), sample(1:2, 1))
    atoms = lapply(s, function(si) syms[[si]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f1 = tryCatch(CnfFormula(clauses1), error = function(e) NULL)
  if (is.null(f1)) next

  clauses2 = lapply(1:sample(1:2, 1), function(j) {
    s = sample(names(syms), sample(1:2, 1))
    atoms = lapply(s, function(si) syms[[si]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f2 = tryCatch(CnfFormula(clauses2), error = function(e) NULL)
  if (is.null(f2)) next

  # CnfFormula from list of formulas (no TRUE/FALSE to avoid Bug #3/#4)
  f_combined = tryCatch(CnfFormula(list(f1, f2)), error = function(e) e)
  if (inherits(f_combined, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [ff-%d]: %s\n", trial, f_combined$message)); next
  }

  # Should be equivalent to f1 & f2
  f_anded = tryCatch(f1 & f2, error = function(e) NULL)
  if (is.null(f_anded)) next

  t_combined = evaluate_formula(f_combined, u)
  t_anded = evaluate_formula(f_anded, u)

  n_tests = n_tests + 1
  if (!all(t_combined == t_anded)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ff-%d]: CnfFormula(list(f1,f2)) != f1 & f2\n", trial))
  }
}
cat(sprintf("  Formula from formulas: %d tests, %d failures\n", n_tests, n_failures))

# === Test CnfFormula with mixed clauses and formulas ===
cat("\n=== Mixed clause + formula in CnfFormula ===\n")
set.seed(80002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # One clause
  s = sample(names(syms), sample(1:2, 1))
  atoms = lapply(s, function(si) syms[[si]] %among% sample(dom, sample(1:2, 1)))
  cl = as.CnfClause(Reduce(`|`, atoms))

  # One formula
  clauses_f = lapply(1:sample(1:2, 1), function(j) {
    s2 = sample(names(syms), sample(1:2, 1))
    atoms2 = lapply(s2, function(si) syms[[si]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms2))
  })
  f = tryCatch(CnfFormula(clauses_f), error = function(e) NULL)
  if (is.null(f)) next

  # Mix them
  f_mixed = tryCatch(CnfFormula(list(cl, f)), error = function(e) e)
  if (inherits(f_mixed, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [mix-%d]: %s\n", trial, f_mixed$message)); next
  }

  # Compare: should be clause AND formula
  f_explicit = tryCatch(as.CnfFormula(cl) & f, error = function(e) NULL)
  if (is.null(f_explicit)) next

  t_mixed = evaluate_formula(f_mixed, u)
  t_explicit = evaluate_formula(f_explicit, u)

  n_tests = n_tests + 1
  if (!all(t_mixed == t_explicit)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mix-%d]: mixed != explicit\n", trial))
  }
}
cat(sprintf("  Mixed clause+formula: %d tests, %d failures\n", n_tests, n_failures))

# === as.list roundtrip for FALSE formulas ===
cat("\n=== as.list roundtrip for FALSE formulas ===\n")

u_last = CnfUniverse()
A_last = CnfSymbol(u_last, "A", c("a", "b", "c"))

n_tests = n_tests + 1
f_false_custom = CnfFormula(list(
  as.CnfClause(A_last %among% "a"),
  as.CnfClause(A_last %among% "b")
))
cat(sprintf("  Contradictory formula as.logical: %s\n", as.logical(f_false_custom)))
if (!isFALSE(as.logical(f_false_custom))) {
  n_failures = n_failures + 1
  cat("FAIL: contradictory formula should be FALSE\n")
}

# as.list on FALSE formula
n_tests = n_tests + 1
f_false_list = as.list(f_false_custom)
cat(sprintf("  as.list(FALSE formula) length: %d\n", length(f_false_list)))
if (length(f_false_list) != 1) {
  n_failures = n_failures + 1
  cat("FAIL: as.list(FALSE) should have 1 element\n")
} else if (!isFALSE(unclass(f_false_list[[1]]))) {
  n_failures = n_failures + 1
  cat("FAIL: as.list(FALSE)[[1]] should be FALSE clause\n")
}

cat(sprintf("  FALSE formula roundtrip: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
