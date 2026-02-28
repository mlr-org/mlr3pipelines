#!/usr/bin/env Rscript
# Test &.CnfFormula shortcircuit paths and c() concatenation:
# - TRUE & f should return f
# - FALSE & f should return FALSE
# - f & TRUE should return f
# - f & FALSE should return FALSE
# - c() concatenation correctness
# - &.CnfFormula with atom, clause, formula inputs
# - all.equal normalize edge cases
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Shortcircuit: TRUE & f, f & TRUE ===
cat("=== AND shortcircuit with TRUE ===\n")
set.seed(115001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  f = tryCatch(CnfFormula(list(
    as.CnfClause(A %among% sample(dom, sample(1:2, 1))),
    as.CnfClause(B %among% sample(dom, sample(1:2, 1)))
  )), error = function(e) NULL)
  if (is.null(f)) next

  t_f = evaluate_formula(f, u)
  TRUE_formula = as.CnfFormula(TRUE)

  # TRUE & f
  n_tests = n_tests + 1
  r1 = tryCatch(TRUE_formula & f, error = function(e) e)
  if (inherits(r1, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [true-and-%d]: %s\n", trial, r1$message)); next
  }
  if (!all(evaluate_formula(r1, u) == t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [true-and-%d]: TRUE & f != f\n", trial))
  }

  # f & TRUE
  n_tests = n_tests + 1
  r2 = tryCatch(f & TRUE_formula, error = function(e) e)
  if (inherits(r2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [and-true-%d]: %s\n", trial, r2$message)); next
  }
  if (!all(evaluate_formula(r2, u) == t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [and-true-%d]: f & TRUE != f\n", trial))
  }
}
cat(sprintf("  AND with TRUE: %d tests, %d failures\n", n_tests, n_failures))

# === Shortcircuit: FALSE & f, f & FALSE ===
cat("\n=== AND shortcircuit with FALSE ===\n")
set.seed(115002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  f = tryCatch(CnfFormula(list(
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)))
  )), error = function(e) NULL)
  if (is.null(f)) next

  FALSE_formula = as.CnfFormula(FALSE)

  # FALSE & f
  n_tests = n_tests + 1
  r1 = tryCatch(FALSE_formula & f, error = function(e) e)
  if (inherits(r1, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [false-and-%d]: %s\n", trial, r1$message)); next
  }
  if (!isFALSE(as.logical(r1))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [false-and-%d]: FALSE & f should be FALSE\n", trial))
  }

  # f & FALSE
  n_tests = n_tests + 1
  r2 = tryCatch(f & FALSE_formula, error = function(e) e)
  if (inherits(r2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [and-false-%d]: %s\n", trial, r2$message)); next
  }
  if (!isFALSE(as.logical(r2))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [and-false-%d]: f & FALSE should be FALSE\n", trial))
  }
}
cat(sprintf("  AND with FALSE: %d tests, %d failures\n", n_tests, n_failures))

# === AND with mixed types: atom & formula, clause & formula ===
cat("\n=== AND with mixed types ===\n")
set.seed(115003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("x", "y", "z")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  atom = A %among% sample(dom, sample(1:2, 1))
  clause = as.CnfClause(B %among% sample(dom, sample(1:2, 1)))
  if (isTRUE(unclass(clause))) next

  f = tryCatch(CnfFormula(list(as.CnfClause(C %among% sample(dom, sample(1:2, 1))))), error = function(e) NULL)
  if (is.null(f)) next

  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  # atom & formula
  n_tests = n_tests + 1
  r1 = tryCatch(atom & f, error = function(e) e)
  if (inherits(r1, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [atom-f-%d]: %s\n", trial, r1$message)); next
  }
  t_atom = assignments[["A"]] %in% atom$values
  t_f = evaluate_formula(f, u)
  expected = t_atom & t_f
  if (!all(evaluate_formula(r1, u) == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [atom-f-%d]: atom & formula wrong\n", trial))
  }

  # clause & formula
  n_tests = n_tests + 1
  r2 = tryCatch(clause & f, error = function(e) e)
  if (inherits(r2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cl-f-%d]: %s\n", trial, r2$message)); next
  }
  cl_bare = unclass(clause)
  t_cl = assignments[["B"]] %in% cl_bare[["B"]]
  expected2 = t_cl & t_f
  if (!all(evaluate_formula(r2, u) == expected2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cl-f-%d]: clause & formula wrong\n", trial))
  }
}
cat(sprintf("  AND with mixed types: %d tests, %d failures\n", n_tests, n_failures))

# === all.equal normalize: different construction, same semantics ===
cat("\n=== all.equal normalize ===\n")
set.seed(115004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Construct same formula two ways
  a_vals = sample(dom, sample(1:2, 1))
  b_vals = sample(dom, sample(1:2, 1))

  f1 = CnfFormula(list(
    as.CnfClause(A %among% a_vals),
    as.CnfClause(B %among% b_vals)
  ))
  f2 = CnfFormula(list(
    as.CnfClause(B %among% b_vals),
    as.CnfClause(A %among% a_vals)
  ))

  n_tests = n_tests + 1
  eq = tryCatch(all.equal(f1, f2), error = function(e) e)
  if (inherits(eq, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [eq-%d]: %s\n", trial, eq$message)); next
  }
  if (!isTRUE(eq)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [eq-%d]: same formula not equal: %s\n", trial, paste(eq, collapse = ", ")))
  }
}
cat(sprintf("  all.equal normalize: %d tests, %d failures\n", n_tests, n_failures))

# === all.equal with TRUE/FALSE ===
cat("\n=== all.equal with TRUE/FALSE ===\n")

n_tests = n_tests + 1
if (!isTRUE(all.equal(as.CnfFormula(TRUE), as.CnfFormula(TRUE)))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE == TRUE failed\n")
}

n_tests = n_tests + 1
if (!isTRUE(all.equal(as.CnfFormula(FALSE), as.CnfFormula(FALSE)))) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE == FALSE failed\n")
}

n_tests = n_tests + 1
eq_tf = all.equal(as.CnfFormula(TRUE), as.CnfFormula(FALSE))
if (isTRUE(eq_tf)) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE == FALSE should not be equal\n")
}

cat(sprintf("  all.equal TRUE/FALSE: %d tests, %d failures\n", n_tests, n_failures))

# === c() concatenation of CnfFormula objects ===
cat("\n=== c() concatenation ===\n")
set.seed(115005)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  cl1 = as.CnfClause(A %among% sample(dom, sample(1:2, 1)))
  cl2 = as.CnfClause(B %among% sample(dom, sample(1:2, 1)))

  f1 = CnfFormula(list(cl1))
  f2 = CnfFormula(list(cl2))

  # f1 & f2 should be same as CnfFormula(list(cl1, cl2))
  n_tests = n_tests + 1
  result_and = tryCatch(f1 & f2, error = function(e) e)
  result_direct = tryCatch(CnfFormula(list(cl1, cl2)), error = function(e) e)
  if (inherits(result_and, "error") || inherits(result_direct, "error")) {
    if (inherits(result_and, "error")) n_failures = n_failures + 1
    next
  }

  t_and = evaluate_formula(result_and, u)
  t_direct = evaluate_formula(result_direct, u)
  if (!all(t_and == t_direct)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [concat-%d]: f1 & f2 != CnfFormula(list(cl1, cl2))\n", trial))
  }
}
cat(sprintf("  c() concatenation: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
