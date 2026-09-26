#!/usr/bin/env Rscript
# Test with overlapping/confusing variable names:
# - "V1" and "V10" (one is substring of other)
# - "" (empty string)
# - Names that look like numbers
# - Names with special regex characters
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

# === Substring names: V1 and V10 ===
cat("=== Substring names (V1, V10) ===\n")
set.seed(92001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  V1 = CnfSymbol(u, "V1", dom)
  V10 = CnfSymbol(u, "V10", dom)
  V100 = CnfSymbol(u, "V100", dom)

  syms = list(V1 = V1, V10 = V10, V100 = V100)

  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:3, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [substr-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("substr-%d", trial))
}
cat(sprintf("  Substring names: %d tests, %d failures\n", n_tests, n_failures))

# === Numeric-like names ===
cat("\n=== Numeric-like names ===\n")
set.seed(92002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("x", "y", "z")
  S1 = CnfSymbol(u, "1", dom)
  S2 = CnfSymbol(u, "2", dom)
  S3 = CnfSymbol(u, "10", dom)

  syms = list("1" = S1, "2" = S2, "10" = S3)

  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [numname-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("numname-%d", trial))
}
cat(sprintf("  Numeric names: %d tests, %d failures\n", n_tests, n_failures))

# === Names with dots and special chars ===
cat("\n=== Special character names ===\n")
set.seed(92003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  SA = CnfSymbol(u, "a.b", dom)
  SB = CnfSymbol(u, "a_b", dom)
  SC = CnfSymbol(u, "a b", dom)

  syms = list("a.b" = SA, "a_b" = SB, "a b" = SC)

  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [special-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("special-%d", trial))
}
cat(sprintf("  Special char names: %d tests, %d failures\n", n_tests, n_failures))

# === Values that look like variable names ===
cat("\n=== Confusing values ===\n")
set.seed(92004)

for (trial in 1:200) {
  u = CnfUniverse()
  # Values look like variable names
  dom = c("A", "B", "C")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  syms = list(A = A, B = B)

  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [confuse-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("confuse-%d", trial))
}
cat(sprintf("  Confusing values: %d tests, %d failures\n", n_tests, n_failures))

# === Single-char names with similar values ===
cat("\n=== Single-char names ===\n")
set.seed(92005)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("1", "2", "3")
  A = CnfSymbol(u, "a", dom)
  B = CnfSymbol(u, "b", dom)
  C = CnfSymbol(u, "c", dom)

  syms = list(a = A, b = B, c = C)

  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:3, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [singchar-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("singchar-%d", trial))
}
cat(sprintf("  Single-char names: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
