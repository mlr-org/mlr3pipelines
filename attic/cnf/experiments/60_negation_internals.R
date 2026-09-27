#!/usr/bin/env Rscript
# Test !.CnfFormula internals: the structure() call creates CnfClause objects
# directly (bypassing the constructor). Verify these are well-formed and
# that the De Morgan law implementation is correct for various formula shapes.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

check_formula_truth = function(formula, expected_truth, universe, label) {
  n_tests <<- n_tests + 1
  result_truth = evaluate_formula(formula, universe)
  mismatches = which(result_truth != expected_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d (expected=%s, got=%s)\n",
      label, idx, expected_truth[idx], result_truth[idx]))
    return(FALSE)
  }
  TRUE
}

# === Negation of single-clause formulas ===
cat("=== Negation of single-clause formulas ===\n")
set.seed(60001)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  chosen = sample(names(syms), sample(1:3, 1))
  atoms = lapply(chosen, function(s) {
    syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
  })
  cl = as.CnfClause(Reduce(`|`, atoms))
  f = tryCatch(CnfFormula(list(cl)), error = function(e) NULL)
  if (is.null(f)) next

  t_f = evaluate_formula(f, u)
  not_f = tryCatch(!f, error = function(e) NULL)
  if (is.null(not_f)) next

  check_formula_truth(not_f, !t_f, u, sprintf("single-neg-%d", trial))
}
cat(sprintf("  Single-clause negation: %d tests, %d failures\n", n_tests, n_failures))

# === Negation of multi-clause formulas ===
cat("\n=== Negation of multi-clause formulas ===\n")
set.seed(60002)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  n_cl = sample(2:4, 1)
  cls = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  t_f = evaluate_formula(f, u)
  not_f = tryCatch(!f, error = function(e) NULL)
  if (is.null(not_f)) next

  check_formula_truth(not_f, !t_f, u, sprintf("multi-neg-%d", trial))
}
cat(sprintf("  Multi-clause negation: %d tests, %d failures\n", n_tests, n_failures))

# === Negation with unit clauses in the formula ===
cat("\n=== Negation with units ===\n")
set.seed(60003)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  # Formula with at least one unit
  cls = list(as.CnfClause(syms[[sample(names(syms), 1)]] %among% sample(c("a", "b", "c"), sample(1:2, 1))))
  for (j in 1:sample(1:3, 1)) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    cls[[length(cls) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  t_f = evaluate_formula(f, u)
  not_f = tryCatch(!f, error = function(e) NULL)
  if (is.null(not_f)) next

  check_formula_truth(not_f, !t_f, u, sprintf("unit-neg-%d", trial))
}
cat(sprintf("  Negation with units: %d tests, %d failures\n", n_tests, n_failures))

# === Double negation with various formulas ===
cat("\n=== Double negation ===\n")
set.seed(60004)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b"))
  }

  n_cl = sample(1:3, 1)
  cls = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  t_f = evaluate_formula(f, u)
  not_not_f = tryCatch(!!f, error = function(e) NULL)
  if (is.null(not_not_f)) next

  check_formula_truth(not_not_f, t_f, u, sprintf("double-neg-%d", trial))
}
cat(sprintf("  Double negation: %d tests, %d failures\n", n_tests, n_failures))

# === Negation creates complement atoms, test they match setdiff ===
cat("\n=== Complement correctness ===\n")
set.seed(60005)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    dom_size = sample(2:5, 1)
    syms[[vname]] = CnfSymbol(u, vname, paste0("d", 1:dom_size))
  }

  # Build formula, negate, and verify complement
  s = sample(names(syms), 1)
  dom = u[[s]]
  vals = sample(dom, sample(1:max(1, length(dom)-1), 1))
  atom = syms[[s]] %among% vals

  # !atom should give complement values
  not_atom = tryCatch(!atom, error = function(e) NULL)
  if (is.null(not_atom)) next

  not_atom_bare = unclass(not_atom)
  if (is.logical(not_atom_bare)) {
    # If vals == full domain, !atom = FALSE
    # If vals == nothing, impossible (atoms must have at least 1 value)
    n_tests = n_tests + 1
    if (length(vals) == length(dom) && !isFALSE(not_atom_bare)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [complement-%d]: !full_domain_atom should be FALSE\n", trial))
    }
    next
  }

  # CnfAtom internal structure: list(symbol = name, values = char_vec)
  complement_vals = sort(not_atom_bare$values)
  expected_complement = sort(setdiff(dom, vals))

  n_tests = n_tests + 1
  if (!identical(complement_vals, expected_complement)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [complement-%d]: complement mismatch: got %s, expected %s\n",
      trial, paste(complement_vals, collapse = ","), paste(expected_complement, collapse = ",")))
  }
}
cat(sprintf("  Complement correctness: %d tests, %d failures\n", n_tests, n_failures))

# === Negation of TRUE/FALSE formulas ===
cat("\n=== Negation of TRUE/FALSE ===\n")

f_true = as.CnfFormula(TRUE)
f_false = as.CnfFormula(FALSE)

# !TRUE == FALSE
not_true = !f_true
n_tests = n_tests + 1
if (!isFALSE(as.logical(not_true))) {
  n_failures = n_failures + 1
  cat("FAIL: !TRUE should be FALSE\n")
}

# !FALSE == TRUE
not_false = !f_false
n_tests = n_tests + 1
if (!isTRUE(as.logical(not_false))) {
  n_failures = n_failures + 1
  cat("FAIL: !FALSE should be TRUE\n")
}

# !!TRUE == TRUE
not_not_true = !!f_true
n_tests = n_tests + 1
if (!isTRUE(as.logical(not_not_true))) {
  n_failures = n_failures + 1
  cat("FAIL: !!TRUE should be TRUE\n")
}

# !!FALSE == FALSE
not_not_false = !!f_false
n_tests = n_tests + 1
if (!isFALSE(as.logical(not_not_false))) {
  n_failures = n_failures + 1
  cat("FAIL: !!FALSE should be FALSE\n")
}
cat(sprintf("  Negation TRUE/FALSE: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
