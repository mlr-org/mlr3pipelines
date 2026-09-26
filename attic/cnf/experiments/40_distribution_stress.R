#!/usr/bin/env Rscript
# Test | distribution on formulas with many clauses
# This exercises the |.CnfFormula distribution path which creates new clauses
# from the cross-product, then simplifies
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

# === Distribution: f1 | f2 semantic correctness ===
cat("=== Distribution semantic correctness ===\n")
set.seed(40001)

for (trial in 1:200) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("x1", "x2", "x3"))
  Y = CnfSymbol(u, "Y", c("y1", "y2"))
  Z = CnfSymbol(u, "Z", c("z1", "z2"))

  # Build f1 with 2-3 clauses
  f1_clauses = list()
  for (i in 1:sample(2:3, 1)) {
    n_atoms = sample(1:3, 1)
    syms = list(X = X, Y = Y, Z = Z)
    chosen = sample(c("X", "Y", "Z"), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    f1_clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  # Build f2 with 2-3 clauses
  f2_clauses = list()
  for (i in 1:sample(2:3, 1)) {
    n_atoms = sample(1:3, 1)
    syms = list(X = X, Y = Y, Z = Z)
    chosen = sample(c("X", "Y", "Z"), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    f2_clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f1 = tryCatch(CnfFormula(f1_clauses), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(f2_clauses), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  f_or = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(f_or, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [dist trial %d]: %s\n", trial, f_or$message)); next
  }

  # Verify: f1 | f2 should be TRUE exactly when f1 is TRUE or f2 is TRUE
  n_tests = n_tests + 1
  t_or = evaluate_formula(f_or, u)
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 | t2
  mismatches = which(t_or != expected)
  if (length(mismatches)) {
    n_failures = n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [dist trial %d]: row %d (f|g=%s, f=%s, g=%s)\n", trial, idx, t_or[idx], t1[idx], t2[idx]))
  }
}
cat(sprintf("  Distribution: %d tests, %d failures\n", n_tests, n_failures))

# === Distribution followed by AND ===
cat("\n=== Distribution + AND combinations ===\n")
set.seed(40002)

for (trial in 1:200) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2"))
  B = CnfSymbol(u, "B", c("b1", "b2"))
  C = CnfSymbol(u, "C", c("c1", "c2"))

  syms = list(A = A, B = B, C = C)

  mk_formula = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_atoms = sample(1:3, 1)
      chosen = sample(c("A", "B", "C"), n_atoms)
      atoms = lapply(chosen, function(s) {
        dom = u[[s]]
        syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = mk_formula()
  f2 = mk_formula()
  f3 = mk_formula()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  # Test (f1 | f2) & f3
  r = tryCatch((f1 | f2) & f3, error = function(e) NULL)
  if (is.null(r)) next

  n_tests = n_tests + 1
  t_r = evaluate_formula(r, u)
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  expected = (t1 | t2) & t3
  if (!all(t_r == expected)) {
    n_failures = n_failures + 1
    idx = which(t_r != expected)[1]
    cat(sprintf("FAIL [(f1|f2)&f3 trial %d]: row %d\n", trial, idx))
  }

  # Test f1 & (f2 | f3)
  r2 = tryCatch(f1 & (f2 | f3), error = function(e) NULL)
  if (!is.null(r2)) {
    n_tests = n_tests + 1
    t_r2 = evaluate_formula(r2, u)
    expected2 = t1 & (t2 | t3)
    if (!all(t_r2 == expected2)) {
      n_failures = n_failures + 1
      idx = which(t_r2 != expected2)[1]
      cat(sprintf("FAIL [f1&(f2|f3) trial %d]: row %d\n", trial, idx))
    }
  }
}
cat(sprintf("  Distribution + AND: %d tests, %d failures\n", n_tests, n_failures))

# === Triple OR distribution ===
cat("\n=== Triple OR distribution ===\n")
set.seed(40003)

for (trial in 1:100) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("x1", "x2"))
  Y = CnfSymbol(u, "Y", c("y1", "y2"))
  Z = CnfSymbol(u, "Z", c("z1", "z2"))

  syms = list(X = X, Y = Y, Z = Z)

  mk_formula = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      n_atoms = sample(1:3, 1)
      chosen = sample(c("X", "Y", "Z"), n_atoms)
      atoms = lapply(chosen, function(s) {
        dom = u[[s]]
        syms[[s]] %among% sample(dom, 1)
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = mk_formula()
  f2 = mk_formula()
  f3 = mk_formula()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  # f1 | f2 | f3
  r = tryCatch(f1 | f2 | f3, error = function(e) NULL)
  if (is.null(r)) next

  n_tests = n_tests + 1
  t_r = evaluate_formula(r, u)
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  expected = t1 | t2 | t3
  if (!all(t_r == expected)) {
    n_failures = n_failures + 1
    idx = which(t_r != expected)[1]
    cat(sprintf("FAIL [f1|f2|f3 trial %d]: row %d\n", trial, idx))
  }
}
cat(sprintf("  Triple OR: %d tests, %d failures\n", n_tests, n_failures))

# === Negation chains ===
cat("\n=== Negation chains ===\n")
set.seed(40004)

for (trial in 1:100) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("x1", "x2"))
  Y = CnfSymbol(u, "Y", c("y1", "y2"))

  syms = list(X = X, Y = Y)

  cls = lapply(1:sample(1:2, 1), function(j) {
    n_atoms = sample(1:2, 1)
    chosen = sample(c("X", "Y"), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  # Test: !!f should be semantically equivalent to f
  f_neg = tryCatch(!f, error = function(e) NULL)
  if (is.null(f_neg)) next
  f_neg_neg = tryCatch(!f_neg, error = function(e) NULL)
  if (is.null(f_neg_neg)) next

  n_tests = n_tests + 1
  t_f = evaluate_formula(f, u)
  t_nn = evaluate_formula(f_neg_neg, u)
  if (!all(t_f == t_nn)) {
    n_failures = n_failures + 1
    idx = which(t_f != t_nn)[1]
    cat(sprintf("FAIL [!!f trial %d]: row %d (f=%s, !!f=%s)\n", trial, idx, t_f[idx], t_nn[idx]))
  }

  # Test: !f should be complement of f
  n_tests = n_tests + 1
  t_neg = evaluate_formula(f_neg, u)
  if (!all(t_f != t_neg)) {
    n_failures = n_failures + 1
    idx = which(t_f == t_neg)[1]
    cat(sprintf("FAIL [!f complement trial %d]: row %d (f=%s, !f=%s)\n", trial, idx, t_f[idx], t_neg[idx]))
  }
}
cat(sprintf("  Negation chains: %d tests, %d failures\n", n_tests, n_failures))

# === as.logical behavior ===
cat("\n=== as.logical behavior ===\n")

u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b"))
Y = CnfSymbol(u, "Y", c("c", "d"))

# Tautology should give TRUE
n_tests = n_tests + 1
f_taut = CnfFormula(list(as.CnfClause(X %among% c("a", "b"))))
if (!isTRUE(as.logical(f_taut))) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: tautology as.logical not TRUE (got %s)\n", as.logical(f_taut)))
}

# Contradiction should give FALSE
n_tests = n_tests + 1
f_contra = X %among% "a" & X %among% "b"
if (!isFALSE(as.logical(f_contra))) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: contradiction as.logical not FALSE (got %s)\n", as.logical(f_contra)))
}

# Non-trivial should give NA
n_tests = n_tests + 1
f_nontrivial = CnfFormula(list(as.CnfClause(X %among% "a")))
if (!is.na(as.logical(f_nontrivial))) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: non-trivial as.logical should be NA (got %s)\n", as.logical(f_nontrivial)))
}

# as.CnfFormula(TRUE) and as.CnfFormula(FALSE) roundtrip
n_tests = n_tests + 1
if (!isTRUE(as.logical(as.CnfFormula(TRUE)))) {
  n_failures = n_failures + 1
  cat("FAIL: as.CnfFormula(TRUE) -> as.logical should be TRUE\n")
}
n_tests = n_tests + 1
if (!isFALSE(as.logical(as.CnfFormula(FALSE)))) {
  n_failures = n_failures + 1
  cat("FAIL: as.CnfFormula(FALSE) -> as.logical should be FALSE\n")
}

cat(sprintf("  as.logical: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
