#!/usr/bin/env Rscript
# Test |.CnfFormula distribution internals:
# - The swap optimization (shorter outer loop)
# - Tautology detection during distribution (line 349: universe coverage)
# - Large cross-products that get simplified down
# - Distribution + immediate simplification
# - Copy-on-modify correctness (e2 not corrupted)
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Distribution swap optimization ===
cat("=== Distribution swap ===\n")
set.seed(172001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function(n_cl) {
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  # Create asymmetric formulas: one short, one long
  f_short = make_f(1)
  f_long = make_f(sample(3:5, 1))
  if (is.null(f_short) || is.null(f_long)) next

  ts = evaluate_formula(f_short, u)
  tl = evaluate_formula(f_long, u)

  # f_short | f_long should equal f_long | f_short
  n_tests = n_tests + 1
  r1 = tryCatch(f_short | f_long, error = function(e) e)
  r2 = tryCatch(f_long | f_short, error = function(e) e)
  if (inherits(r1, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [swap1-%d]: %s\n", trial, r1$message)); next
  }
  if (inherits(r2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [swap2-%d]: %s\n", trial, r2$message)); next
  }
  tr1 = evaluate_formula(r1, u)
  tr2 = evaluate_formula(r2, u)
  expected = ts | tl
  if (!all(tr1 == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [swap1-%d]: short|long\n", trial))
  }
  if (!all(tr2 == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [swap2-%d]: long|short\n", trial))
  }
}
cat(sprintf("  Distribution swap: %d tests, %d failures\n", n_tests, n_failures))

# === Copy-on-modify: distribution doesn't corrupt inputs ===
cat("\n=== Copy-on-modify ===\n")
set.seed(172002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(2:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  # Record truth tables before operation
  t1_before = evaluate_formula(f1, u)
  t2_before = evaluate_formula(f2, u)

  # Perform OR distribution
  r = tryCatch(f1 | f2, error = function(e) NULL)

  # Check that f1 and f2 are unchanged
  n_tests = n_tests + 1
  t1_after = evaluate_formula(f1, u)
  t2_after = evaluate_formula(f2, u)
  if (!all(t1_after == t1_before)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [copy-f1-%d]: f1 changed after f1|f2\n", trial))
  }
  if (!all(t2_after == t2_before)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [copy-f2-%d]: f2 changed after f1|f2\n", trial))
  }

  # Also test AND
  r2 = tryCatch(f1 & f2, error = function(e) NULL)
  n_tests = n_tests + 1
  t1_after2 = evaluate_formula(f1, u)
  t2_after2 = evaluate_formula(f2, u)
  if (!all(t1_after2 == t1_before)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [copy-and-f1-%d]\n", trial))
  }
  if (!all(t2_after2 == t2_before)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [copy-and-f2-%d]\n", trial))
  }
}
cat(sprintf("  Copy-on-modify: %d tests, %d failures\n", n_tests, n_failures))

# === Tautology during distribution ===
cat("\n=== Tautology during distribution ===\n")
set.seed(172003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create formulas where OR distribution produces tautological clauses
  # (symbol range covers full domain after union)
  clauses1 = list(
    as.CnfClause(A %among% sample(dom, 2)),
    as.CnfClause(B %among% sample(dom, sample(1:2, 1)))
  )
  clauses2 = list(
    as.CnfClause(A %among% sample(dom, 2)),
    as.CnfClause(C %among% sample(dom, sample(1:2, 1)))
  )
  clauses1 = clauses1[!sapply(clauses1, function(x) isTRUE(unclass(x)))]
  clauses2 = clauses2[!sapply(clauses2, function(x) isTRUE(unclass(x)))]
  if (length(clauses1) < 1 || length(clauses2) < 1) next

  f1 = tryCatch(CnfFormula(clauses1), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(clauses2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  n_tests = n_tests + 1
  r = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(r, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [taut-%d]: %s\n", trial, r$message)); next
  }
  tr = evaluate_formula(r, u)
  if (!all(tr == (t1 | t2))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [taut-%d]\n", trial))
  }
}
cat(sprintf("  Tautology during distribution: %d tests, %d failures\n", n_tests, n_failures))

# === Multi-formula chained OR ===
cat("\n=== Chained OR ===\n")
set.seed(172004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)

  # (f1 | f2) | f3 vs f1 | (f2 | f3) - associativity
  n_tests = n_tests + 1
  r1 = tryCatch((f1 | f2) | f3, error = function(e) NULL)
  r2 = tryCatch(f1 | (f2 | f3), error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    tr1 = evaluate_formula(r1, u)
    tr2 = evaluate_formula(r2, u)
    expected = t1 | t2 | t3
    if (!all(tr1 == expected)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [assoc1-%d]\n", trial))
    }
    if (!all(tr2 == expected)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [assoc2-%d]\n", trial))
    }
  }

  # Distributivity: f1 | (f2 & f3) = (f1 | f2) & (f1 | f3)
  n_tests = n_tests + 1
  lhs = tryCatch(f1 | (f2 & f3), error = function(e) NULL)
  rhs = tryCatch((f1 | f2) & (f1 | f3), error = function(e) NULL)
  if (!is.null(lhs) && !is.null(rhs)) {
    tlhs = evaluate_formula(lhs, u)
    trhs = evaluate_formula(rhs, u)
    expected = t1 | (t2 & t3)
    if (!all(tlhs == expected)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [dist-lhs-%d]\n", trial))
    }
    if (!all(trhs == expected)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [dist-rhs-%d]\n", trial))
    }
  }
}
cat(sprintf("  Chained OR: %d tests, %d failures\n", n_tests, n_failures))

# === OR with single-clause formulas (no distribution needed) ===
cat("\n=== Single-clause OR ===\n")
set.seed(172005)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Two single-clause formulas
  n_sym1 = sample(1:3, 1)
  chosen1 = sample(names(syms), n_sym1)
  atoms1 = lapply(chosen1, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  cl1 = as.CnfClause(Reduce(`|`, atoms1))

  n_sym2 = sample(1:3, 1)
  chosen2 = sample(names(syms), n_sym2)
  atoms2 = lapply(chosen2, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  cl2 = as.CnfClause(Reduce(`|`, atoms2))

  if (isTRUE(unclass(cl1)) || isTRUE(unclass(cl2))) next
  if (isFALSE(unclass(cl1)) || isFALSE(unclass(cl2))) next

  f1 = as.CnfFormula(cl1)
  f2 = as.CnfFormula(cl2)

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  n_tests = n_tests + 1
  r = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(r, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [single-%d]: %s\n", trial, r$message)); next
  }
  tr = evaluate_formula(r, u)
  if (!all(tr == (t1 | t2))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [single-%d]\n", trial))
  }
}
cat(sprintf("  Single-clause OR: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
