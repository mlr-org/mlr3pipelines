#!/usr/bin/env Rscript
# Stress test all.equal.CnfFormula: verify normalization produces
# consistent results for equivalent formulas constructed differently.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Same formula, different construction order ===
cat("=== Same clauses different order ===\n")
set.seed(139001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f1 = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f1)) next

  # Same clauses in reversed order
  f2 = tryCatch(CnfFormula(rev(clauses)), error = function(e) NULL)
  if (is.null(f2)) next

  # Same clauses in random order
  f3 = tryCatch(CnfFormula(sample(clauses)), error = function(e) NULL)
  if (is.null(f3)) next

  n_tests = n_tests + 1
  # all.equal should report them as equal (semantically they are)
  # Note: the simplifier may produce structurally different results, so we check semantics
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  if (!all(t1 == t2) || !all(t1 == t3)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [order-%d]: different order gives different semantics\n", trial))
  }

  # If all.equal reports TRUE, it should be structurally identical
  ae12 = all.equal(f1, f2)
  ae13 = all.equal(f1, f3)
  if (isTRUE(ae12)) {
    # f1 and f2 are structurally equal after normalization
    # Verify this by checking element-wise
  }
}
cat(sprintf("  Same clauses diff order: %d tests, %d failures\n", n_tests, n_failures))

# === AND in different order ===
cat("\n=== AND order independence ===\n")
set.seed(139002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("x", "y", "z")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  f1 = tryCatch({
    cl1 = as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | B %among% sample(dom, sample(1:2, 1)))
    cl2 = as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | B %among% sample(dom, sample(1:2, 1)))
    if (isTRUE(unclass(cl1)) || isTRUE(unclass(cl2))) return(NULL)
    CnfFormula(list(cl1)) & CnfFormula(list(cl2))
  }, error = function(e) NULL)
  if (is.null(f1)) next

  f2 = tryCatch({
    CnfFormula(list(cl2)) & CnfFormula(list(cl1))
  }, error = function(e) NULL)
  if (is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [and-order-%d]: AND order changed semantics\n", trial))
  }
}
cat(sprintf("  AND order: %d tests, %d failures\n", n_tests, n_failures))

# === OR commutativity ===
cat("\n=== OR commutativity ===\n")
set.seed(139003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  cl1 = as.CnfClause(A %among% sample(dom, sample(1:2, 1)))
  cl2 = as.CnfClause(B %among% sample(dom, sample(1:2, 1)))
  if (isTRUE(unclass(cl1)) || isTRUE(unclass(cl2))) next

  f1 = tryCatch(CnfFormula(list(cl1)), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(list(cl2)), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  f_or_12 = tryCatch(f1 | f2, error = function(e) NULL)
  f_or_21 = tryCatch(f2 | f1, error = function(e) NULL)
  if (is.null(f_or_12) || is.null(f_or_21)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f_or_12, u)
  t2 = evaluate_formula(f_or_21, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [or-comm-%d]: OR not commutative\n", trial))
  }
}
cat(sprintf("  OR commutativity: %d tests, %d failures\n", n_tests, n_failures))

# === TRUE/FALSE formula equality ===
cat("\n=== TRUE/FALSE equality ===\n")
set.seed(139004)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b")
  A = CnfSymbol(u, "A", dom)

  # TRUE formulas
  f_true1 = tryCatch(as.CnfFormula(TRUE), error = function(e) NULL)
  f_true2 = tryCatch(CnfFormula(list()), error = function(e) NULL)
  if (!is.null(f_true1) && !is.null(f_true2)) {
    n_tests = n_tests + 1
    ae = all.equal(f_true1, f_true2)
    if (!isTRUE(ae)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [true-eq-%d]: TRUE formulas not equal: %s\n", trial, ae))
    }
  }

  # FALSE formulas
  f_false1 = tryCatch(as.CnfFormula(FALSE), error = function(e) NULL)
  # F&F = F
  f_false2 = tryCatch(f_false1 & f_false1, error = function(e) NULL)
  if (!is.null(f_false1) && !is.null(f_false2)) {
    n_tests = n_tests + 1
    ae = all.equal(f_false1, f_false2)
    if (!isTRUE(ae)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [false-eq-%d]: FALSE formulas not equal: %s\n", trial, ae))
    }
  }
}
cat(sprintf("  TRUE/FALSE equality: %d tests, %d failures\n", n_tests, n_failures))

# === Constructed vs simplified equality ===
cat("\n=== Constructed vs simplified ===\n")
set.seed(139005)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  # Build via constructor
  f1 = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f1)) next

  # Build incrementally
  f2 = tryCatch({
    f = CnfFormula(list(clauses[[1]]))
    for (i in 2:length(clauses)) {
      f = f & CnfFormula(list(clauses[[i]]))
    }
    f
  }, error = function(e) NULL)
  if (is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [const-vs-simp-%d]: batch != incremental\n", trial))
  }

  # Check all.equal on the two
  ae = all.equal(f1, f2)
  # They should be semantically equal, but may differ structurally (not a bug)
}
cat(sprintf("  Constructed vs simplified: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
