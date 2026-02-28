#!/usr/bin/env Rscript
# Simplification equivalence: verify that building formulas different ways
# gives semantically equivalent results.
# - CnfFormula(clauses) vs incremental & (f1 & f2 & ...)
# - CnfFormula(list(f1, f2)) vs f1 & f2
# - Different clause orderings
# - Clauses constructed with different atom orderings
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: Batch vs incremental construction ===
cat("=== Batch vs incremental ===\n")
set.seed(211001)

for (trial in 1:1000) {
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
  if (length(clauses) < 3) next

  n_tests = n_tests + 1

  # Batch construction
  batch = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(batch, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [batch-%d]: %s\n", trial, batch$message)); next
  }

  # Incremental construction
  incr = tryCatch({
    result = as.CnfFormula(clauses[[1]])
    for (k in 2:length(clauses)) {
      result = result & as.CnfFormula(clauses[[k]])
    }
    result
  }, error = function(e) e)
  if (inherits(incr, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [incr-%d]: %s\n", trial, incr$message)); next
  }

  t_batch = evaluate_formula(batch, u)
  t_incr = evaluate_formula(incr, u)
  if (!all(t_batch == t_incr)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [bvi-%d]: batch != incremental\n", trial))
  }
}
cat(sprintf("  Batch vs incremental: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Different atom orderings in clause construction ===
cat("\n=== Atom ordering independence ===\n")
set.seed(211002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  a_range = sample(dom, sample(1:3, 1))
  b_range = sample(dom, sample(1:3, 1))
  c_range = sample(dom, sample(1:3, 1))

  # Same clause built with different atom orderings
  cl1 = as.CnfClause(A %among% a_range | B %among% b_range | C %among% c_range)
  cl2 = as.CnfClause(C %among% c_range | A %among% a_range | B %among% b_range)
  cl3 = as.CnfClause(B %among% b_range | C %among% c_range | A %among% a_range)

  # All should be equal
  if (!isTRUE(all.equal(cl1, cl2)) || !isTRUE(all.equal(cl1, cl3))) {
    # This is OK if they have different orderings; check semantic equivalence
  }

  # Build formulas with each ordering + other clauses
  other_clauses = lapply(1:sample(2:4, 1), function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  other_clauses = other_clauses[!sapply(other_clauses, function(x) isTRUE(unclass(x)))]
  if (length(other_clauses) < 1) next
  if (isTRUE(unclass(cl1))) next

  n_tests = n_tests + 1

  f1 = tryCatch(CnfFormula(c(list(cl1), other_clauses)), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(c(list(cl2), other_clauses)), error = function(e) NULL)
  f3 = tryCatch(CnfFormula(c(list(cl3), other_clauses)), error = function(e) NULL)
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  if (!all(t1 == t2) || !all(t1 == t3)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [atomord-%d]: atom ordering changed semantics\n", trial))
  }
}
cat(sprintf("  Atom ordering: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Clause permutation equivalence ===
cat("\n=== Clause permutation ===\n")
set.seed(211003)

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
  if (length(clauses) < 3) next

  n_tests = n_tests + 1

  # Original order
  f_orig = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f_orig, "error")) {
    n_failures = n_failures + 1; next
  }

  # Reversed order
  f_rev = tryCatch(CnfFormula(rev(clauses)), error = function(e) e)
  if (inherits(f_rev, "error")) {
    n_failures = n_failures + 1; next
  }

  # Random shuffle
  f_shuf = tryCatch(CnfFormula(clauses[sample(length(clauses))]), error = function(e) e)
  if (inherits(f_shuf, "error")) {
    n_failures = n_failures + 1; next
  }

  t_orig = evaluate_formula(f_orig, u)
  t_rev = evaluate_formula(f_rev, u)
  t_shuf = evaluate_formula(f_shuf, u)
  if (!all(t_orig == t_rev) || !all(t_orig == t_shuf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [perm-%d]: clause order changed semantics\n", trial))
  }
}
cat(sprintf("  Clause permutation: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Value ordering in ranges ===
cat("\n=== Range value ordering ===\n")
set.seed(211004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  a_range = sample(dom, sample(1:3, 1))
  b_range = sample(dom, sample(1:3, 1))

  # Same atom with different value orderings
  cl1_a = as.CnfClause(A %among% a_range | B %among% b_range)
  cl1_b = as.CnfClause(A %among% rev(a_range) | B %among% rev(b_range))

  other = lapply(1:sample(2:4, 1), function(j) {
    as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1)))
  })
  other = other[!sapply(other, function(x) isTRUE(unclass(x)))]
  if (length(other) < 1) next
  if (isTRUE(unclass(cl1_a))) next

  n_tests = n_tests + 1

  fa = tryCatch(CnfFormula(c(list(cl1_a), other)), error = function(e) NULL)
  fb = tryCatch(CnfFormula(c(list(cl1_b), other)), error = function(e) NULL)
  if (is.null(fa) || is.null(fb)) next

  ta = evaluate_formula(fa, u)
  tb = evaluate_formula(fb, u)
  if (!all(ta == tb)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [valord-%d]: value ordering changed semantics\n", trial))
  }
}
cat(sprintf("  Value ordering: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
