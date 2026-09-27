#!/usr/bin/env Rscript
# OR distribution internal mechanics:
# - The `unique()` call when merging ranges
# - The swap optimization (shorter formula goes first)
# - Tautology detection per-symbol during distribution
# - Very asymmetric formulas (1 clause | many clauses)
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: Very asymmetric OR (1 vs many) ===
cat("=== Asymmetric 1 vs many ===\n")
set.seed(213001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # 1 clause on one side
  a1 = sample(dom, sample(1:3, 1))
  b1 = sample(dom, sample(1:3, 1))
  f1 = tryCatch(CnfFormula(list(as.CnfClause(A %among% a1 | B %among% b1))),
    error = function(e) NULL)
  if (is.null(f1)) next

  # Many clauses on other side
  cls = lapply(1:sample(3:6, 1), function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 2) next
  f2 = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f2)) next

  n_tests = n_tests + 1
  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [asym-%d]: %s\n", trial, result$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  expected = t_f1 | t_f2
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [asym-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Asymmetric OR: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: OR where all distributed clauses are tautologies ===
cat("\n=== All-tautology distribution ===\n")
set.seed(213002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  # f1: (A in r1) and (A in r2) -> A must be in intersection
  # f2: (A in complement of intersection) -> makes f1|f2 always TRUE on A
  r1 = sample(dom, 2)
  r2 = sample(dom, 2)
  isct = intersect(r1, r2)
  if (length(isct) == 0) next  # would be FALSE
  comp = setdiff(dom, isct)
  if (length(comp) == 0) next  # complement empty

  # f1 simplifies to (A in isct)
  cls1 = list(
    as.CnfClause(A %among% r1),
    as.CnfClause(A %among% r2)
  )
  f1 = tryCatch(CnfFormula(cls1), error = function(e) NULL)
  if (is.null(f1)) next

  # f2: (A in comp | B in some_range)
  b_range = sample(dom, sample(1:2, 1))
  cls2 = list(as.CnfClause(A %among% comp | B %among% b_range))
  f2 = tryCatch(CnfFormula(cls2), error = function(e) NULL)
  if (is.null(f2)) next

  n_tests = n_tests + 1
  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [alltaut-%d]: %s\n", trial, result$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  expected = t_f1 | t_f2
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [alltaut-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  All-tautology distribution: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: OR with duplicate range values (unique() test) ===
cat("\n=== Duplicate range values ===\n")
set.seed(213003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # f1 and f2 have overlapping A ranges
  shared_a = sample(dom, 1)
  f1_a = unique(c(shared_a, sample(dom, sample(1:2, 1))))
  f2_a = unique(c(shared_a, sample(dom, sample(1:2, 1))))

  cls1 = list(
    as.CnfClause(A %among% f1_a | B %among% sample(dom, sample(1:3, 1)))
  )
  cls2 = list(
    as.CnfClause(A %among% f2_a | C %among% sample(dom, sample(1:3, 1)))
  )

  # Add more clauses
  for (j in 1:sample(1:3, 1)) {
    cl = as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1)))
    if (!isTRUE(unclass(cl))) cls1[[length(cls1) + 1]] = cl
  }
  for (j in 1:sample(1:3, 1)) {
    cl = as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1)))
    if (!isTRUE(unclass(cl))) cls2[[length(cls2) + 1]] = cl
  }

  cls1 = cls1[!sapply(cls1, function(x) isTRUE(unclass(x)))]
  cls2 = cls2[!sapply(cls2, function(x) isTRUE(unclass(x)))]
  if (length(cls1) < 1 || length(cls2) < 1) next

  f1 = tryCatch(CnfFormula(cls1), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(cls2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [duprange-%d]: %s\n", trial, result$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  expected = t_f1 | t_f2
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [duprange-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Duplicate ranges: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Triple OR chain ===
cat("\n=== Triple OR chain ===\n")
set.seed(213004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
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

  n_tests = n_tests + 1

  result = tryCatch((f1 | f2) | f3, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [triple-%d]: %s\n", trial, result$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  t_f3 = evaluate_formula(f3, u)
  expected = (t_f1 | t_f2) | t_f3

  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [triple-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Triple OR chain: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
