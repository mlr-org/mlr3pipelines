#!/usr/bin/env Rscript
# |.CnfFormula distribution with many symbols per clause:
# Distribution creates clauses with the union of symbols from both operands.
# This tests the case where distribution creates very wide clauses,
# potentially creating tautologies during distribution.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

evaluate_raw_clauses = function(clauses, universe) {
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }
  raw_truth
}

# === Pattern 1: Wide clauses OR'd together ===
cat("=== Wide clause distribution ===\n")
set.seed(205001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  E = CnfSymbol(u, "E", dom)
  syms = list(A = A, B = B, C = C, D = D, E = E)

  # f1: clauses with symbols {A, B, C}
  make_abc = function() {
    atoms = lapply(c("A", "B", "C"), function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  }
  # f2: clauses with symbols {C, D, E}
  make_cde = function() {
    atoms = lapply(c("C", "D", "E"), function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  }

  cls1 = lapply(1:sample(1:3, 1), function(j) make_abc())
  cls1 = cls1[!sapply(cls1, function(x) isTRUE(unclass(x)))]
  cls2 = lapply(1:sample(1:3, 1), function(j) make_cde())
  cls2 = cls2[!sapply(cls2, function(x) isTRUE(unclass(x)))]
  if (length(cls1) < 1 || length(cls2) < 1) next

  f1 = tryCatch(CnfFormula(cls1), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(cls2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  # Distribution creates clauses with up to 5 symbols
  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [wide-%d]: %s\n", trial, result$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  expected = t_f1 | t_f2
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [wide-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Wide clause distribution: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Distribution creating near-tautological clauses ===
cat("\n=== Near-tautological distribution ===\n")
set.seed(205002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # f1: clauses with wide A ranges
  cls1 = lapply(1:sample(2:3, 1), function(j) {
    a_range = sample(dom, 2)  # 2 out of 3: nearly full
    b_range = sample(dom, sample(1:2, 1))
    as.CnfClause(A %among% a_range | B %among% b_range)
  })
  cls1 = cls1[!sapply(cls1, function(x) isTRUE(unclass(x)))]

  # f2: clauses where A has complementary ranges
  cls2 = lapply(1:sample(2:3, 1), function(j) {
    a_range = sample(dom, 2)
    c_range = sample(dom, sample(1:2, 1))
    as.CnfClause(A %among% a_range | C %among% c_range)
  })
  cls2 = cls2[!sapply(cls2, function(x) isTRUE(unclass(x)))]

  if (length(cls1) < 1 || length(cls2) < 1) next
  f1 = tryCatch(CnfFormula(cls1), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(cls2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neartaut-%d]: %s\n", trial, result$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  expected = t_f1 | t_f2
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [neartaut-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Near-tautological: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Distribution where both sides have many clauses ===
cat("\n=== Many x many distribution ===\n")
set.seed(205003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_cls = function(n) {
    cls = lapply(1:n, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  }

  cls1 = make_cls(sample(3:5, 1))
  cls2 = make_cls(sample(3:5, 1))
  if (length(cls1) < 2 || length(cls2) < 2) next

  f1 = tryCatch(CnfFormula(cls1), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(cls2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mxm-%d]: %s\n", trial, result$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  expected = t_f1 | t_f2
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mxm-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Many x many: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Distribution with overlapping symbols creating tautology ===
cat("\n=== Shared symbol tautology ===\n")
set.seed(205004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  # f1 has (A in r1) clauses
  # f2 has (A in comp(r1) | B in ...) clauses
  # Distribution of f1 | f2 should create tautologies on A
  a_range1 = sample(dom, 2)
  a_comp1 = setdiff(dom, a_range1)

  cls1 = list(as.CnfClause(A %among% a_range1))
  cls2 = list(
    as.CnfClause(A %among% a_comp1 | B %among% sample(dom, sample(1:3, 1)))
  )

  # Add more clauses
  for (j in 1:sample(1:3, 1)) {
    cl = as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1)))
    if (!isTRUE(unclass(cl))) cls1[[length(cls1) + 1]] = cl
  }
  for (j in 1:sample(1:3, 1)) {
    cl = as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1)))
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
    cat(sprintf("ERROR [shtaut-%d]: %s\n", trial, result$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  expected = t_f1 | t_f2
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [shtaut-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Shared symbol tautology: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
