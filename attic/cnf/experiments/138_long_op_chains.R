#!/usr/bin/env Rscript
# Test long chains of operations (10+ chained AND/OR/NOT).
# Verify that repeated operations maintain semantic correctness.
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

make_simple_formula = function(u, syms, dom) {
  n_cl = sample(1:2, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:min(2, length(syms)), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:(length(dom) - 1), 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) == 0) return(NULL)
  tryCatch(CnfFormula(clauses), error = function(e) NULL)
}

# === Long AND chains ===
cat("=== Long AND chains ===\n")
set.seed(138001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  n_formulas = sample(5:15, 1)
  formulas = lapply(1:n_formulas, function(j) make_simple_formula(u, syms, dom))
  formulas = formulas[!sapply(formulas, is.null)]
  if (length(formulas) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(Reduce(`&`, formulas), error = function(e) NULL)
  if (is.null(result)) next

  # Build expected truth
  truths = lapply(formulas, evaluate_formula, universe = u)
  expected = Reduce(`&`, truths)
  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [and-chain-%d]: AND chain of %d formulas\n", trial, length(formulas)))
  }
}
cat(sprintf("  AND chains: %d tests, %d failures\n", n_tests, n_failures))

# === Long OR chains ===
cat("\n=== Long OR chains ===\n")
set.seed(138002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  n_formulas = sample(3:8, 1)  # OR chains grow fast due to distribution
  formulas = lapply(1:n_formulas, function(j) make_simple_formula(u, syms, dom))
  formulas = formulas[!sapply(formulas, is.null)]
  if (length(formulas) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(Reduce(`|`, formulas), error = function(e) NULL)
  if (is.null(result)) next

  truths = lapply(formulas, evaluate_formula, universe = u)
  expected = Reduce(`|`, truths)
  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [or-chain-%d]: OR chain of %d formulas\n", trial, length(formulas)))
  }
}
cat(sprintf("  OR chains: %d tests, %d failures\n", n_tests, n_failures))

# === Alternating AND/OR chains ===
cat("\n=== Alternating AND/OR chains ===\n")
set.seed(138003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  n_formulas = sample(4:10, 1)
  formulas = lapply(1:n_formulas, function(j) make_simple_formula(u, syms, dom))
  formulas = formulas[!sapply(formulas, is.null)]
  if (length(formulas) < 4) next

  n_tests = n_tests + 1
  result = formulas[[1]]
  truths = list(evaluate_formula(formulas[[1]], u))
  for (i in 2:length(formulas)) {
    op = if (i %% 2 == 0) `&` else `|`
    result = tryCatch(op(result, formulas[[i]]), error = function(e) NULL)
    if (is.null(result)) break
    truth_i = evaluate_formula(formulas[[i]], u)
    truths[[i]] = truth_i
  }
  if (is.null(result)) next

  # Rebuild expected
  expected = truths[[1]]
  for (i in 2:length(truths)) {
    if (is.null(truths[[i]])) break
    op = if (i %% 2 == 0) `&` else `|`
    expected = op(expected, truths[[i]])
  }

  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [altchain-%d]: alternating chain of %d formulas\n", trial, length(formulas)))
  }
}
cat(sprintf("  Alternating chains: %d tests, %d failures\n", n_tests, n_failures))

# === Deep negation chains ===
cat("\n=== Deep negation chains ===\n")
set.seed(138004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  f = make_simple_formula(u, syms, dom)
  if (is.null(f)) next

  n_negs = sample(2:8, 1)
  n_tests = n_tests + 1
  current = f
  for (i in 1:n_negs) {
    current = tryCatch(!current, error = function(e) NULL)
    if (is.null(current)) break
  }
  if (is.null(current)) next

  expected_truth = evaluate_formula(f, u)
  if (n_negs %% 2 == 1) expected_truth = !expected_truth
  actual_truth = evaluate_formula(current, u)
  if (!all(actual_truth == expected_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [negchain-%d]: %d negations\n", trial, n_negs))
  }
}
cat(sprintf("  Negation chains: %d tests, %d failures\n", n_tests, n_failures))

# === Mixed operation trees ===
cat("\n=== Mixed operation trees ===\n")
set.seed(138005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("x", "y", "z")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Build 4-6 base formulas
  n_base = sample(4:6, 1)
  base_fs = lapply(1:n_base, function(j) make_simple_formula(u, syms, dom))
  base_fs = base_fs[!sapply(base_fs, is.null)]
  if (length(base_fs) < 3) next

  # Apply random operations to build a tree
  n_ops = sample(5:12, 1)
  n_tests = n_tests + 1
  pool = base_fs
  pool_truths = lapply(pool, evaluate_formula, universe = u)

  success = TRUE
  for (i in 1:n_ops) {
    op = sample(c("and", "or", "neg"), 1)
    if (op == "neg") {
      idx = sample(length(pool), 1)
      new_f = tryCatch(!pool[[idx]], error = function(e) NULL)
      if (is.null(new_f)) { success = FALSE; break }
      pool[[length(pool) + 1]] = new_f
      pool_truths[[length(pool_truths) + 1]] = !pool_truths[[idx]]
    } else {
      if (length(pool) < 2) { success = FALSE; break }
      idx = sample(length(pool), 2)
      op_fn = if (op == "and") `&` else `|`
      new_f = tryCatch(op_fn(pool[[idx[1]]], pool[[idx[2]]]), error = function(e) NULL)
      if (is.null(new_f)) { success = FALSE; break }
      pool[[length(pool) + 1]] = new_f
      pool_truths[[length(pool_truths) + 1]] = op_fn(pool_truths[[idx[1]]], pool_truths[[idx[2]]])
    }
  }
  if (!success) next

  # Verify the final formula
  final_f = pool[[length(pool)]]
  actual = evaluate_formula(final_f, u)
  expected = pool_truths[[length(pool_truths)]]
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [tree-%d]: mixed op tree with %d ops\n", trial, n_ops))
  }
}
cat(sprintf("  Mixed op trees: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
