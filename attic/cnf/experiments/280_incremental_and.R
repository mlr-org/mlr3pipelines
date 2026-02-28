#!/usr/bin/env Rscript
# Test incremental AND: f1 & f2 & f3 ... built one clause at a time.
# Compare against building the formula all at once.
# This exercises the `&.CnfFormula` operator which concatenates then re-simplifies.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Incremental AND vs batch ===\n")
set.seed(280001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:7, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  # Build incrementally
  incremental = tryCatch({
    f = CnfFormula(list(clauses[[1]]))
    for (j in 2:length(clauses)) {
      f = f & CnfFormula(list(clauses[[j]]))
    }
    f
  }, error = function(e) e)

  # Build all at once
  batch = tryCatch(CnfFormula(clauses), error = function(e) e)

  if (inherits(incremental, "error") || inherits(batch, "error")) next

  n_tests = n_tests + 1
  t_incr = evaluate_formula(incremental, u)
  t_batch = evaluate_formula(batch, u)
  if (!all(t_incr == t_batch)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [incr-%d]: incremental AND differs from batch\n", trial))
  }

  # Also check against raw evaluation
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(t_batch == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [incr-raw-%d]: batch differs from raw\n", trial))
  }
}
cat(sprintf("  Incremental AND vs batch: %d tests, %d failures\n", n_tests, n_failures))

# === Incremental OR ===
cat("\n=== Incremental OR vs batch ===\n")
set.seed(280002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create 3 small formulas and OR them
  formulas = list()
  for (k in 1:3) {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) break
    f = tryCatch(CnfFormula(cls), error = function(e) NULL)
    if (is.null(f)) break
    formulas[[k]] = f
  }
  if (length(formulas) < 3) next

  n_tests = n_tests + 1

  # (f1 | f2) | f3
  r1 = tryCatch((formulas[[1]] | formulas[[2]]) | formulas[[3]], error = function(e) NULL)
  # f1 | (f2 | f3)
  r2 = tryCatch(formulas[[1]] | (formulas[[2]] | formulas[[3]]), error = function(e) NULL)

  if (is.null(r1) || is.null(r2)) next

  t1 = evaluate_formula(r1, u)
  t2 = evaluate_formula(r2, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [incror-%d]: OR associativity failed\n", trial))
  }

  # Check against truth table
  expected = evaluate_formula(formulas[[1]], u) | evaluate_formula(formulas[[2]], u) | evaluate_formula(formulas[[3]], u)
  if (!all(t1 == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [incror-truth-%d]: OR truth mismatch\n", trial))
  }
}
cat(sprintf("  Incremental OR vs batch: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
