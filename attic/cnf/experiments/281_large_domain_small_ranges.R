#!/usr/bin/env Rscript
# Large domain (8-10 values) with very small ranges (1-2 values per atom).
# This creates many opportunities for SSE since ranges are very specific,
# and HLA complement additions are large.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Large domain small ranges ===\n")
set.seed(281001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = letters[1:sample(7:10, 1)]
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ldsm-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ldsm-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Large domain small ranges: %d tests, %d failures\n", n_tests, n_failures))

# === Large domain, operations ===
cat("\n=== Large domain operations ===\n")
set.seed(281002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = letters[1:8]
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(2:4, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  ops = list(
    list(fn = function() f1 & f2, expected = t1 & t2, name = "AND"),
    list(fn = function() f1 | f2, expected = t1 | t2, name = "OR"),
    list(fn = function() !f1, expected = !t1, name = "NOT")
  )

  for (op in ops) {
    n_tests = n_tests + 1
    r = tryCatch(op$fn(), error = function(e) NULL)
    if (!is.null(r) && !all(evaluate_formula(r, u) == op$expected)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [ldop-%d]: %s mismatch\n", trial, op$name))
    }
  }
}
cat(sprintf("  Large domain operations: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
