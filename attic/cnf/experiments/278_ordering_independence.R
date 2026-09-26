#!/usr/bin/env Rscript
# Test that different clause orderings produce semantically equivalent results.
# The simplifier is non-confluent, so structural results may differ, but
# semantic equivalence must hold.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Ordering independence ===\n")
set.seed(278001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  # Create the same formula with multiple orderings
  n_orderings = 5
  results = list()
  for (ord in 1:n_orderings) {
    perm = sample(length(clauses))
    r = tryCatch(CnfFormula(clauses[perm]), error = function(e) e)
    if (inherits(r, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [ord-%d-%d]: %s\n", trial, ord, r$message))
      break
    }
    results[[ord]] = r
  }
  if (length(results) < n_orderings) next

  # All results should be semantically equivalent
  truth1 = evaluate_formula(results[[1]], u)
  for (ord in 2:n_orderings) {
    n_tests = n_tests + 1
    truth_ord = evaluate_formula(results[[ord]], u)
    if (!all(truth1 == truth_ord)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [ord-%d-%d]: ordering %d differs from ordering 1\n", trial, ord, ord))
    }
  }
}
cat(sprintf("  Ordering independence: %d tests, %d failures\n", n_tests, n_failures))

# === Ordering with operations ===
cat("\n=== Ordering with AND/OR ===\n")
set.seed(278002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(2:4, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(sym_names, n_sym)
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

  # Test associativity: (f1 & f2) & f3 == f1 & (f2 & f3)
  r1 = tryCatch((f1 & f2) & f3, error = function(e) NULL)
  r2 = tryCatch(f1 & (f2 & f3), error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    t1 = evaluate_formula(r1, u)
    t2 = evaluate_formula(r2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [assoc-and-%d]: (f1&f2)&f3 != f1&(f2&f3)\n", trial))
    }
  }

  n_tests = n_tests + 1
  # Test commutativity: f1 & f2 == f2 & f1
  r1 = tryCatch(f1 & f2, error = function(e) NULL)
  r2 = tryCatch(f2 & f1, error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    t1 = evaluate_formula(r1, u)
    t2 = evaluate_formula(r2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [comm-and-%d]: f1&f2 != f2&f1\n", trial))
    }
  }

  n_tests = n_tests + 1
  # Test commutativity of OR: f1 | f2 == f2 | f1
  r1 = tryCatch(f1 | f2, error = function(e) NULL)
  r2 = tryCatch(f2 | f1, error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    t1 = evaluate_formula(r1, u)
    t2 = evaluate_formula(r2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [comm-or-%d]: f1|f2 != f2|f1\n", trial))
    }
  }
}
cat(sprintf("  Ordering with AND/OR: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
