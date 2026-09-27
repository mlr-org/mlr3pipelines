#!/usr/bin/env Rscript
# Test the c() operator on CnfFormula objects and how &.CnfFormula
# uses it internally. Also test various ways formulas can be combined
# and reconstructed.
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

# === Pattern 1: AND associativity: (f1 & f2) & f3 == f1 & (f2 & f3) ===
cat("=== AND associativity ===\n")
set.seed(233001)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (any(sapply(list(f1, f2, f3), is.null))) next

  n_tests = n_tests + 1

  lhs = tryCatch((f1 & f2) & f3, error = function(e) e)
  rhs = tryCatch(f1 & (f2 & f3), error = function(e) e)

  if (inherits(lhs, "error") || inherits(rhs, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [assoc-%d]: error\n", trial)); next
  }

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [assoc-%d]: AND not associative\n", trial))
  }
}
cat(sprintf("  AND associativity: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: OR associativity: (f1 | f2) | f3 == f1 | (f2 | f3) ===
cat("\n=== OR associativity ===\n")
set.seed(233002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (any(sapply(list(f1, f2, f3), is.null))) next

  n_tests = n_tests + 1

  lhs = tryCatch((f1 | f2) | f3, error = function(e) e)
  rhs = tryCatch(f1 | (f2 | f3), error = function(e) e)

  if (inherits(lhs, "error") || inherits(rhs, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [orassoc-%d]: error\n", trial)); next
  }

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [orassoc-%d]: OR not associative\n", trial))
  }
}
cat(sprintf("  OR associativity: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Distributivity: f1 & (f2 | f3) == (f1 & f2) | (f1 & f3) ===
cat("\n=== Distributivity ===\n")
set.seed(233003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (any(sapply(list(f1, f2, f3), is.null))) next

  n_tests = n_tests + 1

  # f1 & (f2 | f3) vs (f1 & f2) | (f1 & f3)
  lhs = tryCatch(f1 & (f2 | f3), error = function(e) e)
  rhs = tryCatch((f1 & f2) | (f1 & f3), error = function(e) e)

  if (inherits(lhs, "error") || inherits(rhs, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dist-%d]: error\n", trial)); next
  }

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dist-%d]: distributivity fails\n", trial))
  }
}
cat(sprintf("  Distributivity: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Incremental AND vs batch construction ===
cat("\n=== Incremental vs batch ===\n")
set.seed(233004)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
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

  n_tests = n_tests + 1

  # Batch
  f_batch = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f_batch, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [batch-%d]: %s\n", trial, f_batch$message)); next
  }

  # Incremental
  f_inc = tryCatch({
    acc = CnfFormula(clauses[1:2])
    for (i in 3:length(clauses)) {
      acc = acc & CnfFormula(list(clauses[[i]]))
    }
    acc
  }, error = function(e) e)
  if (inherits(f_inc, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [inc-%d]: %s\n", trial, f_inc$message)); next
  }

  t_batch = evaluate_formula(f_batch, u)
  t_inc = evaluate_formula(f_inc, u)
  if (!all(t_batch == t_inc)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [batchinc-%d]: batch vs incremental mismatch\n", trial))
  }
}
cat(sprintf("  Incremental vs batch: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
