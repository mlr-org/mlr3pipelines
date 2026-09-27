#!/usr/bin/env Rscript
# Test formulas where one symbol appears in every clause (dense sharing).
# This stresses the symbol_registry which tracks which clauses use each symbol.
# Unit propagation for such a symbol affects ALL clauses, and HLA has many
# candidates when all clauses share a symbol.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === All clauses share one symbol ===
cat("=== All share V1 ===\n")
set.seed(260001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    # V1 always present, plus 1-3 others
    n_extra = sample(1:3, 1)
    extra = sample(sym_names[-1], n_extra)
    chosen = c("V1", extra)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [share1-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [share1-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  All share V1: %d tests, %d failures\n", n_tests, n_failures))

# === All clauses share V1, plus unit for V1 ===
cat("\n=== Shared symbol + unit ===\n")
set.seed(260002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Start with a unit for V1
  v1_range = sample(dom, sample(1:2, 1))
  unit_cl = as.CnfClause(syms[["V1"]] %among% v1_range)

  # Additional clauses all using V1
  extra_clauses = lapply(1:sample(3:6, 1), function(j) {
    n_extra = sample(1:2, 1)
    extra = sample(sym_names[-1], n_extra)
    chosen = c("V1", extra)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = c(list(unit_cl), extra_clauses)
  # Shuffle to vary processing order
  clauses = clauses[sample(length(clauses))]
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [shunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [shunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Shared symbol + unit: %d tests, %d failures\n", n_tests, n_failures))

# === Multiple shared symbols ===
cat("\n=== Multiple shared symbols ===\n")
set.seed(260003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # V1 and V2 appear in every clause
  n_cl = sample(3:7, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_extra = sample(0:2, 1)
    extra = if (n_extra > 0) sample(sym_names[3:5], n_extra) else character(0)
    chosen = c("V1", "V2", extra)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multish-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multish-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multiple shared symbols: %d tests, %d failures\n", n_tests, n_failures))

# === Dense sharing with operations ===
cat("\n=== Dense sharing operations ===\n")
set.seed(260004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(2:4, 1)
    cls = lapply(1:n_cl, function(j) {
      # All clauses use V1
      n_extra = sample(1:2, 1)
      extra = sample(sym_names[-1], n_extra)
      chosen = c("V1", extra)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  # Test all operations
  r_and = tryCatch(f1 & f2, error = function(e) NULL)
  r_or = tryCatch(f1 | f2, error = function(e) NULL)
  r_not = tryCatch(!f1, error = function(e) NULL)

  fail = FALSE
  if (!is.null(r_and) && !all(evaluate_formula(r_and, u) == (t1 & t2))) {
    fail = TRUE; cat(sprintf("FAIL [denseop-%d]: AND mismatch\n", trial))
  }
  if (!is.null(r_or) && !all(evaluate_formula(r_or, u) == (t1 | t2))) {
    fail = TRUE; cat(sprintf("FAIL [denseop-%d]: OR mismatch\n", trial))
  }
  if (!is.null(r_not) && !all(evaluate_formula(r_not, u) == !t1)) {
    fail = TRUE; cat(sprintf("FAIL [denseop-%d]: NOT mismatch\n", trial))
  }
  if (fail) n_failures = n_failures + 1
}
cat(sprintf("  Dense sharing operations: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
