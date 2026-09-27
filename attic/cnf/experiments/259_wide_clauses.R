#!/usr/bin/env Rscript
# Test formulas with very wide clauses (many symbols per clause).
# Wide clauses create large is_not_subset_of matrices and exercise
# different HLA/subsumption patterns since there are many potential
# symbol overlaps between clauses.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Wide clauses, 8 vars, clauses use 5-7 symbols ===
cat("=== Wide clauses 8-var ===\n")
set.seed(259001)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:8)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(5:7, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [wide8-%d]: %s\n", trial, result$message)); next
  }
  # 3^8 = 6561 assignments, manageable
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [wide8-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Wide clauses 8-var: %d tests, %d failures\n", n_tests, n_failures))

# === Mix of wide and narrow clauses ===
cat("\n=== Mix wide and narrow ===\n")
set.seed(259002)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:7)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  clauses = list()
  # A few wide clauses
  for (j in 1:sample(2:3, 1)) {
    n_sym = sample(5:7, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  # A few narrow clauses (1-2 symbols, likely units)
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mixwn-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixwn-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Mix wide and narrow: %d tests, %d failures\n", n_tests, n_failures))

# === Wide clause operations ===
cat("\n=== Wide clause operations ===\n")
set.seed(259003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:6)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(3:5, 1)
      chosen = sample(sym_names, n_sym)
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

  op = sample(1:3, 1)
  if (op == 1) {
    r = tryCatch(f1 & f2, error = function(e) NULL)
    expected = t1 & t2
  } else if (op == 2) {
    r = tryCatch(f1 | f2, error = function(e) NULL)
    expected = t1 | t2
  } else {
    r = tryCatch(!f1, error = function(e) NULL)
    expected = !t1
  }
  if (!is.null(r) && !all(evaluate_formula(r, u) == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [wideop-%d]: op=%d mismatch\n", trial, op))
  }
}
cat(sprintf("  Wide clause operations: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
