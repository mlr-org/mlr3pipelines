#!/usr/bin/env Rscript
# Negation of formulas with many clauses (large De Morgan expansion):
# !.CnfFormula applies De Morgan: !(c1 & c2 & ... & cn) = !c1 | !c2 | ... | !cn
# Each !ci produces a formula (conjunction of negated atoms), and then these
# are combined with OR (distribution). With many clauses, this creates
# very large intermediate formulas.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: Negation of 3-5 clause formulas ===
cat("=== Negation 3-5 clauses ===\n")
set.seed(231001)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  nf = tryCatch(!f, error = function(e) e)
  if (inherits(nf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neg35-%d]: %s\n", trial, nf$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_nf = evaluate_formula(nf, u)
  if (!all(t_nf == !t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [neg35-%d]: negation mismatch\n", trial))
  }
}
cat(sprintf("  Negation 3-5 clauses: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Negation of unit formulas ===
cat("\n=== Negation of unit formulas ===\n")
set.seed(231002)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Formula with mix of units and multi-symbol clauses
  clauses = list()
  n_units = sample(1:3, 1)
  unit_syms = sample(sym_names, n_units)
  for (s in unit_syms) {
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
  }
  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  nf = tryCatch(!f, error = function(e) e)
  if (inherits(nf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [negunit-%d]: %s\n", trial, nf$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_nf = evaluate_formula(nf, u)
  if (!all(t_nf == !t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [negunit-%d]: negation mismatch\n", trial))
  }
}
cat(sprintf("  Negation of units: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Double negation with various clause counts ===
cat("\n=== Double negation ===\n")
set.seed(231003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(1:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  nnf = tryCatch(!!f, error = function(e) e)
  if (inherits(nnf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dblneg-%d]: %s\n", trial, nnf$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_nnf = evaluate_formula(nnf, u)
  if (!all(t_f == t_nnf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dblneg-%d]: double negation mismatch\n", trial))
  }
}
cat(sprintf("  Double negation: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Negation then AND with another formula ===
cat("\n=== Negation then AND ===\n")
set.seed(231004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
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

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  # !f1 & f2
  result = tryCatch(!f1 & f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [negand-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = !t1 & t2
  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [negand-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Negation then AND: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
