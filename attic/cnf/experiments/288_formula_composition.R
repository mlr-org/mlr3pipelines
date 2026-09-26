#!/usr/bin/env Rscript
# Test CnfFormula(list(formula1, formula2, ...)) constructor path.
# Also test as.list roundtrip and formula composition.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Constructor with CnfFormula objects ===
cat("=== CnfFormula of CnfFormula ===\n")
set.seed(288001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:3, 1)
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

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  n_tests = n_tests + 1

  # CnfFormula(list(f1, f2, f3)) should equal f1 & f2 & f3
  composed = tryCatch(CnfFormula(list(f1, f2, f3)), error = function(e) NULL)
  anded = tryCatch(f1 & f2 & f3, error = function(e) NULL)
  if (is.null(composed) || is.null(anded)) next

  tc = evaluate_formula(composed, u)
  ta = evaluate_formula(anded, u)
  if (!all(tc == ta)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [compose-%d]: CnfFormula(list(f1,f2,f3)) != f1&f2&f3\n", trial))
  }
}
cat(sprintf("  CnfFormula of CnfFormula: %d tests, %d failures\n", n_tests, n_failures))

# === Mixed constructor: clauses + formulas ===
cat("\n=== Mixed constructor ===\n")
set.seed(288002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create a formula and a standalone clause
  n_cl = sample(2:3, 1)
  cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 1) next

  formula = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(formula)) next

  extra_clause = as.CnfClause(syms[[sample(sym_names, 1)]] %among% sample(dom, sample(1:2, 1)))
  if (isTRUE(unclass(extra_clause))) next

  n_tests = n_tests + 1

  # CnfFormula(list(formula, extra_clause)) should equal formula & CnfFormula(list(extra_clause))
  mixed = tryCatch(CnfFormula(list(formula, extra_clause)), error = function(e) NULL)
  separate = tryCatch(formula & CnfFormula(list(extra_clause)), error = function(e) NULL)
  if (is.null(mixed) || is.null(separate)) next

  tm = evaluate_formula(mixed, u)
  ts = evaluate_formula(separate, u)
  if (!all(tm == ts)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixed-%d]: mixed constructor differs from separate AND\n", trial))
  }
}
cat(sprintf("  Mixed constructor: %d tests, %d failures\n", n_tests, n_failures))

# === as.list roundtrip ===
cat("\n=== as.list roundtrip ===\n")
set.seed(288003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(2:4, 1)
  cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 2) next

  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next
  if (is.logical(unclass(f))) next

  n_tests = n_tests + 1

  # as.list then reconstruct
  clause_list = as.list(f)
  f2 = tryCatch(CnfFormula(clause_list), error = function(e) NULL)
  if (is.null(f2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [roundtrip-%d]: reconstruction failed\n", trial))
    next
  }

  t1 = evaluate_formula(f, u)
  t2 = evaluate_formula(f2, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [roundtrip-%d]: roundtrip changed semantics\n", trial))
  }
}
cat(sprintf("  as.list roundtrip: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
