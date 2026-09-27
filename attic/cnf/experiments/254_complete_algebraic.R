#!/usr/bin/env Rscript
# Complete algebraic property verification:
# Verify all important Boolean algebra laws hold for CNF formulas.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Identity Laws ===
cat("=== Identity laws ===\n")
set.seed(254001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(1:3, 1)
  cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 1) next
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  t_f = evaluate_formula(f, u)
  f_true = as.CnfFormula(TRUE)
  f_false = as.CnfFormula(FALSE)

  # f & TRUE == f
  r = tryCatch(f & f_true, error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == t_f)) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [id-and-true-%d]\n", trial)); next
  }

  # f | FALSE == f
  r = tryCatch(f | f_false, error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == t_f)) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [id-or-false-%d]\n", trial)); next
  }

  # f & FALSE == FALSE
  r = tryCatch(f & f_false, error = function(e) NULL)
  if (!is.null(r) && any(evaluate_formula(r, u))) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [annihilator-and-%d]\n", trial)); next
  }

  # f | TRUE == TRUE
  r = tryCatch(f | f_true, error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u))) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [annihilator-or-%d]\n", trial)); next
  }

  # f & !f == FALSE
  r = tryCatch(f & !f, error = function(e) NULL)
  if (!is.null(r) && any(evaluate_formula(r, u))) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [complement-and-%d]\n", trial)); next
  }

  # f | !f == TRUE
  r = tryCatch(f | !f, error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u))) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [complement-or-%d]\n", trial))
  }
}
cat(sprintf("  Identity laws: %d tests, %d failures\n", n_tests, n_failures))

# === Commutativity ===
cat("\n=== Commutativity ===\n")
set.seed(254002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:2, 1)
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

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  # AND commutativity: f1 & f2 == f2 & f1
  r1 = tryCatch(f1 & f2, error = function(e) NULL)
  r2 = tryCatch(f2 & f1, error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2) && !all(evaluate_formula(r1, u) == evaluate_formula(r2, u))) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [comm-and-%d]\n", trial)); next
  }

  # OR commutativity: f1 | f2 == f2 | f1
  r1 = tryCatch(f1 | f2, error = function(e) NULL)
  r2 = tryCatch(f2 | f1, error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2) && !all(evaluate_formula(r1, u) == evaluate_formula(r2, u))) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [comm-or-%d]\n", trial))
  }
}
cat(sprintf("  Commutativity: %d tests, %d failures\n", n_tests, n_failures))

# === Associativity ===
cat("\n=== Associativity ===\n")
set.seed(254003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    cls = list(as.CnfClause(syms[[sample(sym_names, 1)]] %among% sample(dom, sample(1:2, 1))))
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (any(sapply(list(f1, f2, f3), is.null))) next

  n_tests = n_tests + 1

  # AND associativity: (f1 & f2) & f3 == f1 & (f2 & f3)
  lhs = tryCatch((f1 & f2) & f3, error = function(e) NULL)
  rhs = tryCatch(f1 & (f2 & f3), error = function(e) NULL)
  if (!is.null(lhs) && !is.null(rhs) && !all(evaluate_formula(lhs, u) == evaluate_formula(rhs, u))) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [assoc-and-%d]\n", trial)); next
  }

  # OR associativity: (f1 | f2) | f3 == f1 | (f2 | f3)
  lhs = tryCatch((f1 | f2) | f3, error = function(e) NULL)
  rhs = tryCatch(f1 | (f2 | f3), error = function(e) NULL)
  if (!is.null(lhs) && !is.null(rhs) && !all(evaluate_formula(lhs, u) == evaluate_formula(rhs, u))) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [assoc-or-%d]\n", trial))
  }
}
cat(sprintf("  Associativity: %d tests, %d failures\n", n_tests, n_failures))

# === Distributivity ===
cat("\n=== Distributivity ===\n")
set.seed(254004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    cls = list(as.CnfClause(syms[[sample(sym_names, 1)]] %among% sample(dom, sample(1:2, 1))))
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (any(sapply(list(f1, f2, f3), is.null))) next

  n_tests = n_tests + 1

  # f1 & (f2 | f3) == (f1 & f2) | (f1 & f3)
  lhs = tryCatch(f1 & (f2 | f3), error = function(e) NULL)
  rhs = tryCatch((f1 & f2) | (f1 & f3), error = function(e) NULL)
  if (!is.null(lhs) && !is.null(rhs) && !all(evaluate_formula(lhs, u) == evaluate_formula(rhs, u))) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [dist-and-%d]\n", trial)); next
  }

  # f1 | (f2 & f3) == (f1 | f2) & (f1 | f3)
  lhs = tryCatch(f1 | (f2 & f3), error = function(e) NULL)
  rhs = tryCatch((f1 | f2) & (f1 | f3), error = function(e) NULL)
  if (!is.null(lhs) && !is.null(rhs) && !all(evaluate_formula(lhs, u) == evaluate_formula(rhs, u))) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [dist-or-%d]\n", trial))
  }
}
cat(sprintf("  Distributivity: %d tests, %d failures\n", n_tests, n_failures))

# === Absorption ===
cat("\n=== Absorption ===\n")
set.seed(254005)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:2, 1)
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

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)

  # f1 & (f1 | f2) == f1
  r = tryCatch(f1 & (f1 | f2), error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == t1)) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [absorb-and-%d]\n", trial)); next
  }

  # f1 | (f1 & f2) == f1
  r = tryCatch(f1 | (f1 & f2), error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == t1)) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [absorb-or-%d]\n", trial))
  }
}
cat(sprintf("  Absorption: %d tests, %d failures\n", n_tests, n_failures))

# === Involution ===
cat("\n=== Involution (double negation) ===\n")
set.seed(254006)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(1:2, 1)
  cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 1) next
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  t_f = evaluate_formula(f, u)

  # !!f == f
  r = tryCatch(!!f, error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == t_f)) {
    n_failures = n_failures + 1; cat(sprintf("FAIL [involution-%d]\n", trial))
  }
}
cat(sprintf("  Involution: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
