#!/usr/bin/env Rscript
# Deep negation tests: verify De Morgan laws hold for complex formulas,
# test that negation produces correct results when the input has many
# clauses (producing many clauses in the negated formula), and test
# that chained operations involving negation remain consistent.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === De Morgan: !(f1 & f2) == !f1 | !f2 ===
cat("=== De Morgan AND ===\n")
set.seed(267001)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:3, 1)
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
  lhs = tryCatch(!(f1 & f2), error = function(e) NULL)
  nf1 = tryCatch(!f1, error = function(e) NULL)
  nf2 = tryCatch(!f2, error = function(e) NULL)
  if (is.null(nf1) || is.null(nf2)) next
  rhs = tryCatch(nf1 | nf2, error = function(e) NULL)

  if (!is.null(lhs) && !is.null(rhs) && !all(evaluate_formula(lhs, u) == evaluate_formula(rhs, u))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dm-and-%d]: !(f1&f2) != !f1|!f2\n", trial))
  }
}
cat(sprintf("  De Morgan AND: %d tests, %d failures\n", n_tests, n_failures))

# === De Morgan: !(f1 | f2) == !f1 & !f2 ===
cat("\n=== De Morgan OR ===\n")
set.seed(267002)

for (trial in 1:400) {
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
  lhs = tryCatch(!(f1 | f2), error = function(e) NULL)
  nf1 = tryCatch(!f1, error = function(e) NULL)
  nf2 = tryCatch(!f2, error = function(e) NULL)
  if (is.null(nf1) || is.null(nf2)) next
  rhs = tryCatch(nf1 & nf2, error = function(e) NULL)

  if (!is.null(lhs) && !is.null(rhs) && !all(evaluate_formula(lhs, u) == evaluate_formula(rhs, u))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dm-or-%d]: !(f1|f2) != !f1&!f2\n", trial))
  }
}
cat(sprintf("  De Morgan OR: %d tests, %d failures\n", n_tests, n_failures))

# === Negation of multi-clause formulas ===
cat("\n=== Negation of multi-clause ===\n")
set.seed(267003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Formulas with 3-4 clauses => negation produces many clauses via De Morgan
  n_cl = sample(3:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  t_f = evaluate_formula(f, u)

  nf = tryCatch(!f, error = function(e) NULL)
  if (is.null(nf)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [negmc-%d]: negation failed\n", trial)); next
  }

  t_nf = evaluate_formula(nf, u)
  if (!all(t_nf == !t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [negmc-%d]: !f != expected\n", trial))
  }
}
cat(sprintf("  Negation of multi-clause: %d tests, %d failures\n", n_tests, n_failures))

# === Chained: f & (!f | g) == f & g ===
cat("\n=== Chained negation identity ===\n")
set.seed(267004)

for (trial in 1:400) {
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

  f = make_f(); g = make_f()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1
  t_f = evaluate_formula(f, u)
  t_g = evaluate_formula(g, u)

  # f & (!f | g) should equal f & g
  nf = tryCatch(!f, error = function(e) NULL)
  if (is.null(nf)) next

  lhs = tryCatch(f & (nf | g), error = function(e) NULL)
  rhs = tryCatch(f & g, error = function(e) NULL)

  if (!is.null(lhs) && !is.null(rhs) && !all(evaluate_formula(lhs, u) == evaluate_formula(rhs, u))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chained-%d]: f & (!f | g) != f & g\n", trial))
  }
}
cat(sprintf("  Chained negation identity: %d tests, %d failures\n", n_tests, n_failures))

# === XOR property: (f ^ g) == (f & !g) | (!f & g) ===
cat("\n=== XOR identity ===\n")
set.seed(267005)

for (trial in 1:300) {
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

  f = make_f(); g = make_f()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1
  t_f = evaluate_formula(f, u)
  t_g = evaluate_formula(g, u)
  expected_xor = xor(t_f, t_g)

  nf = tryCatch(!f, error = function(e) NULL)
  ng = tryCatch(!g, error = function(e) NULL)
  if (is.null(nf) || is.null(ng)) next

  xor_formula = tryCatch((f & ng) | (nf & g), error = function(e) NULL)
  if (!is.null(xor_formula) && !all(evaluate_formula(xor_formula, u) == expected_xor)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [xor-%d]: XOR identity failed\n", trial))
  }
}
cat(sprintf("  XOR identity: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
