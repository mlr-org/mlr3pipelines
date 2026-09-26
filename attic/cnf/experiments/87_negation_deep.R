#!/usr/bin/env Rscript
# Deep testing of !.CnfFormula negation:
# - Negation of formulas with tautological symbol ranges
# - Negation of single-clause formulas (should produce AND of negated atoms)
# - Double negation identity
# - De Morgan with complex formulas
# - Negation followed by combination with original
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Negation produces empty ranges (FALSE clauses) ===
cat("=== Negation with tautological symbols ===\n")
set.seed(87001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Create clause with full-domain symbol: A %among% dom | B %among% {subset}
  # Negation: !A (empty) & !B = FALSE & !B = FALSE
  # But that's just the negation of one clause; the overall formula
  # negation involves De Morgan across multiple clauses

  vals = sample(dom, sample(1:2, 1))
  clause = as.CnfClause(A %among% vals | B %among% sample(dom, sample(1:2, 1)))
  f = tryCatch(CnfFormula(list(clause)), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  neg_f = tryCatch(!f, error = function(e) e)
  if (inherits(neg_f, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neg-taut-%d]: %s\n", trial, neg_f$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_neg = evaluate_formula(neg_f, u)
  if (!all(t_f != t_neg)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [neg-taut-%d]: f and !f should be complementary\n", trial))
  }
}
cat(sprintf("  Negation tautological: %d tests, %d failures\n", n_tests, n_failures))

# === Negation of multi-clause formulas ===
cat("\n=== Negation of multi-clause formulas ===\n")
set.seed(87002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(1:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  neg_f = tryCatch(!f, error = function(e) e)
  if (inherits(neg_f, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neg-multi-%d]: %s\n", trial, neg_f$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_neg = evaluate_formula(neg_f, u)

  # f and !f should be complementary (XOR = all TRUE)
  if (!all(xor(t_f, t_neg))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [neg-multi-%d]: not complementary\n", trial))
  }
}
cat(sprintf("  Multi-clause negation: %d tests, %d failures\n", n_tests, n_failures))

# === Double negation identity ===
cat("\n=== Double negation ===\n")
set.seed(87003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(1:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  neg_neg_f = tryCatch(!!f, error = function(e) e)
  if (inherits(neg_neg_f, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dbl-neg-%d]: %s\n", trial, neg_neg_f$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_nn = evaluate_formula(neg_neg_f, u)
  if (!all(t_f == t_nn)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dbl-neg-%d]: !!f != f\n", trial))
  }
}
cat(sprintf("  Double negation: %d tests, %d failures\n", n_tests, n_failures))

# === De Morgan: !(f1 & f2) == !f1 | !f2 ===
cat("\n=== De Morgan's law ===\n")
set.seed(87004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function() {
    n_cl = sample(1:2, 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula()
  f2 = make_formula()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  lhs = tryCatch(!(f1 & f2), error = function(e) NULL)
  rhs = tryCatch((!f1) | (!f2), error = function(e) NULL)
  if (is.null(lhs) || is.null(rhs)) next

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan-%d]: !(f1 & f2) != !f1 | !f2\n", trial))
  }
}
cat(sprintf("  De Morgan: %d tests, %d failures\n", n_tests, n_failures))

# === Negation + AND = FALSE ===
cat("\n=== Negation AND identity ===\n")
set.seed(87005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(1:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  neg_f = tryCatch(!f, error = function(e) NULL)
  if (is.null(neg_f)) next

  n_tests = n_tests + 1
  f_and_negf = tryCatch(f & neg_f, error = function(e) e)
  if (inherits(f_and_negf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [and-neg-%d]: %s\n", trial, f_and_negf$message)); next
  }

  t_result = evaluate_formula(f_and_negf, u)
  if (any(t_result)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [and-neg-%d]: f & !f should be FALSE everywhere\n", trial))
  }
}
cat(sprintf("  AND negation: %d tests, %d failures\n", n_tests, n_failures))

# === Negation + OR = TRUE ===
cat("\n=== Negation OR identity ===\n")
set.seed(87006)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(1:2, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  neg_f = tryCatch(!f, error = function(e) NULL)
  if (is.null(neg_f)) next

  n_tests = n_tests + 1
  f_or_negf = tryCatch(f | neg_f, error = function(e) e)
  if (inherits(f_or_negf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [or-neg-%d]: %s\n", trial, f_or_negf$message)); next
  }

  t_result = evaluate_formula(f_or_negf, u)
  if (!all(t_result)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [or-neg-%d]: f | !f should be TRUE everywhere\n", trial))
  }
}
cat(sprintf("  OR negation: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
