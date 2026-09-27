#!/usr/bin/env Rscript
# Complex negation patterns:
# - Negation of large formulas (many clauses)
# - Triple/quadruple negation on multi-clause formulas
# - Negation then AND, then negation again
# - De Morgan's law on complex expressions
# Also: specific test of !.CnfFormula using setdiff on clause ranges
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Negation of multi-clause formulas ===
cat("=== Negation of multi-clause formulas ===\n")
set.seed(150001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  t_f = evaluate_formula(f, u)

  # Test negation
  n_tests = n_tests + 1
  neg_f = tryCatch(!f, error = function(e) NULL)
  if (is.null(neg_f)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neg-%d]: negation failed\n", trial)); next
  }
  t_neg = evaluate_formula(neg_f, u)
  if (!all(t_neg == !t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [neg-%d]: !f != logical negation\n", trial))
  }

  # Test double negation
  n_tests = n_tests + 1
  dbl_neg = tryCatch(!!f, error = function(e) NULL)
  if (is.null(dbl_neg)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dblneg-%d]: double negation failed\n", trial)); next
  }
  t_dbl = evaluate_formula(dbl_neg, u)
  if (!all(t_dbl == t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dblneg-%d]: !!f != f\n", trial))
  }
}
cat(sprintf("  Negation basic: %d tests, %d failures\n", n_tests, n_failures))

# === De Morgan complex ===
cat("\n=== De Morgan complex ===\n")
set.seed(150002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  make_f = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f = make_f(sample(1:3, 1))
  g = make_f(sample(1:3, 1))
  if (is.null(f) || is.null(g)) next

  t_f = evaluate_formula(f, u)
  t_g = evaluate_formula(g, u)

  # De Morgan: !(f & g) == !f | !g
  n_tests = n_tests + 1
  f_and_g = tryCatch(f & g, error = function(e) NULL)
  if (is.null(f_and_g)) next
  neg_fg = tryCatch(!f_and_g, error = function(e) NULL)
  neg_f_or_neg_g = tryCatch(!f | !g, error = function(e) NULL)
  if (is.null(neg_fg) || is.null(neg_f_or_neg_g)) next

  t_lhs = evaluate_formula(neg_fg, u)
  t_rhs = evaluate_formula(neg_f_or_neg_g, u)
  expected = !(t_f & t_g)
  if (!all(t_lhs == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan-lhs-%d]\n", trial))
  }
  if (!all(t_rhs == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan-rhs-%d]\n", trial))
  }

  # De Morgan: !(f | g) == !f & !g
  n_tests = n_tests + 1
  f_or_g = tryCatch(f | g, error = function(e) NULL)
  if (is.null(f_or_g)) next
  neg_fog = tryCatch(!f_or_g, error = function(e) NULL)
  neg_f_and_neg_g = tryCatch(!f & !g, error = function(e) NULL)
  if (is.null(neg_fog) || is.null(neg_f_and_neg_g)) next

  t_lhs2 = evaluate_formula(neg_fog, u)
  t_rhs2 = evaluate_formula(neg_f_and_neg_g, u)
  expected2 = !(t_f | t_g)
  if (!all(t_lhs2 == expected2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan2-lhs-%d]\n", trial))
  }
  if (!all(t_rhs2 == expected2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan2-rhs-%d]\n", trial))
  }
}
cat(sprintf("  De Morgan: %d tests, %d failures\n", n_tests, n_failures))

# === Negation-then-AND-then-negation chains ===
cat("\n=== Neg-AND-Neg chains ===\n")
set.seed(150003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    clauses = lapply(1:sample(1:2, 1), function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f = make_f()
  g = make_f()
  if (is.null(f) || is.null(g)) next

  t_f = evaluate_formula(f, u)
  t_g = evaluate_formula(g, u)

  # !(!f & !g) = f | g
  n_tests = n_tests + 1
  result = tryCatch(!(!f & !g), error = function(e) NULL)
  if (is.null(result)) next
  t_result = evaluate_formula(result, u)
  expected = t_f | t_g
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [nag-%d]: !(!f & !g) != f|g\n", trial))
  }

  # !(!f | !g) = f & g
  n_tests = n_tests + 1
  result2 = tryCatch(!(!f | !g), error = function(e) NULL)
  if (is.null(result2)) next
  t_result2 = evaluate_formula(result2, u)
  expected2 = t_f & t_g
  if (!all(t_result2 == expected2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [nog-%d]: !(!f | !g) != f&g\n", trial))
  }
}
cat(sprintf("  Neg-AND-Neg: %d tests, %d failures\n", n_tests, n_failures))

# === Triple negation ===
cat("\n=== Triple negation ===\n")
set.seed(150004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  n_cl = sample(1:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) == 0) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next
  t_f = evaluate_formula(f, u)

  # !!!f = !f
  n_tests = n_tests + 1
  triple = tryCatch(!!!f, error = function(e) NULL)
  if (is.null(triple)) next
  t_triple = evaluate_formula(triple, u)
  if (!all(t_triple == !t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [triple-%d]: !!!f != !f\n", trial))
  }
}
cat(sprintf("  Triple negation: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
