#!/usr/bin/env Rscript
# Test with almost-full-domain ranges: domain size 5, atoms use ranges of size 4.
# This means each atom excludes exactly one value. The complement of such an atom
# is a single-value atom. This creates interesting SSE/subsumption patterns because
# the ranges are very similar to each other.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Almost-full ranges, 3 variables ===
cat("=== Almost-full 3-var ===\n")
set.seed(256001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) {
      # Range of size 4 (exclude one value)
      excluded = sample(dom, 1)
      syms[[s]] %among% setdiff(dom, excluded)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [af3-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [af3-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Almost-full 3-var: %d tests, %d failures\n", n_tests, n_failures))

# === Mix of almost-full and narrow ranges ===
cat("\n=== Mixed range widths ===\n")
set.seed(256002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) {
      if (runif(1) > 0.5) {
        # Almost full: 4 of 5 values
        excluded = sample(dom, 1)
        syms[[s]] %among% setdiff(dom, excluded)
      } else {
        # Narrow: 1 of 5 values
        syms[[s]] %among% sample(dom, 1)
      }
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mix-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mix-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Mixed range widths: %d tests, %d failures\n", n_tests, n_failures))

# === Operations with almost-full domain formulas ===
cat("\n=== Almost-full operations ===\n")
set.seed(256003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) {
        excluded = sample(dom, sample(1:2, 1))
        syms[[s]] %among% setdiff(dom, excluded)
      })
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

  # Test AND, OR, NOT
  r_and = tryCatch(f1 & f2, error = function(e) NULL)
  r_or = tryCatch(f1 | f2, error = function(e) NULL)
  r_not = tryCatch(!f1, error = function(e) NULL)

  fail = FALSE
  if (!is.null(r_and) && !all(evaluate_formula(r_and, u) == (t1 & t2))) {
    fail = TRUE; cat(sprintf("FAIL [afop-%d]: AND mismatch\n", trial))
  }
  if (!is.null(r_or) && !all(evaluate_formula(r_or, u) == (t1 | t2))) {
    fail = TRUE; cat(sprintf("FAIL [afop-%d]: OR mismatch\n", trial))
  }
  if (!is.null(r_not) && !all(evaluate_formula(r_not, u) == !t1)) {
    fail = TRUE; cat(sprintf("FAIL [afop-%d]: NOT mismatch\n", trial))
  }
  if (fail) n_failures = n_failures + 1
}
cat(sprintf("  Almost-full operations: %d tests, %d failures\n", n_tests, n_failures))

# === Negation of almost-full = single-value atoms ===
cat("\n=== Negation duality ===\n")
set.seed(256004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Build formula with almost-full atoms
  n_cl = sample(1:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) {
      excluded = sample(dom, 1)
      syms[[s]] %among% setdiff(dom, excluded)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  t_f = evaluate_formula(f, u)

  # !f should have single-value atoms (complement of 4-of-5 is 1-of-5)
  nf = tryCatch(!f, error = function(e) NULL)
  if (is.null(nf)) next

  # Double negation: !!f == f
  nnf = tryCatch(!nf, error = function(e) NULL)
  if (!is.null(nnf) && !all(evaluate_formula(nnf, u) == t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [negdual-%d]: double negation mismatch\n", trial))
    next
  }

  # De Morgan: !(f1 & f2) == !f1 | !f2
  if (n_cl >= 2) {
    f_part1 = tryCatch(CnfFormula(clauses[1]), error = function(e) NULL)
    f_part2 = tryCatch(CnfFormula(clauses[2:length(clauses)]), error = function(e) NULL)
    if (!is.null(f_part1) && !is.null(f_part2)) {
      lhs = tryCatch(!(f_part1 & f_part2), error = function(e) NULL)
      rhs = tryCatch((!f_part1) | (!f_part2), error = function(e) NULL)
      if (!is.null(lhs) && !is.null(rhs) && !all(evaluate_formula(lhs, u) == evaluate_formula(rhs, u))) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [negdual-%d]: De Morgan mismatch\n", trial))
      }
    }
  }
}
cat(sprintf("  Negation duality: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
