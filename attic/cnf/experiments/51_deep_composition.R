#!/usr/bin/env Rscript
# Test deeply nested formula composition: ((f1 | f2) & f3) | (f4 & !f5) etc.
# Exercises the |.CnfFormula distribution, &.CnfFormula, and !.CnfFormula in deep chains
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

check_formula_truth = function(formula, expected_truth, universe, label) {
  n_tests <<- n_tests + 1
  result_truth = evaluate_formula(formula, universe)
  mismatches = which(result_truth != expected_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d (expected=%s, got=%s)\n",
      label, idx, expected_truth[idx], result_truth[idx]))
    return(FALSE)
  }
  TRUE
}

# === Deep AND chains: f1 & f2 & f3 & f4 & f5 ===
cat("=== Deep AND chains ===\n")
set.seed(51001)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  # Build 3-5 formulas and AND them together
  n_formulas = sample(3:5, 1)
  formulas = list()
  clauses_lists = list()
  for (fi in 1:n_formulas) {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_atoms = sample(1:min(3, 3), 1)
      chosen = sample(names(syms), n_atoms)
      atoms = lapply(chosen, function(s) {
        syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses_lists[[fi]] = cls
    formulas[[fi]] = tryCatch(CnfFormula(cls), error = function(e) NULL)
    if (is.null(formulas[[fi]])) next
  }
  if (any(sapply(formulas, is.null))) next

  # Truth tables for individual formulas
  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  truths = lapply(formulas, function(f) evaluate_formula(f, u))
  expected_and = Reduce(`&`, truths)

  # Deep AND chain
  combined = Reduce(`&`, formulas)
  check_formula_truth(combined, expected_and, u, sprintf("deep-and-%d", trial))
}
cat(sprintf("  Deep AND: %d tests, %d failures\n", n_tests, n_failures))

# === Deep OR chains: f1 | f2 | f3 | f4 ===
cat("\n=== Deep OR chains ===\n")
set.seed(51002)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  n_formulas = sample(3:4, 1)
  formulas = list()
  for (fi in 1:n_formulas) {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_atoms = sample(1:min(3, 3), 1)
      chosen = sample(names(syms), n_atoms)
      atoms = lapply(chosen, function(s) {
        syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    formulas[[fi]] = tryCatch(CnfFormula(cls), error = function(e) NULL)
    if (is.null(formulas[[fi]])) next
  }
  if (any(sapply(formulas, is.null))) next

  truths = lapply(formulas, function(f) evaluate_formula(f, u))
  expected_or = Reduce(`|`, truths)

  combined = Reduce(`|`, formulas)
  check_formula_truth(combined, expected_or, u, sprintf("deep-or-%d", trial))
}
cat(sprintf("  Deep OR: %d tests, %d failures\n", n_tests, n_failures))

# === Alternating AND/OR: (f1 | f2) & (f3 | f4) ===
cat("\n=== Alternating AND/OR ===\n")
set.seed(51003)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  mk_formula = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      n_atoms = sample(1:2, 1)
      chosen = sample(names(syms), n_atoms)
      atoms = lapply(chosen, function(s) {
        syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = mk_formula(); f2 = mk_formula(); f3 = mk_formula(); f4 = mk_formula()
  if (is.null(f1) || is.null(f2) || is.null(f3) || is.null(f4)) next

  t1 = evaluate_formula(f1, u); t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u); t4 = evaluate_formula(f4, u)

  # (f1 | f2) & (f3 | f4)
  expected = (t1 | t2) & (t3 | t4)
  combined = tryCatch((f1 | f2) & (f3 | f4), error = function(e) NULL)
  if (is.null(combined)) next
  check_formula_truth(combined, expected, u, sprintf("alt-andor-%d", trial))
}
cat(sprintf("  Alternating AND/OR: %d tests, %d failures\n", n_tests, n_failures))

# === Negation in deep trees: !(!f1 & f2) | (f3 & !f4) ===
cat("\n=== Negation in deep trees ===\n")
set.seed(51004)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b"))
  }

  mk_formula = function() {
    cls = lapply(1:sample(1:2, 1), function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) {
        syms[[s]] %among% sample(c("a", "b"), 1)
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = mk_formula(); f2 = mk_formula(); f3 = mk_formula(); f4 = mk_formula()
  if (is.null(f1) || is.null(f2) || is.null(f3) || is.null(f4)) next

  t1 = evaluate_formula(f1, u); t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u); t4 = evaluate_formula(f4, u)

  # Test various deep negation patterns
  pattern = sample(1:4, 1)
  if (pattern == 1) {
    # !(!f1 & f2) | (f3 & !f4)
    expected = !(!t1 & t2) | (t3 & !t4)
    combined = tryCatch((!(!f1 & f2)) | (f3 & !f4), error = function(e) NULL)
  } else if (pattern == 2) {
    # !(f1 | f2) & !(f3 | f4)
    expected = !(t1 | t2) & !(t3 | t4)
    combined = tryCatch(!(f1 | f2) & !(f3 | f4), error = function(e) NULL)
  } else if (pattern == 3) {
    # !!(f1 & !f2) | f3
    expected = (t1 & !t2) | t3
    combined = tryCatch(!!(f1 & !f2) | f3, error = function(e) NULL)
  } else {
    # (f1 | !f2) & (!f3 | f4)
    expected = (t1 | !t2) & (!t3 | t4)
    combined = tryCatch((f1 | !f2) & (!f3 | f4), error = function(e) NULL)
  }
  if (is.null(combined)) next
  check_formula_truth(combined, expected, u, sprintf("deep-neg-%d-p%d", trial, pattern))
}
cat(sprintf("  Deep negation: %d tests, %d failures\n", n_tests, n_failures))

# === Triple negation and beyond ===
cat("\n=== Triple+ negation ===\n")
set.seed(51005)

for (trial in 1:100) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:2) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  cls = lapply(1:sample(1:3, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  t_f = evaluate_formula(f, u)

  # !!!f == !f
  f_nnn = tryCatch(!!!f, error = function(e) NULL)
  if (!is.null(f_nnn)) {
    check_formula_truth(f_nnn, !t_f, u, sprintf("triple-neg-%d", trial))
  }

  # !!!!f == f
  f_nnnn = tryCatch(!!!!f, error = function(e) NULL)
  if (!is.null(f_nnnn)) {
    check_formula_truth(f_nnnn, t_f, u, sprintf("quad-neg-%d", trial))
  }
}
cat(sprintf("  Triple+ negation: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
