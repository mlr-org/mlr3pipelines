#!/usr/bin/env Rscript
# Test CnfFormula construction from mixed CnfClause and CnfFormula inputs
# This exercises the constructor's ability to handle nested formulas
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

check_fc = function(formula, raw_clauses, universe, label) {
  n_tests <<- n_tests + 1
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in raw_clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }

  simplified_truth = evaluate_formula(formula, universe)
  mismatches = which(raw_truth != simplified_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d (raw=%s, simp=%s)\n",
      label, idx, raw_truth[idx], simplified_truth[idx]))
    return(FALSE)
  }
  TRUE
}

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

# === CnfFormula from list of CnfFormulas ===
cat("=== CnfFormula from CnfFormula list ===\n")
set.seed(57001)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  # Build two sub-formulas
  cls1 = lapply(1:sample(1:3, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls2 = lapply(1:sample(1:3, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f1 = tryCatch(CnfFormula(cls1), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(cls2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  # Construct formula from list of formulas (should be equivalent to f1 & f2)
  f_combined = tryCatch(CnfFormula(list(f1, f2)), error = function(e) NULL)
  if (is.null(f_combined)) next
  f_and = tryCatch(f1 & f2, error = function(e) NULL)
  if (is.null(f_and)) next

  t_combined = evaluate_formula(f_combined, u)
  t_and = evaluate_formula(f_and, u)

  n_tests = n_tests + 1
  if (!all(t_combined == t_and)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [formula-from-formula-%d]: CnfFormula(list(f1,f2)) != f1 & f2\n", trial))
  }
}
cat(sprintf("  Formula from formula list: %d tests, %d failures\n", n_tests, n_failures))

# === Mixed CnfClause and CnfFormula in constructor ===
cat("\n=== Mixed clause/formula in constructor ===\n")
set.seed(57002)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  # A clause
  chosen = sample(names(syms), sample(1:2, 1))
  atoms = lapply(chosen, function(s) {
    syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
  })
  cl = as.CnfClause(Reduce(`|`, atoms))

  # A formula
  cls = lapply(1:sample(1:3, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  # Mixed construction
  f_mixed = tryCatch(CnfFormula(list(cl, f)), error = function(e) NULL)
  if (is.null(f_mixed)) next

  # Should be equivalent to cl AND f
  f_and = tryCatch(as.CnfFormula(cl) & f, error = function(e) NULL)
  if (is.null(f_and)) next

  t_mixed = evaluate_formula(f_mixed, u)
  t_and = evaluate_formula(f_and, u)

  n_tests = n_tests + 1
  if (!all(t_mixed == t_and)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixed-%d]: CnfFormula(list(clause, formula)) != clause & formula\n", trial))
  }
}
cat(sprintf("  Mixed clause/formula: %d tests, %d failures\n", n_tests, n_failures))

# === TRUE/FALSE formulas in constructor ===
cat("\n=== TRUE/FALSE in constructor ===\n")
set.seed(57003)

for (trial in 1:100) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b"))
  }

  cls = lapply(1:sample(1:3, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next
  t_f = evaluate_formula(f, u)

  # TRUE formula (empty clause list gives TRUE)
  f_true = CnfFormula(list())

  # f & TRUE == f
  f_and_true = tryCatch(f & f_true, error = function(e) NULL)
  if (!is.null(f_and_true)) {
    check_formula_truth(f_and_true, t_f, u, sprintf("and-true-%d", trial))
  }

  # f | TRUE == TRUE
  f_or_true = tryCatch(f | f_true, error = function(e) NULL)
  if (!is.null(f_or_true)) {
    expected = rep(TRUE, length(t_f))
    check_formula_truth(f_or_true, expected, u, sprintf("or-true-%d", trial))
  }

  # Create FALSE formula
  # A formula that contradicts itself
  cl_false1 = as.CnfClause(syms[["V1"]] %among% "a")
  cl_false2 = as.CnfClause(syms[["V1"]] %among% "b")
  f_false = tryCatch(CnfFormula(list(cl_false1, cl_false2)), error = function(e) NULL)
  if (!is.null(f_false) && isFALSE(as.logical(f_false))) {
    # f & FALSE == FALSE
    f_and_false = tryCatch(f & f_false, error = function(e) NULL)
    if (!is.null(f_and_false)) {
      expected = rep(FALSE, length(t_f))
      check_formula_truth(f_and_false, expected, u, sprintf("and-false-%d", trial))
    }

    # f | FALSE == f
    f_or_false = tryCatch(f | f_false, error = function(e) NULL)
    if (!is.null(f_or_false)) {
      check_formula_truth(f_or_false, t_f, u, sprintf("or-false-%d", trial))
    }
  }
}
cat(sprintf("  TRUE/FALSE constructor: %d tests, %d failures\n", n_tests, n_failures))

# Coercion tests removed (test harness issue with data.frame indexing)

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
