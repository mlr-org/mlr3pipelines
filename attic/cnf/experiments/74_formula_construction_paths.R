#!/usr/bin/env Rscript
# Test various construction paths for CnfFormula:
# - From list of CnfClauses
# - From list of CnfFormulas
# - From mixed list
# - From nested CnfFormulas
# - Various coercion paths
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

# === Construction from CnfFormula list ===
cat("=== CnfFormula from list of CnfFormulas ===\n")
set.seed(74001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Build 2-3 sub-formulas, then combine
  sub_formulas = list()
  all_clauses = list()
  for (sf in 1:sample(2:3, 1)) {
    n_cl = sample(1:3, 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) {
        syms[[s]] %among% sample(dom, sample(1:2, 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    f_sub = tryCatch(CnfFormula(clauses), error = function(e) NULL)
    if (is.null(f_sub)) next
    sub_formulas[[length(sub_formulas) + 1]] = f_sub
    all_clauses = c(all_clauses, clauses)
  }
  if (length(sub_formulas) < 2) next

  # Combine via CnfFormula(list of formulas)
  f_combined = tryCatch(CnfFormula(sub_formulas), error = function(e) e)
  if (inherits(f_combined, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [from-formulas-%d]: %s\n", trial, f_combined$message)); next
  }

  # Compare with constructing from all clauses directly
  f_direct = tryCatch(CnfFormula(all_clauses), error = function(e) NULL)
  if (is.null(f_direct)) next

  t_combined = evaluate_formula(f_combined, u)
  t_direct = evaluate_formula(f_direct, u)

  n_tests = n_tests + 1
  if (!all(t_combined == t_direct)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [from-formulas-%d]: combined != direct\n", trial))
  }
}
cat(sprintf("  From CnfFormula list: %d tests, %d failures\n", n_tests, n_failures))

# === Mixed clause/formula list ===
cat("\n=== Mixed clause/formula list ===\n")
set.seed(74002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  all_clauses = list()
  mixed_list = list()

  # Add some clauses directly
  for (i in 1:sample(1:3, 1)) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:2, 1))
    })
    cl = as.CnfClause(Reduce(`|`, atoms))
    mixed_list[[length(mixed_list) + 1]] = cl
    all_clauses[[length(all_clauses) + 1]] = cl
  }

  # Add a formula
  f_clauses = lapply(1:sample(1:2, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f_sub = tryCatch(CnfFormula(f_clauses), error = function(e) NULL)
  if (is.null(f_sub)) next
  mixed_list[[length(mixed_list) + 1]] = f_sub
  all_clauses = c(all_clauses, f_clauses)

  f_mixed = tryCatch(CnfFormula(mixed_list), error = function(e) e)
  if (inherits(f_mixed, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [mixed-%d]: %s\n", trial, f_mixed$message)); next
  }

  f_direct = tryCatch(CnfFormula(all_clauses), error = function(e) NULL)
  if (is.null(f_direct)) next

  t_mixed = evaluate_formula(f_mixed, u)
  t_direct = evaluate_formula(f_direct, u)

  n_tests = n_tests + 1
  if (!all(t_mixed == t_direct)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixed-%d]: mixed != direct\n", trial))
  }
}
cat(sprintf("  Mixed lists: %d tests, %d failures\n", n_tests, n_failures))

# === as.CnfFormula coercion paths ===
cat("\n=== Coercion paths ===\n")

u2 = CnfUniverse()
X = CnfSymbol(u2, "X", c("a", "b", "c"))

# CnfAtom -> CnfFormula
atom = X %among% c("a", "b")
n_tests = n_tests + 1
r = tryCatch(as.CnfFormula(atom), error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: as.CnfFormula(atom) error: %s\n", r$message))
} else {
  t = evaluate_formula(r, u2)
  expected = c("a", "b", "c") %in% c("a", "b")
  if (!all(t == expected)) {
    n_failures = n_failures + 1
    cat("FAIL: as.CnfFormula(atom) wrong truth values\n")
  }
}

# CnfClause -> CnfFormula
clause = as.CnfClause(X %among% c("a"))
n_tests = n_tests + 1
r = tryCatch(as.CnfFormula(clause), error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: as.CnfFormula(clause) error: %s\n", r$message))
}

# TRUE atom -> CnfFormula
taut_atom = X %among% c("a", "b", "c")
n_tests = n_tests + 1
r = tryCatch(as.CnfFormula(taut_atom), error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: as.CnfFormula(TRUE atom) error: %s\n", r$message))
} else if (!isTRUE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: as.CnfFormula(TRUE atom) should be TRUE\n")
}

# FALSE atom -> CnfFormula
contr_atom = X %among% character(0)
n_tests = n_tests + 1
r = tryCatch(as.CnfFormula(contr_atom), error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: as.CnfFormula(FALSE atom) error: %s\n", r$message))
} else if (!isFALSE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: as.CnfFormula(FALSE atom) should be FALSE\n")
}

# CnfFormula -> CnfFormula (identity)
f_test = CnfFormula(list(as.CnfClause(X %among% "a")))
n_tests = n_tests + 1
r = as.CnfFormula(f_test)
if (!identical(r, f_test)) {
  n_failures = n_failures + 1
  cat("FAIL: as.CnfFormula(CnfFormula) should be identity\n")
}

# logical TRUE -> CnfFormula
n_tests = n_tests + 1
r = as.CnfFormula(TRUE)
if (!isTRUE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: as.CnfFormula(TRUE) should give TRUE formula\n")
}

# logical FALSE -> CnfFormula
n_tests = n_tests + 1
r = as.CnfFormula(FALSE)
if (!isFALSE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: as.CnfFormula(FALSE) should give FALSE formula\n")
}

cat(sprintf("  Coercion paths: %d tests, %d failures\n", n_tests, n_failures))

# === Construction with TRUE/FALSE clauses mixed in ===
cat("\n=== TRUE/FALSE clauses in list ===\n")
set.seed(74003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  clauses = list()
  real_clauses = list()  # only non-TRUE/FALSE clauses
  has_false = FALSE

  for (i in 1:sample(3:6, 1)) {
    what = sample(c("normal", "TRUE", "FALSE"), 1, prob = c(0.6, 0.2, 0.2))
    if (what == "TRUE") {
      clauses[[length(clauses) + 1]] = as.CnfClause(TRUE)
    } else if (what == "FALSE") {
      clauses[[length(clauses) + 1]] = as.CnfClause(FALSE)
      has_false = TRUE
    } else {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) {
        syms[[s]] %among% sample(dom, sample(1:2, 1))
      })
      cl = as.CnfClause(Reduce(`|`, atoms))
      clauses[[length(clauses) + 1]] = cl
      real_clauses[[length(real_clauses) + 1]] = cl
    }
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [tf-mixed-%d]: %s\n", trial, f$message)); next
  }

  if (has_false) {
    # Should be FALSE
    n_tests = n_tests + 1
    if (!isFALSE(as.logical(f))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [tf-mixed-%d]: formula with FALSE clause should be FALSE\n", trial))
    }
  } else if (length(real_clauses) == 0) {
    # All TRUE -> TRUE
    n_tests = n_tests + 1
    if (!isTRUE(as.logical(f))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [tf-mixed-%d]: formula with only TRUE clauses should be TRUE\n", trial))
    }
  } else {
    # Compare with real_clauses only
    f_real = tryCatch(CnfFormula(real_clauses), error = function(e) NULL)
    if (!is.null(f_real)) {
      t1 = evaluate_formula(f, u)
      t2 = evaluate_formula(f_real, u)
      n_tests = n_tests + 1
      if (!all(t1 == t2)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [tf-mixed-%d]: TRUE clauses should not affect result\n", trial))
      }
    }
  }
}
cat(sprintf("  TRUE/FALSE in list: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
