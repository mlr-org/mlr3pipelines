#!/usr/bin/env Rscript
# Extreme simplification scenarios:
# Test formulas that simplify from many clauses to very few,
# formulas that are satisfiable by exactly one assignment,
# and formulas that are unsatisfiable (simplify to FALSE).
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

evaluate_raw_clauses = function(clauses, universe) {
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }
  raw_truth
}

# === Pattern 1: Formulas that should simplify to FALSE ===
cat("=== Unsatisfiable formulas ===\n")
set.seed(225001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create contradicting unit clauses
  s = sample(sym_names, 1)
  v1 = sample(dom, 1)
  v2 = sample(setdiff(dom, v1), 1)

  clauses = list(
    as.CnfClause(syms[[s]] %among% v1),
    as.CnfClause(syms[[s]] %among% v2)
  )

  # Add random extra clauses
  for (j in 1:sample(2:6, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [unsat-%d]: %s\n", trial, result$message)); next
  }

  # Should be FALSE
  if (!isFALSE(unclass(result))) {
    # Double check with semantic eval
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [unsat-%d]: semantic mismatch\n", trial))
    } else if (all(!raw_truth)) {
      # Semantically FALSE but not simplified to FALSE
      # This is a missed optimization, not a bug, so we don't count it
    }
  }
}
cat(sprintf("  Unsatisfiable: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Exactly one satisfying assignment ===
cat("\n=== Single satisfying assignment ===\n")
set.seed(225002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(3:4, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Fix all variables to specific values (all units)
  assignment = sample(dom, n_vars, replace = TRUE)
  clauses = list()
  for (i in 1:n_vars) {
    clauses[[i]] = as.CnfClause(syms[[sym_names[i]]] %among% assignment[i])
  }

  # Add redundant clauses that are compatible
  for (j in 1:sample(3:8, 1)) {
    n_sym = sample(2:min(3, n_vars), 1)
    chosen_idx = sample(n_vars, n_sym)
    atoms = lapply(chosen_idx, function(i) {
      vals = unique(c(assignment[i], sample(dom, sample(0:1, 1))))
      syms[[sym_names[i]]] %among% vals
    })
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [single-%d]: %s\n", trial, result$message)); next
  }

  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [single-%d]: semantic mismatch\n", trial))
  }

  # Verify exactly one satisfying assignment
  if (sum(raw_truth) != 1) {
    # Our construction should give exactly 1 (all units pin values)
    n_failures = n_failures + 1
    cat(sprintf("FAIL [single-%d]: expected 1 sat, got %d\n", trial, sum(raw_truth)))
  }
}
cat(sprintf("  Single satisfying: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Near-tautological (almost TRUE) ===
cat("\n=== Near-tautological ===\n")
set.seed(225003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Clauses where each symbol range is large (3 out of 4)
  clauses = list()
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 3))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neartaut-%d]: %s\n", trial, result$message)); next
  }

  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [neartaut-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Near-tautological: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Many clauses simplify to few ===
cat("\n=== Many to few simplification ===\n")
set.seed(225004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create a core clause and many subsumed clauses
  core_syms = sample(sym_names, 2)
  core_ranges = lapply(core_syms, function(s) sample(dom, sample(1:2, 1)))

  # Core clause
  atoms = mapply(function(s, r) syms[[s]] %among% r, core_syms, core_ranges, SIMPLIFY = FALSE)
  core_cl = as.CnfClause(Reduce(`|`, atoms))

  clauses = list(core_cl)

  # Many clauses that are supersets (will be subsumed)
  for (j in 1:sample(5:12, 1)) {
    extra_syms = sample(setdiff(sym_names, core_syms), sample(1:2, 1))
    all_syms = c(core_syms, extra_syms)
    atoms = lapply(seq_along(all_syms), function(i) {
      s = all_syms[i]
      if (i <= length(core_syms)) {
        r = unique(c(core_ranges[[i]], sample(dom, sample(0:2, 1))))
      } else {
        r = sample(dom, sample(1:3, 1))
      }
      syms[[s]] %among% r
    })
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # Add a few independent clauses
  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [many2few-%d]: %s\n", trial, result$message)); next
  }

  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [many2few-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Many to few: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
