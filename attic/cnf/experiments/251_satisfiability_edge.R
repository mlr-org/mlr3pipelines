#!/usr/bin/env Rscript
# Satisfiability edge case testing:
# Formulas at the boundary of satisfiability:
# - Nearly unsatisfiable (only 1-2 satisfying assignments)
# - Just barely satisfiable after heavy simplification
# - Unsatisfiable formulas that should simplify to FALSE
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

# === Pattern 1: Exactly one satisfying assignment ===
cat("=== One satisfying assignment ===\n")
set.seed(251001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(3:4, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Fix one satisfying assignment
  target = sapply(sym_names, function(s) sample(dom, 1))

  # Create unit clauses that force each variable to its target value
  clauses = list()
  for (i in 1:n_vars) {
    clauses[[i]] = as.CnfClause(syms[[sym_names[i]]] %among% target[i])
  }

  # Add some clauses that are satisfied by the target
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(2:min(3, n_vars), 1)
    chosen = sample(sym_names, n_sym)
    # Include the target values plus some others
    atoms = lapply(chosen, function(s) {
      vals = unique(c(target[s], sample(dom, sample(0:1, 1))))
      syms[[s]] %among% vals
    })
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [onesat-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [onesat-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  One satisfying: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Unsatisfiable via contradicting units ===
cat("\n=== Unsatisfiable via units ===\n")
set.seed(251002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create contradicting units for one symbol
  s_target = sample(sym_names, 1)
  r1 = sample(dom, sample(1:2, 1))
  r2 = sample(setdiff(dom, r1), sample(1:min(2, length(dom) - length(r1)), 1))

  clauses = list(
    as.CnfClause(syms[[s_target]] %among% r1),
    as.CnfClause(syms[[s_target]] %among% r2)
  )

  # Add more random clauses
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
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
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unsat-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Unsatisfiable units: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Near-unsatisfiable via random overconstrained ===
cat("\n=== Near-unsatisfiable random ===\n")
set.seed(251003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b")
  n_vars = sample(3:5, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Generate many clauses (overconstrained)
  n_cl = sample(n_vars * 3, n_vars * 5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:min(3, n_vars), 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < n_vars) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [nearunsat-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [nearunsat-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Near-unsatisfiable: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Unsatisfiable via operations ===
cat("\n=== Unsatisfiable via operations ===\n")
set.seed(251004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # f AND !f should be unsatisfiable
  val = sample(dom, sample(1:2, 1))
  f = CnfFormula(list(as.CnfClause(A %among% val)))

  n_tests = n_tests + 1
  result = tryCatch(f & !f, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [unsatop-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  if (any(truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unsatop-%d]: f & !f is satisfiable\n", trial))
  }
}
cat(sprintf("  Unsatisfiable ops: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
