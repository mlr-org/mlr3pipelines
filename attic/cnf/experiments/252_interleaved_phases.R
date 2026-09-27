#!/usr/bin/env Rscript
# Interleaved phase testing:
# Test formulas where different simplification phases interact:
# - Unit propagation during pairwise loop
# - SSE creating new units that affect later pairwise comparisons
# - 2nd-order SSE modifying clauses during the trigger loop
# These are the most complex interactions in the simplifier.
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

# === Pattern 1: Formulas designed to cascade across phases ===
cat("=== Cross-phase cascading ===\n")
set.seed(252001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Layer 1: Units
  clauses = list()
  n_units = sample(1:2, 1)
  for (k in 1:n_units) {
    s = sample(sym_names[1:2], 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
  }

  # Layer 2: 2-symbol clauses that could become units after propagation
  for (k in 1:sample(2:4, 1)) {
    pair = sample(sym_names, 2)
    atoms = lapply(pair, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # Layer 3: 3-symbol clauses that could trigger SSE or 2nd-order SSE
  for (k in 1:sample(2:4, 1)) {
    triple = sample(sym_names, 3)
    atoms = lapply(triple, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [xphase-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [xphase-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Cross-phase cascading: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Formulas where non-unit processing creates units ===
cat("\n=== Non-unit to unit transition ===\n")
set.seed(252002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  clauses = list()
  # No explicit units, but 2-symbol clauses where SSE could make one a unit
  for (k in 1:sample(6:12, 1)) {
    pair = sample(sym_names, 2)
    atoms = lapply(pair, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [nu2u-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [nu2u-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Non-unit to unit: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Dense interconnected formulas ===
cat("\n=== Dense interconnected ===\n")
set.seed(252003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Every pair of variables appears in at least one clause
  clauses = list()
  pairs = combn(sym_names, 2, simplify = FALSE)
  for (pair in pairs) {
    atoms = lapply(pair, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # Add some 3-symbol clauses
  for (k in 1:sample(2:4, 1)) {
    triple = sample(sym_names, 3)
    atoms = lapply(triple, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dense-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dense-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Dense interconnected: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Formulas that should simplify to a single clause or TRUE ===
cat("\n=== Heavy simplification ===\n")
set.seed(252004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Start with a base clause
  base_syms = sample(sym_names, sample(1:2, 1))
  base_ranges = lapply(base_syms, function(s) sample(dom, sample(1:2, 1)))
  names(base_ranges) = base_syms

  clauses = list()
  # Add the base clause
  atoms = lapply(base_syms, function(s) syms[[s]] %among% base_ranges[[s]])
  clauses[[1]] = as.CnfClause(Reduce(`|`, atoms))

  # Add clauses that are supersets of the base (should be subsumed)
  for (k in 1:sample(2:4, 1)) {
    extra_syms = sample(sym_names, sample(1:2, 1))
    atoms = c(
      lapply(base_syms, function(s) syms[[s]] %among% unique(c(base_ranges[[s]], sample(dom, sample(0:1, 1))))),
      lapply(setdiff(extra_syms, base_syms), function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    )
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # Add some random clauses
  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [heavy-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [heavy-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Heavy simplification: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
