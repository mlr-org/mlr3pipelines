#!/usr/bin/env Rscript
# Test HLA unit phase with roe_inverse (delayedAssign path):
# - Units with many non-unit clauses sharing the unit symbol
# - Multiple units interacting with HLA
# - Non-unit HLA eliminates clauses, then unit HLA uses updated remaining_nonunit_entries
# - Exercise the roe_inverse match() for correctness
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

# === Pattern 1: Unit + many clauses sharing unit symbol ===
cat("=== Unit HLA with many shared clauses ===\n")
set.seed(182001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create a unit for A
  a_range = sample(dom, sample(1:3, 1))
  clauses = list(as.CnfClause(A %among% a_range))

  # Many clauses using A plus other symbols
  for (j in 1:sample(4:8, 1)) {
    n_sym = sample(2:3, 1)
    chosen = c("A", sample(c("B", "C"), n_sym - 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [uHLA1-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [uHLA1-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Unit HLA shared: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Multiple units, HLA on each ===
cat("\n=== Multiple units HLA ===\n")
set.seed(182002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  clauses = list()
  # 2-3 units
  unit_syms = sample(names(syms), sample(2:3, 1))
  for (s in unit_syms) {
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
  }
  # Non-unit clauses
  for (j in 1:sample(4:8, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [uHLA2-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [uHLA2-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multiple units HLA: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Non-unit HLA eliminates clauses, then unit HLA ===
cat("\n=== Non-unit then unit HLA ===\n")
set.seed(182003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Unit
  clauses = list(as.CnfClause(A %among% sample(dom, sample(2:4, 1))))

  # Clauses designed for non-unit HLA (pairs differing in one symbol)
  b1 = sample(dom, 2)
  c1 = sample(dom, 2)
  c2 = sample(dom, 3)
  clauses[[length(clauses) + 1]] = as.CnfClause(B %among% b1 | C %among% c1)
  clauses[[length(clauses) + 1]] = as.CnfClause(B %among% b1 | C %among% c2)

  # More random clauses
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [nuuHLA-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [nuuHLA-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Non-unit then unit HLA: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Large domain unit HLA ===
cat("\n=== Large domain unit HLA ===\n")
set.seed(182004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = paste0("v", 1:8)
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Unit
  clauses = list(as.CnfClause(A %among% sample(dom, sample(2:6, 1))))
  # Clauses with A
  for (j in 1:sample(4:8, 1)) {
    n_sym = sample(2:3, 1)
    chosen = c("A", sample(c("B", "C"), n_sym - 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:5, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ldHLA-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ldHLA-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Large domain unit HLA: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
