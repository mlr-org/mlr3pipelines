#!/usr/bin/env Rscript
# remaining_entries computation edge cases:
# - All entries eliminated except one
# - All entries become units
# - Mix of eliminated and unit entries
# - remaining_nonunit_entries is empty
# - remaining_unit_entries is empty
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

# === Pattern 1: All clauses subsumed by units ===
cat("=== All subsumed by units ===\n")
set.seed(216001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Units
  a_val = sample(dom, sample(1:2, 1))
  b_val = sample(dom, sample(1:2, 1))
  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(B %among% b_val)
  )

  # Clauses that will be subsumed after unit propagation
  # (they contain A and B ranges that are supersets of the units)
  for (j in 1:sample(3:6, 1)) {
    a_range = unique(c(a_val, sample(dom, sample(0:2, 1))))
    b_range = unique(c(b_val, sample(dom, sample(0:2, 1))))
    c_range = sample(dom, sample(1:3, 1))
    cl = as.CnfClause(A %among% a_range | B %among% b_range | C %among% c_range)
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [allsub-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [allsub-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  All subsumed: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Pairwise subsumption eliminates most clauses ===
cat("\n=== Pairwise subsumption heavy ===\n")
set.seed(216002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create a "base" clause and many clauses that subsume it
  base_a = sample(dom, sample(2:3, 1))
  base_b = sample(dom, sample(2:3, 1))
  base_c = sample(dom, sample(2:3, 1))

  clauses = list(
    as.CnfClause(A %among% base_a | B %among% base_b | C %among% base_c)
  )

  # Clauses with subsets of base ranges (these subsume the base)
  for (j in 1:sample(3:6, 1)) {
    a_sub = sample(base_a, sample(1:length(base_a), 1))
    b_sub = sample(base_b, sample(1:length(base_b), 1))
    n_sym = sample(2:3, 1)
    if (n_sym == 2) {
      cl = as.CnfClause(A %among% a_sub | B %among% b_sub)
    } else {
      c_sub = sample(base_c, sample(1:length(base_c), 1))
      cl = as.CnfClause(A %among% a_sub | B %among% b_sub | C %among% c_sub)
    }
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # Additional random clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [pairsub-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [pairsub-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Pairwise subsumption: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: SSE eliminates all symbols from multiple clauses ===
cat("\n=== SSE eliminates all symbols ===\n")
set.seed(216003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Clauses designed so SSE progressively eliminates symbols
  a1 = sample(dom, sample(1:2, 1))
  a2 = sample(dom, sample(1:2, 1))
  b1 = sample(dom, sample(1:2, 1))
  c1 = sample(dom, sample(1:2, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a2 | B %among% b1),
    as.CnfClause(A %among% a1 | C %among% c1),
    as.CnfClause(A %among% a2 | C %among% c1)
  )

  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sseallelim-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sseallelim-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE eliminates all: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Formulas that simplify to exactly one clause ===
cat("\n=== Simplify to single clause ===\n")
set.seed(216004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # A "core" clause
  core_a = sample(dom, sample(1:3, 1))
  core_b = sample(dom, sample(1:3, 1))
  core_clause = as.CnfClause(A %among% core_a | B %among% core_b)

  # Clauses that all subsume the core (wider ranges)
  clauses = list(core_clause)
  for (j in 1:sample(3:6, 1)) {
    a_super = unique(c(core_a, sample(dom, sample(0:2, 1))))
    b_super = unique(c(core_b, sample(dom, sample(0:2, 1))))
    c_range = sample(dom, sample(1:3, 1))
    cl = as.CnfClause(A %among% a_super | B %among% b_super | C %among% c_range)
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

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
}
cat(sprintf("  Single clause: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
