#!/usr/bin/env Rscript
# Test eliminate_symbol_from_clause (lines 227-284):
# - When symbol elimination changes is_not_subset_of rows in both directions
# - When rows_changed includes clauses that then get eliminated during the loop
# - When others_ref_this_symbol includes stale entries (units, eliminated)
# - When meta_idx > meta_idx_outer (early return before matrix exists)
# Also test interaction between eliminate_symbol_from_clause and on_updated_subset_relations
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

# === Symbol elimination that cascades through on_updated_subset_relations ===
cat("=== Symbol elimination + on_updated_subset_relations ===\n")
set.seed(149001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  # Construct: Unit restricts clause, emptying one symbol, which triggers
  # eliminate_symbol_from_clause, which updates matrices, which triggers
  # on_updated_subset_relations for other clauses
  unit_sym = sample(c("A", "B", "C", "D"), 1)
  unit_val = sample(dom, sample(1:2, 1))
  syms = list(A = A, B = B, C = C, D = D)

  clauses = list(
    as.CnfClause(syms[[unit_sym]] %among% unit_val)
  )
  # Add 3-5 multi-symbol clauses sharing symbols
  n_multi = sample(3:5, 1)
  for (j in 1:n_multi) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [elim-cascade-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [elim-cascade-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Elimination cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Multiple symbols eliminated from same clause in sequence ===
cat("\n=== Sequential symbol elimination ===\n")
set.seed(149002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Multiple units that together strip symbols from a clause
  n_units = sample(2:3, 1)
  unit_syms = sample(names(syms), n_units)
  clauses = lapply(unit_syms, function(s) {
    as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
  })

  # Long clauses that overlap with units
  n_long = sample(2:4, 1)
  for (j in 1:n_long) {
    n_sym = sample(3:4, 1)
    # Ensure clause overlaps with at least 2 units
    chosen = unique(c(sample(unit_syms, min(2, n_units)), sample(names(syms), n_sym - 2)))
    chosen = chosen[1:min(n_sym, length(chosen))]
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [seq-elim-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [seq-elim-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Sequential elimination: %d tests, %d failures\n", n_tests, n_failures))

# === Symbol elimination creating new unit during pairwise phase ===
cat("\n=== Symbol elim -> new unit during pairwise ===\n")
set.seed(149003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Small domain maximizes chance of symbol elimination creating units
  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    # Use small ranges to increase conflict
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [elim-unit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [elim-unit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Symbol elim -> unit: %d tests, %d failures\n", n_tests, n_failures))

# === Clauses added late (high meta_idx) getting symbols eliminated ===
cat("\n=== Late clause symbol elimination ===\n")
set.seed(149004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Short clauses first (will be processed early), long clauses last
  clauses = list()
  # 2-3 short clauses (2 symbols)
  for (j in 1:sample(2:3, 1)) {
    chosen = sample(names(syms), 2)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  # 2-3 long clauses (3-4 symbols)
  for (j in 1:sample(2:3, 1)) {
    chosen = sample(names(syms), sample(3:4, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [late-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [late-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Late clause elim: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
