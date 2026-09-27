#!/usr/bin/env Rscript
# Test 2nd-order SSE where oneend clause doesn't have symbol_target.
# This exercises the case where char_union(NULL, twoend_range) = twoend_range
# in try_sse_2nd_order, and also the is_not_subset_of matrix where
# is_not_subset_of[[target]][oneend, symbol_target] = TRUE because
# the symbol is absent from the oneend clause.
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

# === Constructed case: oneend has only symbol_intersect, not symbol_target ===
cat("=== Oneend missing symbol_target ===\n")
set.seed(145001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # oneend: A in a1 (single symbol, has only A = symbol_intersect, NOT B = symbol_target)
  # twoend: A in a2 | B in b2 (has both A = symbol_intersect and B = symbol_target)
  # target: A in at | B in bt | C in ct (has all three)
  # For oneend to have not_subset_count[oneend, target] = 1:
  #   A: a1 not subset of at -> TRUE
  # For twoend to have not_subset_count[twoend, target] = 2:
  #   A: a2 not subset of at -> TRUE
  #   B: b2 not subset of bt -> TRUE

  a1 = sample(dom, sample(1:2, 1))
  a2 = sample(dom, sample(1:3, 1))
  b2 = sample(dom, sample(1:3, 1))
  at = sample(dom, sample(1:3, 1))
  bt = sample(dom, sample(1:3, 1))
  ct = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1),
    as.CnfClause(A %among% a2 | B %among% b2),
    as.CnfClause(A %among% at | B %among% bt | C %among% ct)
  )
  # Add an extra clause to avoid trivial simplification
  extra_a = sample(dom, sample(1:3, 1))
  extra_c = sample(dom, sample(1:3, 1))
  clauses[[4]] = as.CnfClause(A %among% extra_a | C %among% extra_c)

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [missing-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [missing-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Missing symbol_target: %d tests, %d failures\n", n_tests, n_failures))

# === Twoend missing symbol in oneend ===
cat("\n=== Asymmetric symbol sets ===\n")
set.seed(145002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  # Vary which symbols are in each clause
  # Some clauses have many symbols, others have few
  n_cl = sample(4:7, 1)
  syms = list(A = A, B = B, C = C, D = D)
  all_names = names(syms)

  clauses = lapply(1:n_cl, function(j) {
    if (j <= 2) {
      # Short clauses (1-2 symbols)
      n_sym = sample(1:2, 1)
    } else {
      # Longer clauses (2-4 symbols)
      n_sym = sample(2:4, 1)
    }
    chosen = sample(all_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [asym-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [asym-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Asymmetric symbol sets: %d tests, %d failures\n", n_tests, n_failures))

# === Many single-symbol (unit) clauses mixed with multi-symbol clauses ===
cat("\n=== Units + multi-symbol for 2nd-order SSE ===\n")
set.seed(145003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # 1-2 unit clauses
  n_units = sample(1:2, 1)
  unit_syms = sample(names(syms), n_units)
  clauses = lapply(unit_syms, function(s) {
    as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
  })

  # 3-6 multi-symbol clauses
  n_multi = sample(3:6, 1)
  for (j in 1:n_multi) {
    chosen = sample(names(syms), sample(2:4, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [unit-2nd-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unit-2nd-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Units + 2nd-order: %d tests, %d failures\n", n_tests, n_failures))

# === Random with 5 vars domain 3 (many symbol mismatches) ===
cat("\n=== 5 vars domain 3 ===\n")
set.seed(145004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (i in 1:5) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(4:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [5v3d-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [5v3d-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  5 vars domain 3: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
