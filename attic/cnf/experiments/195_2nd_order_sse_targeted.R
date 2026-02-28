#!/usr/bin/env Rscript
# Targeted 2nd-order SSE tests:
# Construct formulas specifically designed to trigger 2nd-order self-subsumption.
# The 2nd-order SSE applies when:
# - clause A is a subset of clause T in all symbols except s1
# - clause B is a subset of clause T in all symbols except s1 and s2
# - A and B are disjoint on s1 outside of T
# Then T's s2 range can be restricted to union(A's s2, B's s2)
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

# === Pattern 1: Direct 2nd-order SSE construction ===
cat("=== Direct 2nd-order SSE ===\n")
set.seed(195001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create a target clause T: (A in a_range | B in b_range | C in c_range)
  a_target = sample(dom, sample(2:4, 1))
  b_target = sample(dom, sample(2:4, 1))
  c_target = sample(dom, sample(2:4, 1))

  # symbol_intersect = A, symbol_target = B
  # Create oneend: (A in a_oneend | B in b_oneend | C in c_sub)
  #   where c_sub ⊆ c_target (so C is a subset)
  #   and a_oneend ⊆ a_target is NOT required (oneend differs in A)
  #   and b_oneend ⊆ b_target is NOT required (but we track it separately)
  c_sub = sample(c_target, min(length(c_target), sample(1:length(c_target), 1)))
  a_oneend = sample(dom, sample(1:3, 1))
  b_oneend = sample(dom, sample(1:3, 1))

  # Create twoend: (A in a_twoend | B in b_twoend | C in c_sub2)
  #   where c_sub2 ⊆ c_target
  #   and differs from target in both A and B
  c_sub2 = sample(c_target, min(length(c_target), sample(1:length(c_target), 1)))
  a_twoend = sample(dom, sample(1:3, 1))
  b_twoend = sample(dom, sample(1:3, 1))

  # Disjointness: a_oneend and a_twoend should be disjoint outside a_target
  # (this is the condition for 2nd-order SSE)

  clauses = list(
    as.CnfClause(A %among% a_target | B %among% b_target | C %among% c_target),
    as.CnfClause(A %among% a_oneend | B %among% b_oneend | C %among% c_sub),
    as.CnfClause(A %among% a_twoend | B %among% b_twoend | C %among% c_sub2)
  )

  # Add more random clauses for variety
  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [2sse-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [2sse-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Direct 2nd-order SSE: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Cascading 2nd-order SSE ===
cat("\n=== Cascading 2nd-order SSE ===\n")
set.seed(195002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Multiple clause groups that can each trigger 2nd-order SSE
  clauses = list()
  for (group in 1:sample(2:3, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    # target clause
    target_ranges = lapply(chosen, function(s) sample(dom, sample(2:3, 1)))
    names(target_ranges) = chosen
    target_atoms = lapply(chosen, function(s) syms[[s]] %among% target_ranges[[s]])
    cl_target = as.CnfClause(Reduce(`|`, target_atoms))
    if (!isTRUE(unclass(cl_target))) clauses[[length(clauses) + 1]] = cl_target

    # Create 2-3 helper clauses
    for (h in 1:sample(2:3, 1)) {
      helper_atoms = lapply(chosen, function(s) {
        r = target_ranges[[s]]
        # sometimes keep subset, sometimes not
        if (sample(c(TRUE, FALSE), 1)) {
          syms[[s]] %among% sample(r, max(1, sample(1:length(r), 1)))
        } else {
          syms[[s]] %among% sample(dom, sample(1:3, 1))
        }
      })
      cl_helper = as.CnfClause(Reduce(`|`, helper_atoms))
      if (!isTRUE(unclass(cl_helper))) clauses[[length(clauses) + 1]] = cl_helper
    }
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cascade2-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cascade2-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Cascading 2nd-order: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: 2nd-order SSE with 2-symbol clauses ===
cat("\n=== 2nd-order with 2-symbol clauses ===\n")
set.seed(195003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  # With only 2 symbols, 2nd-order SSE is interesting because:
  # oneend differs in 1 symbol, twoend differs in both
  # The disjointness condition is on one symbol
  n_clauses = sample(5:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    a_range = sample(dom, sample(1:4, 1))
    b_range = sample(dom, sample(1:4, 1))
    as.CnfClause(A %among% a_range | B %among% b_range)
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [2sym2nd-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [2sym2nd-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2-symbol 2nd-order: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: 2nd-order with units + non-units ===
cat("\n=== 2nd-order with units ===\n")
set.seed(195004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Units restrict the domain; after restriction, 2nd-order SSE may apply
  clauses = list()
  # Add a unit
  unit_sym = sample(names(syms), 1)
  unit_range = sample(dom, sample(2:3, 1))
  clauses[[1]] = as.CnfClause(syms[[unit_sym]] %among% unit_range)

  # Non-unit clauses that overlap with the unit
  for (j in 1:sample(4:7, 1)) {
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
    cat(sprintf("ERROR [unit2nd-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unit2nd-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2nd-order with units: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
