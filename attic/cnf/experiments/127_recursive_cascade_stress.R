#!/usr/bin/env Rscript
# Stress test the recursive cascade path:
# apply_domain_restriction -> on_update_range -> handle_sse_2nd_order_* -> try_sse_2nd_order -> apply_domain_restriction
# This recursive path is the deepest calling chain in the simplifier.
# We construct formulas that maximize the chance of this cascade triggering.
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

# === Dense 3-var quaternary: many overlapping clauses ===
cat("=== Dense 3-var quaternary ===\n")
set.seed(127001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  syms = list(A = A, B = B, C = C)

  # Create many 2-symbol clauses (high interaction density)
  n_clauses = sample(6:12, 1)
  clauses = lapply(1:n_clauses, function(j) {
    chosen = sample(names(syms), 2)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

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
cat(sprintf("  Dense 3v quaternary: %d tests, %d failures\n", n_tests, n_failures))

# === 4-var ternary with unit + SSE cascade triggers ===
cat("\n=== 4-var cascade triggers ===\n")
set.seed(127002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  syms = list(A = A, B = B, C = C, D = D)

  # Start with a unit to trigger cascade
  unit_sym = sample(names(syms), 1)
  unit_vals = sample(dom, sample(1:2, 1))
  clauses = list(as.CnfClause(syms[[unit_sym]] %among% unit_vals))

  # Add clauses that share symbols with each other (chain-like)
  pairs = list(c("A", "B"), c("B", "C"), c("C", "D"), c("A", "D"))
  for (p in sample(pairs, sample(3:4, 1))) {
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[p[1]]] %among% sample(dom, sample(1:2, 1)) |
      syms[[p[2]]] %among% sample(dom, sample(1:2, 1))
    )
  }
  # Add some 3-symbol clauses
  for (j in 1:sample(1:3, 1)) {
    chosen = sample(names(syms), 3)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [4v-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [4v-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  4v cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Constructed 2nd-order SSE with disjoint intersection ===
cat("\n=== 2nd-order disjoint intersection ===\n")
set.seed(127003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  syms = list(A = A, B = B, C = C)

  # Construct: oneend and twoend that share a symbol with disjoint ranges
  # This should trigger try_sse_2nd_order
  a_split = sample(dom, sample(2:3, 1))
  a_comp = setdiff(dom, a_split)

  clauses = list(
    # oneend: A in a_split | B in something
    as.CnfClause(A %among% a_split | B %among% sample(dom, sample(1:4, 1))),
    # twoend: A in a_comp | B in something_else
    as.CnfClause(A %among% a_comp | B %among% sample(dom, sample(1:4, 1))),
    # target: A in subset | C in something
    as.CnfClause(A %among% sample(dom, sample(1:4, 1)) | C %among% sample(dom, sample(1:4, 1)))
  )
  # Add more clauses for interaction
  for (j in 1:sample(1:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [disjoint-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [disjoint-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2nd-order disjoint: %d tests, %d failures\n", n_tests, n_failures))

# === Very many clauses with same 2 symbols (max SSE opportunity) ===
cat("\n=== Many same-symbol clauses ===\n")
set.seed(127004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e", "f")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Many clauses all using A and B
  n_clauses = sample(8:15, 1)
  clauses = lapply(1:n_clauses, function(j) {
    as.CnfClause(
      A %among% sample(dom, sample(1:5, 1)) |
      B %among% sample(dom, sample(1:5, 1))
    )
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [many-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [many-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Many same-symbol: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
