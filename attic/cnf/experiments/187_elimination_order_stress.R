#!/usr/bin/env Rscript
# Test clause elimination order stress:
# - The pairwise loop processes clauses in order of meta_idx_outer
# - Elimination during the loop can affect later pairs
# - Check that permutations of the same clauses give semantically equivalent results
# Also: stress test with clauses that trigger many different phases
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

# === Pattern 1: Same clauses, many different orderings ===
cat("=== Permutation invariance (semantic) ===\n")
set.seed(187001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  raw_truth = evaluate_raw_clauses(clauses, u)

  # Try multiple permutations
  n_perms = 5
  all_ok = TRUE
  for (p in 1:n_perms) {
    perm = sample(length(clauses))
    n_tests = n_tests + 1
    result = tryCatch(CnfFormula(clauses[perm]), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [perm-%d-%d]: %s\n", trial, p, result$message))
      all_ok = FALSE
      next
    }
    truth = evaluate_formula(result, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [perm-%d-%d]: semantic mismatch\n", trial, p))
      all_ok = FALSE
    }
  }
}
cat(sprintf("  Permutation: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Clauses that exercise all phases simultaneously ===
cat("\n=== All-phases formulas ===\n")
set.seed(187002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  clauses = list()
  # Unit for unit propagation
  clauses[[1]] = as.CnfClause(syms[[sample(names(syms), 1)]] %among% sample(dom, sample(1:3, 1)))
  # Subsumable pair
  sub_syms = sample(names(syms), 2)
  sub_ranges = lapply(sub_syms, function(s) sample(dom, sample(2:3, 1)))
  clauses[[2]] = as.CnfClause(Reduce(`|`, lapply(1:2, function(i) syms[[sub_syms[i]]] %among% sub_ranges[[i]])))
  clauses[[3]] = as.CnfClause(Reduce(`|`, lapply(1:2, function(i) syms[[sub_syms[i]]] %among% sample(sub_ranges[[i]], min(length(sub_ranges[[i]]), sample(1:2, 1))))))
  # SSE pair
  sse_syms = sample(names(syms), 2)
  clauses[[4]] = as.CnfClause(syms[[sse_syms[1]]] %among% sample(dom, 2) | syms[[sse_syms[2]]] %among% sample(dom, 2))
  clauses[[5]] = as.CnfClause(syms[[sse_syms[1]]] %among% sample(dom, 2) | syms[[sse_syms[2]]] %among% sample(dom, 2))
  # More random clauses
  for (j in 1:sample(3:6, 1)) {
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
    cat(sprintf("ERROR [allph-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [allph-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  All-phases: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Very many clauses, few variables ===
cat("\n=== Very many clauses ===\n")
set.seed(187003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(20:40, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 10) next

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
cat(sprintf("  Very many clauses: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
