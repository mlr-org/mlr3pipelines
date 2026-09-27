#!/usr/bin/env Rscript
# Order sensitivity testing:
# The simplifier is non-confluent, meaning different clause orderings
# produce structurally different (but semantically equivalent) results.
# Test that all permutations give the same truth table.
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

# === Pattern 1: Small clause sets, all permutations ===
cat("=== All permutations of small sets ===\n")
set.seed(247001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create 3-4 clauses
  n_cl = sample(3:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  raw_truth = evaluate_raw_clauses(clauses, u)

  # Test all permutations
  perms = if (length(clauses) == 3) {
    list(c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1))
  } else {
    # Just sample some permutations for n=4
    lapply(1:12, function(x) sample(length(clauses)))
  }

  failed = FALSE
  for (perm in perms) {
    result = tryCatch(CnfFormula(clauses[perm]), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [perm-%d]: %s\n", trial, result$message))
      failed = TRUE; break
    }
    truth = evaluate_formula(result, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [perm-%d]: ordering %s gives wrong result\n", trial, paste(perm, collapse=",")))
      failed = TRUE; break
    }
  }
}
cat(sprintf("  All permutations: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Larger sets, random permutations ===
cat("\n=== Random permutations of larger sets ===\n")
set.seed(247002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  raw_truth = evaluate_raw_clauses(clauses, u)

  # Test 10 random permutations
  for (k in 1:10) {
    perm = sample(length(clauses))
    result = tryCatch(CnfFormula(clauses[perm]), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [rperm-%d-%d]: %s\n", trial, k, result$message)); break
    }
    truth = evaluate_formula(result, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [rperm-%d-%d]: permutation gives wrong result\n", trial, k)); break
    }
  }
}
cat(sprintf("  Random permutations: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Reverse ordering ===
cat("\n=== Reverse ordering ===\n")
set.seed(247003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  raw_truth = evaluate_raw_clauses(clauses, u)

  # Forward order
  result_fwd = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result_fwd, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [rev-%d-fwd]: %s\n", trial, result_fwd$message)); next
  }
  # Reverse order
  result_rev = tryCatch(CnfFormula(rev(clauses)), error = function(e) e)
  if (inherits(result_rev, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [rev-%d-rev]: %s\n", trial, result_rev$message)); next
  }

  truth_fwd = evaluate_formula(result_fwd, u)
  truth_rev = evaluate_formula(result_rev, u)

  if (!all(truth_fwd == raw_truth) || !all(truth_rev == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [rev-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Reverse ordering: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Duplicate clauses in different positions ===
cat("\n=== Duplicate clauses ===\n")
set.seed(247004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  # Duplicate some clauses
  dup_idx = sample(length(clauses), sample(1:min(2, length(clauses)), 1))
  clauses_with_dup = c(clauses, clauses[dup_idx])

  n_tests = n_tests + 1
  raw_truth = evaluate_raw_clauses(clauses, u)

  result_orig = tryCatch(CnfFormula(clauses), error = function(e) e)
  result_dup = tryCatch(CnfFormula(clauses_with_dup), error = function(e) e)

  if (inherits(result_orig, "error") || inherits(result_dup, "error")) {
    n_failures = n_failures + 1
    msg = if (inherits(result_orig, "error")) result_orig$message else result_dup$message
    cat(sprintf("ERROR [dup-%d]: %s\n", trial, msg)); next
  }

  truth_orig = evaluate_formula(result_orig, u)
  truth_dup = evaluate_formula(result_dup, u)

  if (!all(truth_orig == raw_truth) || !all(truth_dup == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dup-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Duplicates: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
