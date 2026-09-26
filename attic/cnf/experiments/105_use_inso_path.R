#!/usr/bin/env Rscript
# Test the use_inso optimization in register_unit:
# - When units are created DURING the pairwise loop (via SSE or domain restriction)
# - The is_not_subset_of matrix is used to skip unnecessary apply_domain_restriction calls
# - Edge case: unit created from clause that was already in the pairwise matrix
# - Edge case: unit created from clause not yet in the pairwise matrix (meta_idx > meta_idx_outer)
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

# === SSE creates unit during pairwise, triggers use_inso ===
cat("=== SSE creates unit -> use_inso ===\n")
set.seed(105001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Pattern designed to create unit during pairwise:
  # Clause 1: A %among% c("0","1") | B %among% "0"
  # Clause 2: A %among% c("0","2") | B %among% "0"
  # After SSE on A: Clause 1 becomes A %among% "0" | B %among% "0" (not yet unit)
  # Then: A %among% "0" | B %among% c("0","1") -- could subsume clause 1
  a1 = sample(dom, 2)
  a2_extra = setdiff(dom, a1)
  a2 = c(a1[1], a2_extra)

  b_val = sample(dom, 1)

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b_val),
    as.CnfClause(A %among% a2 | B %among% b_val),
    as.CnfClause(C %among% sample(dom, sample(1:2, 1)) | B %among% sample(dom, sample(1:2, 1)))
  )

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sse-unit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sse-unit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE creates unit: %d tests, %d failures\n", n_tests, n_failures))

# === Multiple units with overlapping symbols ===
cat("\n=== Multiple units overlapping ===\n")
set.seed(105002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  n_vars = sample(4:6, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create formula with some clauses that become units through SSE
  clauses = list()
  n_clauses = sample(4:8, 1)
  for (j in 1:n_clauses) {
    # Mix of narrow and wide clauses
    n_sym = sample(1:min(3, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      n_vals = sample(1:(length(dom) - 1), 1)
      syms[[s]] %among% sample(dom, n_vals)
    })
    clauses[[j]] = as.CnfClause(Reduce(`|`, atoms))
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [overlap-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [overlap-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multiple units overlapping: %d tests, %d failures\n", n_tests, n_failures))

# === Units with unit_domains intersection (register_unit duplicate path) ===
cat("\n=== Duplicate unit registration ===\n")
set.seed(105003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2", "3")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Two units for the same symbol with overlapping domains
  a_val1 = sample(dom, sample(1:3, 1))
  a_val2 = sample(dom, sample(1:3, 1))
  isct = intersect(a_val1, a_val2)

  clauses = list(
    as.CnfClause(A %among% a_val1),
    as.CnfClause(A %among% a_val2),
    as.CnfClause(B %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1)))
  )

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dupunit-%d]: %s\n", trial, result$message)); next
  }

  if (length(isct) == 0) {
    # Should be FALSE
    if (!isFALSE(as.logical(result))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [dupunit-%d]: disjoint units should be FALSE\n", trial))
    }
  } else {
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [dupunit-%d]: semantic mismatch\n", trial))
    }
  }
}
cat(sprintf("  Duplicate unit registration: %d tests, %d failures\n", n_tests, n_failures))

# === Contradiction detection via use_inso ===
cat("\n=== Contradiction via unit propagation ===\n")
set.seed(105004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Create contradictory formulas
  # A = "a" and (A %among% c("b","c") | B = "b") -> forces B = "b"
  # Then B %among% c("a","c") -> contradiction
  clauses = list(
    as.CnfClause(A %among% "a"),
    as.CnfClause(A %among% c("b", "c") | B %among% "b"),
    as.CnfClause(B %among% sample(setdiff(dom, "b"), sample(1:2, 1))),
    as.CnfClause(C %among% sample(dom, sample(1:2, 1)))
  )

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [contr-%d]: %s\n", trial, result$message)); next
  }

  # Should be FALSE
  if (!isFALSE(as.logical(result))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [contr-%d]: should be FALSE (contradiction)\n", trial))
  }
}
cat(sprintf("  Contradiction detection: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
