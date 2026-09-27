#!/usr/bin/env Rscript
# Test: multiple unit clauses for the same symbol.
# When two units share a symbol, register_unit intersects their domains
# and eliminates the second one. Verify this works correctly across many
# configurations, including cascading from SSE that creates duplicate units.
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

# === Two initial units for the same symbol ===
cat("=== Two initial units same symbol ===\n")
set.seed(135001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Two unit clauses for A with overlapping but different ranges
  a1 = sample(dom, sample(1:3, 1))
  a2 = sample(dom, sample(1:3, 1))
  isct = intersect(a1, a2)
  if (length(isct) == 0) next  # would be contradiction, skip for now

  # Add non-unit clauses to prevent early return
  b_vals = sample(dom, sample(1:3, 1))
  clauses = list(
    as.CnfClause(A %among% a1),
    as.CnfClause(A %among% a2),
    as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | B %among% b_vals)
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dup-unit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dup-unit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Dup units: %d tests, %d failures\n", n_tests, n_failures))

# === Two initial units + contradiction path ===
cat("\n=== Contradicting unit pairs ===\n")
set.seed(135002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Two unit clauses for A with disjoint ranges -> contradiction
  a1 = sample(dom, 1)
  a2 = sample(setdiff(dom, a1), 1)

  clauses = list(
    as.CnfClause(A %among% a1),
    as.CnfClause(A %among% a2),
    as.CnfClause(B %among% sample(dom, sample(1:2, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [contr-%d]: %s\n", trial, result$message)); next
  }
  # Should be FALSE (contradiction)
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [contr-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Contradicting units: %d tests, %d failures\n", n_tests, n_failures))

# === SSE creates duplicate unit ===
cat("\n=== SSE creates duplicate unit ===\n")
set.seed(135003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Unit for A, non-unit clause with A|B where B gets eliminated by SSE, creating another unit for A
  a_unit = sample(dom, sample(1:3, 1))

  # Clause: A %among% a2 | B %among% b1
  a2 = sample(dom, sample(1:3, 1))
  b1 = sample(dom, sample(1:3, 1))

  # Another clause that conflicts on B: B %among% b2 where b1 ∩ b2 might be empty
  b2 = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a_unit),
    as.CnfClause(A %among% a2 | B %among% b1),
    as.CnfClause(B %among% b2),
    as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sse-dup-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sse-dup-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE dup unit: %d tests, %d failures\n", n_tests, n_failures))

# === Three+ units for same symbol ===
cat("\n=== Triple units same symbol ===\n")
set.seed(135004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Three units for A
  a1 = sample(dom, sample(2:4, 1))
  a2 = sample(dom, sample(2:4, 1))
  a3 = sample(dom, sample(2:4, 1))
  isct = Reduce(intersect, list(a1, a2, a3))
  if (length(isct) == 0) next  # contradiction

  clauses = list(
    as.CnfClause(A %among% a1),
    as.CnfClause(A %among% a2),
    as.CnfClause(A %among% a3),
    as.CnfClause(A %among% sample(dom, sample(1:4, 1)) | B %among% sample(dom, sample(1:4, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [triple-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [triple-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Triple units: %d tests, %d failures\n", n_tests, n_failures))

# === Units-only formulas ===
cat("\n=== All units ===\n")
set.seed(135005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom_size = sample(3:6, 1)
  dom = paste0("v", 1:dom_size)
  n_vars = sample(2:4, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # All clauses are units (may have duplicates)
  n_units = sample(n_vars:(n_vars + 3), 1)
  clauses = lapply(1:n_units, function(j) {
    vname = sample(names(syms), 1)
    as.CnfClause(syms[[vname]] %among% sample(dom, sample(1:(dom_size - 1), 1)))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [allunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [allunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  All units: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
