#!/usr/bin/env Rscript
# Test scenarios where two units for the same symbol are registered:
# - SSE creates a new unit for a symbol that already has a unit
# - Unit propagation chain creates cascading units
# - The register_unit intersection + use_inso optimization path
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

# === Constructed: two units same symbol ===
cat("=== Constructed dual units ===\n")
set.seed(165001)

u = CnfUniverse()
dom = c("a", "b", "c", "d")
A = CnfSymbol(u, "A", dom)
B = CnfSymbol(u, "B", dom)
C = CnfSymbol(u, "C", dom)

# Two units for A that overlap
for (trial in 1:500) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  a_unit1 = sample(dom, sample(1:3, 1))
  a_unit2 = sample(dom, sample(1:3, 1))
  # Make sure they overlap
  if (length(intersect(a_unit1, a_unit2)) == 0) {
    a_unit2 = c(sample(a_unit1, 1), sample(setdiff(dom, a_unit1), min(1, length(setdiff(dom, a_unit1)))))
  }

  clauses = list(
    as.CnfClause(A %among% a_unit1),
    as.CnfClause(A %among% a_unit2),
    as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1))),
    as.CnfClause(B %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dual-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dual-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Dual units: %d tests, %d failures\n", n_tests, n_failures))

# === Contradicting units ===
cat("\n=== Contradicting units ===\n")
set.seed(165002)

for (trial in 1:500) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Two units with disjoint ranges -> contradiction
  a1 = sample(dom, sample(1:2, 1))
  a2 = sample(setdiff(dom, a1), min(sample(1:2, 1), length(setdiff(dom, a1))))
  if (length(a2) == 0) next

  clauses = list(
    as.CnfClause(A %among% a1),
    as.CnfClause(A %among% a2),
    as.CnfClause(B %among% sample(dom, sample(1:3, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [contra-%d]: %s\n", trial, result$message)); next
  }
  if (!isFALSE(unclass(result))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [contra-%d]: should be FALSE\n", trial))
  }
}
cat(sprintf("  Contradicting units: %d tests, %d failures\n", n_tests, n_failures))

# === SSE creates unit that duplicates existing unit ===
cat("\n=== SSE creates duplicate unit ===\n")
set.seed(165003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom3 = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom3)
  B = CnfSymbol(u, "B", dom3)
  C = CnfSymbol(u, "C", dom3)
  syms = list(A = A, B = B, C = C)

  # Start with one unit and some 2-symbol clauses
  # SSE between clauses can create new units
  unit_sym = sample(names(syms), 1)
  unit_range = sample(dom3, sample(1:2, 1))

  n_cl = sample(4:8, 1)
  clauses = list(as.CnfClause(syms[[unit_sym]] %among% unit_range))
  for (j in 1:n_cl) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom3, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

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
cat(sprintf("  SSE duplicate unit: %d tests, %d failures\n", n_tests, n_failures))

# === Multiple units for different symbols ===
cat("\n=== Multiple units different symbols ===\n")
set.seed(165004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom4 = c("a", "b", "c", "d")
  syms = list()
  for (i in 1:4) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom4)
  }

  # Create 2-3 units for different symbols + non-units
  n_units = sample(2:3, 1)
  unit_syms = sample(names(syms), n_units)
  clauses = lapply(unit_syms, function(s) {
    as.CnfClause(syms[[s]] %among% sample(dom4, sample(1:3, 1)))
  })

  n_nonunits = sample(4:8, 1)
  for (j in 1:n_nonunits) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom4, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multi-unit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multi-unit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multiple units: %d tests, %d failures\n", n_tests, n_failures))

# === Chain: unit -> SSE -> new unit -> propagation ===
cat("\n=== Unit-SSE-unit chain ===\n")
set.seed(165005)

for (trial in 1:500) {
  u = CnfUniverse()
  dom3 = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom3)
  B = CnfSymbol(u, "B", dom3)
  C = CnfSymbol(u, "C", dom3)
  D = CnfSymbol(u, "D", dom3)
  syms = list(A = A, B = B, C = C, D = D)

  # Chain: A unit -> restricts clause(A,B) -> SSE creates B unit -> restricts clause(B,C) -> ...
  a_val = sample(dom3, 1)
  b_vals = sample(dom3, 2)
  c_vals = sample(dom3, 2)

  clauses = list(
    as.CnfClause(A %among% a_val),  # unit A
    as.CnfClause(A %among% sample(dom3, 2) | B %among% sample(dom3, sample(1:2, 1))),
    as.CnfClause(B %among% b_vals | C %among% sample(dom3, sample(1:2, 1))),
    as.CnfClause(C %among% c_vals | D %among% sample(dom3, sample(1:2, 1))),
    as.CnfClause(A %among% sample(dom3, sample(1:2, 1)) | D %among% sample(dom3, sample(1:2, 1)))
  )
  # Add some extra clauses
  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom3, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Unit-SSE chain: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
