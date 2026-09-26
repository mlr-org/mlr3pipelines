#!/usr/bin/env Rscript
# Test cascading unit creation during initial non-unit processing phase:
# When processing non-units against existing units, apply_domain_restriction can
# create new units. These new units then propagate to already-registered clauses.
# This tests the reentry paths in register_unit.
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

# === Constructed: unit propagation creates new unit during init ===
cat("=== Cascade: unit -> new unit during init ===\n")
set.seed(168001)

# Scenario: unit for A, clause (A in {disjoint} | B in {val}) creates B unit,
# which then propagates to already-registered clauses with B
for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  a_val = sample(dom, 1)
  a_disj = setdiff(dom, a_val)
  b_val = sample(dom, sample(1:3, 1))
  c_val = sample(dom, sample(1:2, 1))

  # Clause order matters: clauses sorted by length, so units come first
  clauses = list(
    as.CnfClause(A %among% a_val),              # unit A
    as.CnfClause(B %among% b_val | C %among% c_val),  # registered first (has B and C)
    as.CnfClause(A %among% a_disj | B %among% sample(dom, 1))  # A range disjoint from unit -> A eliminated -> becomes B unit
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cascade-init-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cascade-init-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Cascade init: %d tests, %d failures\n", n_tests, n_failures))

# === Double cascade: unit -> new unit -> another new unit ===
cat("\n=== Double cascade: A unit -> B unit -> C unit ===\n")
set.seed(168002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  a_val = sample(dom, 1)

  # Design: A unit -> eliminates A from clause2 -> creates B unit
  #         B unit -> eliminates B from clause3 -> creates C unit
  #         C unit -> restricts clause4
  clauses = list(
    as.CnfClause(A %among% a_val),                                          # unit A
    as.CnfClause(A %among% setdiff(dom, a_val) | B %among% sample(dom, 1)),  # becomes B unit
    as.CnfClause(B %among% setdiff(dom, dom[1]) | C %among% sample(dom, 1)), # may become C unit if B disjoint
    as.CnfClause(C %among% sample(dom, sample(1:2, 1)) | D %among% sample(dom, sample(1:2, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dbl-cascade-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dbl-cascade-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Double cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Cascade creates duplicate unit (intersection path) ===
cat("\n=== Cascade creates duplicate unit ===\n")
set.seed(168003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  a_val = sample(dom, 1)
  b_unit = sample(dom, sample(1:3, 1))
  b_cascade = sample(dom, sample(1:2, 1))

  # Both an explicit B unit and a cascade-created B unit
  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(B %among% b_unit),
    as.CnfClause(A %among% setdiff(dom, a_val) | B %among% b_cascade),  # cascade creates B unit
    as.CnfClause(B %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:2, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

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
cat(sprintf("  Duplicate unit cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Random with many units to stress cascade paths ===
cat("\n=== Random many-unit stress ===\n")
set.seed(168004)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(3:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create many units (1-3) and many 2-symbol clauses
  n_units = sample(1:3, 1)
  clauses = list()
  for (j in 1:n_units) {
    s = sample(names(syms), 1)
    clauses[[j]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
  }
  n_multi = sample(4:10, 1)
  for (j in 1:n_multi) {
    n_sym = 2  # exactly 2 symbols to maximize cascade potential
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[n_units + j]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [many-unit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [many-unit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Many-unit stress: %d tests, %d failures\n", n_tests, n_failures))

# === Cascade creating contradictions ===
cat("\n=== Cascade contradictions ===\n")
set.seed(168005)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  a_val = sample(dom, 1)
  b_val = sample(dom, 1)

  # A unit -> cascade creates B unit (b_val) -> B unit contradicts clause with disjoint B range
  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(A %among% setdiff(dom, a_val) | B %among% b_val),  # -> B unit
    as.CnfClause(B %among% setdiff(dom, b_val))  # contradicts B unit!
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cascade-contra-%d]: %s\n", trial, result$message)); next
  }
  if (!isFALSE(unclass(result))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cascade-contra-%d]: should be FALSE\n", trial))
  }
}
cat(sprintf("  Cascade contradictions: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
