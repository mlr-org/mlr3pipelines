#!/usr/bin/env Rscript
# Test HLA (Hidden Literal Addition) phase interactions:
# - HLA after 2nd-order SSE modifications
# - HLA unit phase with remaining_nonunit_entries modified by non-unit HLA
# - HLA with exactly domain-filling extensions (hidden tautology)
# - HLA subsumption during HLA extension
# - delayedAssign(roe_inverse) correctness
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

# === Constructed HLA hidden tautology ===
cat("=== Constructed HLA hidden tautology ===\n")
set.seed(170001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Create a clause that can become tautological through HLA
  # clause: A in {a} | B in {b}
  # another: B in {a,c} | A in {a,b,c} <- B complement is {b}, adding {b} to first clause's B range: {b,a,c} = full -> tautology!
  # Wait, that's not right. Let me think...
  # clause C1: A in {a} | B in {b}
  # clause C2: A in {a,b} (subset of C1 for A only if {a,b} subset of {a} - no)
  # For HLA: C2 must be subset of C1 for all but one symbol
  # C2 has only symbol A. C1 has symbols A, B.
  # is_not_subset_of[C2][C1, A] = TRUE if C2.A ⊄ C1.A (i.e. {a,b} ⊄ {a} - TRUE)
  # C2 has no B, so B column trivially FALSE
  # not_subset_count = 1 (only A)
  # HLA: extend C1.A with complement of C2.A w/r/t universe: dom \ (C1.A ∪ C2.A) = {a,b,c} \ ({a} ∪ {a,b}) = {c}
  # C1.A becomes {a,c}. Not full domain. Not tautology.

  # Better approach: random stress
  a_range = sample(dom, sample(1:2, 1))
  b_range = sample(dom, sample(1:2, 1))
  c_range = sample(dom, sample(1:2, 1))

  clauses = list(
    as.CnfClause(A %among% a_range | B %among% b_range),
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | C %among% c_range),
    as.CnfClause(B %among% sample(dom, sample(1:2, 1)) | C %among% sample(dom, sample(1:2, 1))),
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | B %among% sample(dom, sample(1:2, 1)) | C %among% sample(dom, sample(1:2, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [hla-taut-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hla-taut-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA hidden tautology: %d tests, %d failures\n", n_tests, n_failures))

# === HLA unit phase stress ===
cat("\n=== HLA unit phase stress ===\n")
set.seed(170002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Create units + many multi-symbol clauses
  # Units in HLA phase process remaining_nonunit_entries
  n_units = sample(1:2, 1)
  unit_syms = sample(names(syms), n_units)
  clauses = lapply(unit_syms, function(s) {
    as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
  })

  n_multi = sample(4:8, 1)
  for (j in 1:n_multi) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[n_units + j]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [hla-unit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hla-unit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA unit phase: %d tests, %d failures\n", n_tests, n_failures))

# === HLA after non-unit HLA eliminates entries ===
cat("\n=== HLA unit after non-unit HLA ===\n")
set.seed(170003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Many clauses with various lengths, some units
  # Goal: non-unit HLA eliminates some clauses, then unit HLA uses modified remaining_nonunit_entries
  clauses = list()
  # 1-2 units
  for (j in 1:sample(1:2, 1)) {
    s = sample(names(syms), 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
  }
  # 4-8 multi-symbol clauses of various lengths
  for (j in 1:sample(4:8, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [hla-after-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hla-after-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA unit after non-unit: %d tests, %d failures\n", n_tests, n_failures))

# === 2nd-order SSE then HLA ===
cat("\n=== 2nd-order SSE then HLA ===\n")
set.seed(170004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(3:4, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create clauses designed for 2nd-order SSE: pairs with not_subset_count == 2
  n_cl = sample(6:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:min(3, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [2ndsse-hla-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [2ndsse-hla-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2nd-order + HLA: %d tests, %d failures\n", n_tests, n_failures))

# === Large formulas: all phases interact ===
cat("\n=== Large formula all-phases ===\n")
set.seed(170005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(4:6, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Mix: 1-2 units, 10-20 multi-symbol clauses
  clauses = list()
  for (j in 1:sample(1:2, 1)) {
    s = sample(names(syms), 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
  }
  for (j in 1:sample(10:20, 1)) {
    n_sym = sample(2:min(4, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 8) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [large-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [large-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Large all-phases: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
