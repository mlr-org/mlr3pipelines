#!/usr/bin/env Rscript
# Test apply_domain_restriction edge cases:
# - Restriction that empties a range (creates contradiction or eliminates symbol)
# - Restriction that creates a unit (triggers cascading unit propagation)
# - Multiple restrictions on same clause
# - Restriction during pairwise phase (meta_idx_outer interaction)
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

# === Restriction that empties a range ===
cat("=== Restriction -> empty range ===\n")
set.seed(163001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Unit A=a restricts clause (A in {b,c} | B in {b})
  # -> A range becomes empty -> clause becomes (B in {b})
  a_unit = sample(dom, 1)
  a_cl = setdiff(dom, a_unit)  # disjoint from unit
  b_cl = sample(dom, sample(1:2, 1))
  c_cl = sample(dom, sample(1:2, 1))

  clauses = list(
    as.CnfClause(A %among% a_unit),  # unit
    as.CnfClause(A %among% a_cl | B %among% b_cl),
    as.CnfClause(B %among% sample(dom, sample(1:2, 1)) | C %among% c_cl)
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [empty-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [empty-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Empty range: %d tests, %d failures\n", n_tests, n_failures))

# === Restriction that creates a unit (cascading) ===
cat("\n=== Restriction -> unit cascade ===\n")
set.seed(163002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Unit A=a, Clause (A in {a,b} | B in {b}) -> SSE removes A's b -> becomes unit B={b}
  # Then B={b} propagates to other clauses
  a_val = sample(dom, 1)
  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(A %among% c(a_val, sample(setdiff(dom, a_val), 1)) | B %among% sample(dom, 1)),
    as.CnfClause(B %among% sample(dom, sample(1:2, 1)) | C %among% sample(dom, sample(1:2, 1))),
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | C %among% sample(dom, sample(1:2, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cascade-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cascade-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Unit cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Multiple restrictions on same clause ===
cat("\n=== Multiple restrictions ===\n")
set.seed(163003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create units for multiple symbols, which will restrict the same clause
  n_units = sample(1:3, 1)
  unit_syms = sample(names(syms), n_units)
  clauses = lapply(unit_syms, function(s) {
    as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
  })

  # Add multi-symbol clauses that get restricted by all units
  n_nonunits = sample(2:4, 1)
  for (j in 1:n_nonunits) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multi-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multi-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multiple restrictions: %d tests, %d failures\n", n_tests, n_failures))

# === Contradiction via restriction ===
cat("\n=== Contradiction via restriction ===\n")
set.seed(163004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Unit A={a}, clause (A in {b,c}) -> restriction empties A -> clause becomes FALSE
  a_val = sample(dom, 1)
  a_other = setdiff(dom, a_val)
  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(A %among% a_other)
  )

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
cat(sprintf("  Contradiction: %d tests, %d failures\n", n_tests, n_failures))

# === Large random with many unit interactions ===
cat("\n=== Large random unit interactions ===\n")
set.seed(163005)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  syms = list()
  for (i in 1:4) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Mix units and non-units
  n_units = sample(1:3, 1)
  n_nonunits = sample(3:8, 1)
  clauses = list()
  for (j in 1:n_units) {
    s = sample(names(syms), 1)
    clauses[[j]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
  }
  for (j in 1:n_nonunits) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[n_units + j]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

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
cat(sprintf("  Large random: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
