#!/usr/bin/env Rscript
# Test unusual and edge-case patterns:
# - All clauses identical
# - All clauses are units for different symbols
# - Clauses that are exact negations of each other
# - Single-symbol clauses with overlapping ranges
# - Formula where every clause is subsumed by another
# - Formula where SSE creates a chain reaction
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

# === All clauses identical ===
cat("=== All identical clauses ===\n")
set.seed(117001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  a_vals = sample(dom, sample(1:3, 1))
  b_vals = sample(dom, sample(1:3, 1))
  base_clause = as.CnfClause(A %among% a_vals | B %among% b_vals)
  if (isTRUE(unclass(base_clause))) next

  # Repeat same clause many times
  n_copies = sample(5:20, 1)
  clauses = rep(list(base_clause), n_copies)

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ident-%d]: %s\n", trial, result$message)); next
  }
  # Should simplify to single clause
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ident-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  All identical: %d tests, %d failures\n", n_tests, n_failures))

# === Units for every symbol ===
cat("\n=== All units ===\n")
set.seed(117002)

for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(3:6, 1)
  dom_size = sample(2:5, 1)
  d = paste0("v", 1:dom_size)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  # One unit for each variable
  clauses = lapply(names(syms), function(s) {
    as.CnfClause(syms[[s]] %among% sample(d, sample(1:(dom_size - 1), 1)))
  })

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [allunits-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [allunits-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  All units: %d tests, %d failures\n", n_tests, n_failures))

# === Complement clauses: A %among% "a" and A %among% c("b","c") ===
cat("\n=== Complement clauses ===\n")
set.seed(117003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Create pairs of complementary clauses
  a_vals = sample(dom, sample(1:3, 1))
  a_comp = setdiff(dom, a_vals)
  b_vals = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a_vals | B %among% b_vals),
    as.CnfClause(A %among% a_comp | B %among% sample(dom, sample(1:3, 1))),
    as.CnfClause(C %among% sample(dom, sample(1:3, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [comp-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [comp-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Complement clauses: %d tests, %d failures\n", n_tests, n_failures))

# === Single symbol all clauses (different ranges for same symbol) ===
cat("\n=== Single symbol ===\n")
set.seed(117004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)

  n_clauses = sample(3:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    as.CnfClause(A %among% sample(dom, sample(1:4, 1)))
  })

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [single-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [single-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Single symbol: %d tests, %d failures\n", n_tests, n_failures))

# === Massive subsumption: many clauses where shorter subsumes longer ===
cat("\n=== Massive subsumption ===\n")
set.seed(117005)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Short clause: A = "0"
  short = as.CnfClause(A %among% sample(dom, 1))

  # Many longer clauses that are subsumed by short
  clauses = list(short)
  for (j in 1:sample(5:15, 1)) {
    a_vals = sample(dom, sample(2:3, 1))
    if (!any(unclass(short)[["A"]] %in% a_vals)) a_vals[1] = unclass(short)[["A"]]
    b_vals = sample(dom, sample(1:2, 1))
    c_vals = sample(dom, sample(1:2, 1))
    clauses[[length(clauses) + 1]] = as.CnfClause(A %among% a_vals | B %among% b_vals | C %among% c_vals)
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [subsum-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [subsum-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Massive subsumption: %d tests, %d failures\n", n_tests, n_failures))

# === SSE chain reaction ===
cat("\n=== SSE chain reaction ===\n")
set.seed(117006)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  # SSE: clause1 restricts clause2, which becomes unit, which restricts clause3...
  clauses = list(
    as.CnfClause(A %among% sample(dom, 2) | B %among% sample(dom, 1)),
    as.CnfClause(A %among% sample(dom, 1) | B %among% sample(dom, 1)),
    as.CnfClause(B %among% sample(dom, 2) | C %among% sample(dom, 1)),
    as.CnfClause(C %among% sample(dom, 2) | D %among% sample(dom, 1)),
    as.CnfClause(A %among% sample(dom, 1) | D %among% sample(dom, 2))
  )

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
cat(sprintf("  SSE chain: %d tests, %d failures\n", n_tests, n_failures))

# === Very wide clauses (many symbols per clause) ===
cat("\n=== Wide clauses ===\n")
set.seed(117007)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1")
  n_vars = sample(6:8, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("W", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Each clause uses 4-6 symbols
  n_clauses = sample(3:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(4:min(6, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [wide-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [wide-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Wide clauses: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
