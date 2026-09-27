#!/usr/bin/env Rscript
# Test cascading through on_update_range:
# - apply_domain_restriction -> on_update_range -> try_sse_2nd_order
# - Domain restrictions that cascade through multiple clauses
# - SSE that creates units which then restrict other clauses
# - 2nd-order SSE triggered by range changes during simplification
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

# === Constructed 2nd-order SSE cascade ===
cat("=== 2nd-order SSE cascade ===\n")
set.seed(122001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Construct clauses that can trigger 2nd-order SSE:
  # For 2nd-order SSE to trigger, we need:
  # - oneend: clause where only 1 symbol is not subset of target
  # - twoend: clause where 2 symbols are not subset of target
  # - they share a symbol (symbol_intersect) and their ranges for that symbol are disjoint outside target

  a1 = sample(dom, sample(1:3, 1))
  a2 = sample(dom, sample(1:3, 1))
  b1 = sample(dom, sample(1:3, 1))
  b2 = sample(dom, sample(1:3, 1))
  c1 = sample(dom, sample(1:3, 1))
  c2 = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a2 | B %among% b2),
    as.CnfClause(A %among% c1 | C %among% c2),
    as.CnfClause(B %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1)))
  )
  # add extra clauses sometimes
  if (runif(1) > 0.5) {
    clauses[[length(clauses) + 1]] = as.CnfClause(
      A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1))
    )
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [2nd-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [2nd-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2nd-order SSE cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Domain restriction cascades ===
cat("\n=== Domain restriction cascades ===\n")
set.seed(122002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2", "3", "4")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Unit that restricts other clauses, creating cascading effects
  unit_sym = sample(c("A", "B", "C"), 1)
  unit_vals = sample(dom, sample(1:4, 1))
  syms = list(A = A, B = B, C = C)

  clauses = list(as.CnfClause(syms[[unit_sym]] %among% unit_vals))

  # Add clauses that share symbols with the unit
  for (j in 1:sample(3:7, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

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
cat(sprintf("  Domain cascades: %d tests, %d failures\n", n_tests, n_failures))

# === SSE creating units that restrict other clauses ===
cat("\n=== SSE -> unit -> restriction ===\n")
set.seed(122003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  syms = list(A = A, B = B, C = C, D = D)

  # Clauses designed so SSE turns something into a unit
  # e.g. A in {a,b} | B in {c} and A in {a,b} | B in {a,b}
  # SSE: B in {c} intersect B in {a,b} = empty -> contradiction? No.
  # Actually: SSE requires one symbol to be subset. So A in {a,b} is subset of A in {a,b},
  # and B gets intersected to {c} & {a,b} = {} -> B eliminated -> unit A in {a,b}

  n_clauses = sample(4:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

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
cat(sprintf("  SSE->unit: %d tests, %d failures\n", n_tests, n_failures))

# === Multiple units with overlapping restrictions ===
cat("\n=== Multiple units overlapping ===\n")
set.seed(122004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("x", "y", "z", "w")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  syms = list(A = A, B = B, C = C)

  # Multiple units for different symbols
  clauses = list()
  n_units = sample(1:3, 1)
  used_syms = sample(names(syms), n_units)
  for (s in used_syms) {
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
  }

  # Non-unit clauses
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

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
cat(sprintf("  Multi units: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
