#!/usr/bin/env Rscript
# Symbol elimination cascade testing:
# When SSE removes a symbol from a clause, the clause might become a unit.
# That unit triggers further unit propagation, potentially cascading.
# Test that all cascading paths produce correct results.
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

# === Pattern 1: SSE creates unit which propagates to another clause ===
cat("=== SSE -> unit -> propagation cascade ===\n")
set.seed(244001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create clause pairs that could trigger SSE
  clauses = list()

  # Two clauses that share all symbols but one, enabling SSE
  shared_syms = sample(sym_names, 2)
  diff_sym = sample(setdiff(sym_names, shared_syms), 1)

  shared_ranges = lapply(shared_syms, function(s) sample(dom, sample(1:2, 1)))
  names(shared_ranges) = shared_syms

  # Clause 1: shared + diff_sym with range A
  atoms1 = c(
    lapply(shared_syms, function(s) syms[[s]] %among% shared_ranges[[s]]),
    list(syms[[diff_sym]] %among% sample(dom, sample(1:2, 1)))
  )
  clauses[[1]] = as.CnfClause(Reduce(`|`, atoms1))

  # Clause 2: shared only (subset of clause 1 on shared symbols)
  atoms2 = lapply(shared_syms, function(s) {
    r = shared_ranges[[s]]
    syms[[s]] %among% sample(r, sample(1:length(r), 1))
  })
  clauses[[2]] = as.CnfClause(Reduce(`|`, atoms2))

  # Clause 3: uses diff_sym (could be affected by unit propagation if clause 1 becomes unit)
  atoms3 = list(syms[[diff_sym]] %among% sample(dom, sample(1:3, 1)))
  if (runif(1) > 0.5) {
    extra = sample(setdiff(sym_names, diff_sym), 1)
    atoms3 = c(atoms3, list(syms[[extra]] %among% sample(dom, sample(1:2, 1))))
  }
  clauses[[3]] = as.CnfClause(Reduce(`|`, atoms3))

  # More random clauses
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

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
cat(sprintf("  SSE cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Deep cascade - unit creates unit creates unit ===
cat("\n=== Deep unit cascade ===\n")
set.seed(244002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:6)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  clauses = list()
  # Chain: V1 unit -> V2 gets restricted -> V2 clause becomes unit -> V3 restricted -> ...
  # V1 unit
  v1_range = sample(dom, sample(1:2, 1))
  clauses[[1]] = as.CnfClause(syms[["V1"]] %among% v1_range)

  # V1+V2 clause (V2 might become unit after V1 propagation)
  v2_range = sample(dom, sample(1:2, 1))
  clauses[[2]] = as.CnfClause(syms[["V1"]] %among% sample(dom, sample(1:2, 1)) | syms[["V2"]] %among% v2_range)

  # V2+V3 clause
  v3_range = sample(dom, sample(1:2, 1))
  clauses[[3]] = as.CnfClause(syms[["V2"]] %among% sample(dom, sample(1:2, 1)) | syms[["V3"]] %among% v3_range)

  # V3+V4 clause
  v4_range = sample(dom, sample(1:2, 1))
  clauses[[4]] = as.CnfClause(syms[["V3"]] %among% sample(dom, sample(1:2, 1)) | syms[["V4"]] %among% v4_range)

  # More random clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [deep-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [deep-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Deep cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Symbol elimination that empties a clause (contradiction) ===
cat("\n=== Contradiction via elimination ===\n")
set.seed(244003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create clauses where symbol elimination could produce empty clause
  clauses = list()

  # Units that restrict ranges
  for (k in 1:sample(2:3, 1)) {
    s = sample(sym_names, 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, 1))
  }

  # Multi-symbol clauses
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))  # small ranges
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [contr-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [contr-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Contradiction via elimination: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Multiple symbols eliminated from same clause ===
cat("\n=== Multiple symbol elimination ===\n")
set.seed(244004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  clauses = list()

  # Wide clause with 4-5 symbols
  wide_syms = sample(sym_names, sample(4:5, 1))
  atoms = lapply(wide_syms, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
  clauses[[1]] = as.CnfClause(Reduce(`|`, atoms))

  # Units for some of those symbols (could eliminate multiple symbols from the wide clause)
  for (s in sample(wide_syms, sample(2:3, 1))) {
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(2:3, 1)))
  }

  # More clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multielim-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multielim-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multi symbol elimination: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
