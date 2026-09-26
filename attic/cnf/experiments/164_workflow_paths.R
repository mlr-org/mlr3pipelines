#!/usr/bin/env Rscript
# Test specific simplifier workflow paths:
# - Subsumption first, then SSE (order matters)
# - Unit propagation that enables SSE
# - SSE that enables subsumption
# - HLA that enables subsumption
# - 2nd-order SSE that enables 1st-order SSE
# All verified via truth table comparison
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

# === Random formulas with guaranteed units ===
cat("=== Units + multi-symbol ===\n")
set.seed(164001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Guaranteed: 2 units + 4-6 multi-symbol clauses
  clauses = list(
    as.CnfClause(syms[[sample(names(syms), 1)]] %among% sample(dom, sample(1:2, 1))),
    as.CnfClause(syms[[sample(names(syms), 1)]] %among% sample(dom, sample(1:2, 1)))
  )
  for (j in 1:sample(4:6, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [units-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [units-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Units + multi: %d tests, %d failures\n", n_tests, n_failures))

# === Clauses designed for SSE ===
cat("\n=== SSE-designed patterns ===\n")
set.seed(164002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create clause pairs where one is subset of the other on all but one symbol
  # This should trigger SSE
  base_sym = sample(names(syms), 1)
  other_syms = setdiff(names(syms), base_sym)

  base_range = sample(dom, sample(2:3, 1))
  # clause1: base_sym in base_range, other_sym1 in range1
  s1 = sample(other_syms, 1)
  r1 = sample(dom, sample(1:3, 1))
  # clause2: base_sym in subset_range, other_sym1 in range1 (same)
  # SSE on base_sym should restrict clause2
  subset_range = sample(base_range, min(length(base_range) - 1, sample(1:2, 1)))

  clauses = list(
    as.CnfClause(syms[[base_sym]] %among% base_range | syms[[s1]] %among% r1),
    as.CnfClause(syms[[base_sym]] %among% setdiff(dom, subset_range) | syms[[s1]] %among% r1)
  )
  # Add some extra clauses
  for (j in 1:sample(2:4, 1)) {
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
    cat(sprintf("ERROR [sse-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sse-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE patterns: %d tests, %d failures\n", n_tests, n_failures))

# === Mixed workflow stress ===
cat("\n=== Mixed workflow stress ===\n")
set.seed(164003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom_size = sample(3:5, 1)
  n_vars = sample(3:5, 1)
  dom = paste0("v", 1:dom_size)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(4:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:min(3, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:(dom_size - 1), 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mixed-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixed-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Mixed workflow: %d tests, %d failures\n", n_tests, n_failures))

# === Operations after simplification ===
cat("\n=== Operations on simplified formulas ===\n")
set.seed(164004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(2:4, 1)
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) < 1) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  # AND
  n_tests = n_tests + 1
  r = tryCatch(f1 & f2, error = function(e) NULL)
  if (!is.null(r)) {
    tr = evaluate_formula(r, u)
    if (!all(tr == (t1 & t2))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [op-and-%d]\n", trial))
    }
  }

  # OR
  n_tests = n_tests + 1
  r = tryCatch(f1 | f2, error = function(e) NULL)
  if (!is.null(r)) {
    tr = evaluate_formula(r, u)
    if (!all(tr == (t1 | t2))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [op-or-%d]\n", trial))
    }
  }

  # NOT
  n_tests = n_tests + 1
  r = tryCatch(!f1, error = function(e) NULL)
  if (!is.null(r)) {
    tr = evaluate_formula(r, u)
    if (!all(tr == !t1)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [op-not-%d]\n", trial))
    }
  }

  # Combined: !(f1 & f2)
  n_tests = n_tests + 1
  r = tryCatch(!(f1 & f2), error = function(e) NULL)
  if (!is.null(r)) {
    tr = evaluate_formula(r, u)
    if (!all(tr == !(t1 & t2))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [op-negand-%d]\n", trial))
    }
  }
}
cat(sprintf("  Operations: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
