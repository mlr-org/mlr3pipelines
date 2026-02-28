#!/usr/bin/env Rscript
# Test meta_idx_outer boundary conditions:
# - Clauses that become units during pairwise but haven't been processed yet
# - SSE cascading that reaches clauses beyond meta_idx_outer
# - The early return at line 169 and 254 that skips matrix work
# Also: test with orderings that maximize the chance of early returns
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

# === Pattern 1: Units + many non-units in specific orderings ===
cat("=== Unit + non-units ordering ===\n")
set.seed(184001)

for (trial in 1:2000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create clauses where unit propagation during pairwise creates new units
  # from clauses not yet processed
  clauses = list()

  # Units that restrict heavily
  for (j in 1:sample(1:2, 1)) {
    s = sample(names(syms), 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, 1))
  }

  # 2-symbol clauses where one symbol is heavily restricted by the unit
  for (j in 1:sample(4:8, 1)) {
    chosen = sample(names(syms), 2)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [meta1-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [meta1-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Unit + non-units: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: SSE creates unit in late clause (high meta_idx) ===
cat("\n=== SSE unit in late clause ===\n")
set.seed(184002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # First create short clauses (2 symbols, processed first)
  # Then longer clauses (3-4 symbols, processed later)
  # SSE on short clauses can modify longer clauses
  clauses = list()
  for (j in 1:sample(3:5, 1)) {
    chosen = sample(names(syms), 2)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  for (j in 1:sample(3:5, 1)) {
    n_sym = sample(3:4, 1)
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
    cat(sprintf("ERROR [meta2-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [meta2-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE unit in late: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Dense clauses with many variables ===
cat("\n=== Dense many-var ===\n")
set.seed(184003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(5:7, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(8:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 6) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dense-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dense-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Dense many-var: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Singleton + large domain ===
cat("\n=== Singleton + large domain ===\n")
set.seed(184004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom_s = c("x")
  dom_l = paste0("v", 1:6)
  A = CnfSymbol(u, "A", dom_s)
  B = CnfSymbol(u, "B", dom_l)
  C = CnfSymbol(u, "C", dom_l)
  syms = list(A = A, B = B, C = C)
  doms = list(A = dom_s, B = dom_l, C = dom_l)

  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:min(4, length(d)), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sing-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sing-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Singleton + large: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
