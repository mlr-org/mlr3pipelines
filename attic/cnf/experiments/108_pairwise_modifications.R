#!/usr/bin/env Rscript
# Test pairwise comparison loop edge cases:
# - Clauses eliminated during inner loop
# - Clauses becoming units during pairwise
# - is_not_subset_of matrix with clauses shortened by unit propagation
# - SSE cascading through on_updated_subset_relations during pairwise
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

# === Subsumption during pairwise ===
cat("=== Subsumption during pairwise ===\n")
set.seed(108001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  n_vars = sample(3:4, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create pairs of clauses where one subsumes the other
  # (subset ranges for all symbols)
  n_clauses = sample(4:8, 1)
  clauses = list()
  for (j in 1:n_clauses) {
    n_sym = sample(2:n_vars, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      n_vals = sample(1:2, 1)
      syms[[s]] %among% sample(dom, n_vals)
    })
    clauses[[j]] = as.CnfClause(Reduce(`|`, atoms))
  }

  # Add a clause that is a superset of another (guaranteed subsumption)
  if (length(clauses) >= 2) {
    base = unclass(clauses[[1]])
    if (!isTRUE(base) && !isFALSE(base)) {
      extra_sym = sample(setdiff(names(syms), names(base)), min(1, n_vars - length(base)))
      wider = base
      for (s in names(wider)) {
        extra = setdiff(dom, wider[[s]])
        if (length(extra) > 0) wider[[s]] = c(wider[[s]], sample(extra, min(1, length(extra))))
      }
      clauses[[length(clauses) + 1]] = structure(wider, universe = u, class = "CnfClause")
    }
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
cat(sprintf("  Subsumption during pairwise: %d tests, %d failures\n", n_tests, n_failures))

# === SSE creates unit during pairwise, triggering cascading ===
cat("\n=== SSE -> unit cascade in pairwise ===\n")
set.seed(108002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  # SSE pattern: two clauses that share all but one symbol
  # After SSE, the modified clause might become a unit
  a1 = sample(dom, 2)
  a2 = setdiff(dom, a1)
  b_val = sample(dom, 1)

  clauses = list(
    as.CnfClause(A %among% c(a1[1], a2) | B %among% b_val),
    as.CnfClause(A %among% a1 | B %among% b_val),
    # Additional clauses that will be affected by the unit
    as.CnfClause(B %among% sample(dom, sample(1:2, 1)) | C %among% sample(dom, sample(1:2, 1))),
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | C %among% sample(dom, sample(1:2, 1)) | D %among% sample(dom, sample(1:2, 1)))
  )

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
cat(sprintf("  SSE -> unit cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Clauses shortened by unit propagation during pairwise setup ===
cat("\n=== Shortened clauses in pairwise ===\n")
set.seed(108003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2", "3")
  n_vars = sample(4:6, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Mix of units and long clauses
  clauses = list()
  # 1 unit
  unit_sym = sample(names(syms), 1)
  clauses[[1]] = as.CnfClause(syms[[unit_sym]] %among% sample(dom, sample(1:2, 1)))

  # Long clauses (3-4 symbols) that reference the unit's symbol
  for (j in 1:sample(4:8, 1)) {
    n_sym = sample(3:min(4, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    # Ensure at least some clauses reference the unit symbol
    if (j <= 2 && !unit_sym %in% chosen) {
      chosen[1] = unit_sym
    }
    atoms = lapply(chosen, function(s) {
      n_vals = sample(1:3, 1)
      syms[[s]] %among% sample(dom, n_vals)
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [short-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [short-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Shortened clauses: %d tests, %d failures\n", n_tests, n_failures))

# === Dense pairwise interactions ===
cat("\n=== Dense pairwise ===\n")
set.seed(108004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  n_vars = 3
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("W", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Many clauses all using all 3 symbols -> very dense pairwise interactions
  n_clauses = sample(8:15, 1)
  clauses = lapply(1:n_clauses, function(j) {
    atoms = lapply(names(syms), function(s) {
      n_vals = sample(1:2, 1)
      syms[[s]] %among% sample(dom, n_vals)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

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
cat(sprintf("  Dense pairwise: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
