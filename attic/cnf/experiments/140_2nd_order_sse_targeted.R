#!/usr/bin/env Rscript
# Targeted 2nd-order SSE patterns: construct formulas that specifically
# trigger oneend and twoend 2nd-order SSE paths.
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

# === Constructed oneend pattern ===
cat("=== Constructed oneend 2nd-order SSE ===\n")
set.seed(140001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Construct 3 clauses that form a oneend pattern:
  # Clause 1 (oneend): A in a1, B in b1  -- 1 symbol not subset of target (B)
  # Clause 2 (twoend): A in a2, B in b2, C in c2  -- 2 symbols not subset of target (A and B)
  # Target:           A in at, B in bt, C in ct
  # Condition: clause1 and clause2 are disjoint on B outside of target's B range

  a1 = sample(dom, sample(1:3, 1))
  b1 = sample(dom, sample(1:3, 1))
  a2 = sample(dom, sample(1:3, 1))
  b2 = sample(dom, sample(1:3, 1))
  c2 = sample(dom, sample(1:3, 1))
  at = sample(dom, sample(2:3, 1))
  bt = sample(dom, sample(2:3, 1))
  ct = sample(dom, sample(2:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a2 | B %among% b2 | C %among% c2),
    as.CnfClause(A %among% at | B %among% bt | C %among% ct)
  )
  # Add additional clauses for variety
  clauses[[4]] = as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1)))

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [oneend-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [oneend-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Oneend 2nd-order: %d tests, %d failures\n", n_tests, n_failures))

# === Constructed twoend pattern ===
cat("\n=== Constructed twoend 2nd-order SSE ===\n")
set.seed(140002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  # Twoend pattern:
  # Clause (twoend): has 2 symbols not subset of target
  # Find a oneend clause that shares one of those symbols

  a1 = sample(dom, sample(1:3, 1))
  b1 = sample(dom, sample(1:3, 1))
  c1 = sample(dom, sample(1:3, 1))
  d1 = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1 | C %among% c1),
    as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1))),
    as.CnfClause(B %among% sample(dom, sample(1:3, 1)) | C %among% sample(dom, sample(1:3, 1)) | D %among% d1),
    as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | D %among% sample(dom, sample(1:3, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [twoend-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [twoend-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Twoend 2nd-order: %d tests, %d failures\n", n_tests, n_failures))

# === 2nd-order SSE cascade: one SSE triggers another ===
cat("\n=== 2nd-order SSE cascade ===\n")
set.seed(140003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  # Multiple clauses that could chain 2nd-order SSE
  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:4, 1)
    chosen = sample(c("A", "B", "C", "D"), n_sym)
    syms = list(A = A, B = B, C = C, D = D)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
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
cat(sprintf("  2nd-order cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Random with many 2-symbol clauses (triggers 2nd-order SSE) ===
cat("\n=== Many 2-symbol clauses ===\n")
set.seed(140004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(3:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Mostly 2-symbol clauses to maximize 2nd-order SSE opportunities
  n_cl = sample(5:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), 2)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [2sym-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [2sym-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2-symbol clauses: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
