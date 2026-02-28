#!/usr/bin/env Rscript
# Cascading elimination stress:
# - SSE that creates units, which propagate and create more units
# - eliminate_symbol_from_clause leading to cascading effects
# - Deep chains where one elimination triggers another
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

# === Pattern 1: Chain unit propagation ===
cat("=== Chain unit propagation ===\n")
set.seed(196001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Create a chain: unit for A -> restricts clause (A|B) to unit for B -> restricts clause (B|C) to unit for C...
  a_val = sample(dom, 1)
  b_val = sample(dom, 1)
  c_val = sample(dom, 1)

  # Unit: A in {a_val}
  # Clause: A in {complement of a_val} | B in {b_val} -> after unit prop, B = {b_val}
  # Clause: B in {complement of b_val} | C in {c_val} -> after unit prop, C = {c_val}
  a_comp = setdiff(dom, a_val)
  b_comp = setdiff(dom, b_val)

  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(A %among% a_comp | B %among% b_val),
    as.CnfClause(B %among% b_comp | C %among% c_val)
  )
  # Add more clauses
  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

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
cat(sprintf("  Chain unit propagation: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: SSE creating unit during pairwise ===
cat("\n=== SSE unit during pairwise ===\n")
set.seed(196002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create conditions for SSE:
  # Clause 1: (A in a_range | B in b_range)
  # Clause 2: (A in a_range | B in b_range2) where b_range ⊂ b_range2
  # SSE restricts clause 2's B to intersection with clause 1's B -> could become unit
  a_range = sample(dom, sample(1:2, 1))
  b_range = sample(dom, sample(1:2, 1))
  b_extra = sample(setdiff(dom, b_range), min(2, length(setdiff(dom, b_range))))
  b_range2 = c(b_range, b_extra)

  clauses = list(
    as.CnfClause(A %among% a_range | B %among% b_range),
    as.CnfClause(A %among% a_range | B %among% b_range2)
  )
  # Adding C clauses for more complexity
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ssepw-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ssepw-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE unit during pairwise: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Deep cascading (4+ levels) ===
cat("\n=== Deep cascading elimination ===\n")
set.seed(196003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  E = CnfSymbol(u, "E", dom)
  syms = list(A = A, B = B, C = C, D = D, E = E)

  # Long chain: unit for A -> clause becomes unit for B -> clause becomes unit for C -> ...
  vals = sapply(names(syms), function(s) sample(dom, 1))
  clauses = list(
    as.CnfClause(A %among% vals["A"])  # unit for A
  )
  # Each clause: complement of previous var | next var
  prev = "A"
  for (s in c("B", "C", "D", "E")) {
    comp = setdiff(dom, vals[prev])
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[prev]] %among% comp | syms[[s]] %among% vals[s])
    prev = s
  }
  # Add random clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:5, 1)
    chosen = sample(names(syms), n_sym)
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
cat(sprintf("  Deep cascading: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Elimination during is_not_subset_of loop ===
cat("\n=== Elimination during pairwise loop ===\n")
set.seed(196004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create many clauses that are very close to each other
  # (high chance of SSE between multiple pairs during the pairwise loop)
  base_a = sample(dom, sample(1:2, 1))
  base_b = sample(dom, sample(1:2, 1))
  base_c = sample(dom, sample(1:2, 1))

  clauses = list()
  for (j in 1:sample(6:10, 1)) {
    a_range = if (sample(c(TRUE, FALSE), 1)) base_a else sample(dom, sample(1:3, 1))
    b_range = if (sample(c(TRUE, FALSE), 1)) base_b else sample(dom, sample(1:3, 1))
    c_range = if (sample(c(TRUE, FALSE), 1)) base_c else sample(dom, sample(1:3, 1))

    # Sometimes 2-sym, sometimes 3-sym
    if (sample(c(TRUE, FALSE), 1)) {
      cl = as.CnfClause(A %among% a_range | B %among% b_range | C %among% c_range)
    } else {
      chosen = sample(c("A", "B", "C"), 2)
      ranges = list(A = a_range, B = b_range, C = c_range)
      atoms = lapply(chosen, function(s) syms[[s]] %among% ranges[[s]])
      cl = as.CnfClause(Reduce(`|`, atoms))
    }
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [elimpw-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [elimpw-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Elimination during pairwise: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
