#!/usr/bin/env Rscript
# Formulas where simplification creates many intermediate states:
# - Clauses that get shortened progressively
# - Units that appear and disappear
# - The same clause gets modified multiple times
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

# === Pattern 1: Progressive clause shortening ===
cat("=== Progressive shortening ===\n")
set.seed(207001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Unit for A that will propagate and shorten clauses
  a_val = sample(dom, 1)
  a_comp = setdiff(dom, a_val)

  # Clause (A_comp | B | C | D) - after A-propagation becomes (B | C | D)
  # Then if B is also restricted, becomes (C | D) etc.
  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(A %among% a_comp | B %among% sample(dom, sample(1:2, 1)) |
                 C %among% sample(dom, sample(1:2, 1)) | D %among% sample(dom, sample(1:2, 1)))
  )

  # More clauses that create SSE opportunities
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
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
    cat(sprintf("ERROR [progsh-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [progsh-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Progressive shortening: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Cascading unit creation chain ===
cat("\n=== Cascading unit creation ===\n")
set.seed(207002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  E = CnfSymbol(u, "E", dom)
  syms = list(A = A, B = B, C = C, D = D, E = E)

  # Chain: unit A -> creates unit B -> creates unit C -> restricts D, E
  a_val = sample(dom, 1); a_comp = setdiff(dom, a_val)
  b_val = sample(dom, 1); b_comp = setdiff(dom, b_val)
  c_val = sample(dom, 1); c_comp = setdiff(dom, c_val)

  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(A %among% a_comp | B %among% b_val),
    as.CnfClause(B %among% b_comp | C %among% c_val),
    as.CnfClause(C %among% c_comp | D %among% sample(dom, sample(1:2, 1)))
  )

  # Add interaction clauses
  for (j in 1:sample(3:5, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), min(n_sym, 5))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cascunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cascunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Cascading unit creation: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Clause modified by multiple different mechanisms ===
cat("\n=== Multiple modification mechanisms ===\n")
set.seed(207003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Unit for A restricts ranges; SSE restricts further; subsumption might eliminate
  a_val = sample(dom, sample(1:2, 1))
  clauses = list(as.CnfClause(A %among% a_val))

  # Target clause with A, B, C
  target_a = sample(dom, sample(2:4, 1))
  target_b = sample(dom, sample(2:4, 1))
  target_c = sample(dom, sample(2:4, 1))
  clauses[[length(clauses) + 1]] = as.CnfClause(
    A %among% target_a | B %among% target_b | C %among% target_c
  )

  # SSE pair on B
  b1 = sample(dom, sample(1:3, 1))
  b2 = sample(dom, sample(1:3, 1))
  clauses[[length(clauses) + 1]] = as.CnfClause(B %among% b1 | C %among% sample(dom, sample(1:3, 1)))
  clauses[[length(clauses) + 1]] = as.CnfClause(B %among% b2 | C %among% sample(dom, sample(1:3, 1)))

  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multmod-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multmod-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multiple mechanisms: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Operations on formulas that had complex simplification ===
cat("\n=== Operations on complex-simplified ===\n")
set.seed(207004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_complex = function() {
    n_cl = sample(3:6, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(2:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 2) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_complex(); f2 = make_complex()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  # Chain of operations
  result = tryCatch({
    r = f1 & f2
    r = r | !f1
    !r
  }, error = function(e) e)

  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [compops-%d]: %s\n", trial, result$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  expected = !((t_f1 & t_f2) | !t_f1)

  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [compops-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Ops on complex: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
