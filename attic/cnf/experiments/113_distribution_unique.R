#!/usr/bin/env Rscript
# Test |.CnfFormula distribution with overlapping ranges:
# - When e1_clause and e2_clause share symbols with overlapping ranges
# - The unique(c(...)) call in distribution
# - Tautology detection during distribution
# - Distribution result needs simplification
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Distribution with same symbol in both formulas ===
cat("=== Same symbol distribution ===\n")
set.seed(113001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # f1 and f2 both constrain A, but with different ranges
  a1 = sample(dom, sample(1:3, 1))
  a2 = sample(dom, sample(1:3, 1))
  b1 = sample(dom, sample(1:3, 1))
  b2 = sample(dom, sample(1:3, 1))

  f1 = tryCatch(CnfFormula(list(as.CnfClause(A %among% a1 | B %among% b1))), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(list(as.CnfClause(A %among% a2 | B %among% b2))), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [same-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 | t2
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [same-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Same symbol: %d tests, %d failures\n", n_tests, n_failures))

# === Distribution creates tautological clauses ===
cat("\n=== Tautology during distribution ===\n")
set.seed(113002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # f1: A %among% c("0","1") -> single clause
  # f2: A %among% c("1","2") -> single clause
  # f1 | f2: distribute A %among% c("0","1") into A %among% c("1","2")
  # Result clause: A %among% unique(c("0","1","1","2")) = c("0","1","2") = dom -> TAUTOLOGY
  a_vals1 = sample(dom, sample(1:2, 1))
  a_vals2 = sample(dom, sample(1:2, 1))
  b_vals1 = sample(dom, sample(1:2, 1))
  b_vals2 = sample(dom, sample(1:2, 1))

  f1 = tryCatch(CnfFormula(list(as.CnfClause(A %among% a_vals1 | B %among% b_vals1))), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(list(as.CnfClause(A %among% a_vals2 | B %among% b_vals2))), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [taut-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 | t2
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [taut-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Tautology during distribution: %d tests, %d failures\n", n_tests, n_failures))

# === Multi-clause x multi-clause distribution with shared symbols ===
cat("\n=== Multi x multi distribution ===\n")
set.seed(113003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  make_formula = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(c("A", "B", "C"), sample(1:2, 1))
      atoms = lapply(chosen, function(s) {
        sym = switch(s, A = A, B = B, C = C)
        sym %among% sample(dom, sample(1:2, 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula(sample(2:3, 1))
  f2 = make_formula(sample(2:3, 1))
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multi-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 | t2
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multi-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multi x multi: %d tests, %d failures\n", n_tests, n_failures))

# === Distribution followed by simplification ===
cat("\n=== Distribution + simplification ===\n")
set.seed(113004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2", "3")
  n_vars = sample(3:4, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  make_formula = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:min(2, n_vars), 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula(sample(1:3, 1))
  f2 = make_formula(sample(1:3, 1))
  if (is.null(f1) || is.null(f2)) next

  # (f1 | f2) then & with f3
  f3 = make_formula(sample(1:2, 1))
  if (is.null(f3)) next

  n_tests = n_tests + 1
  result = tryCatch((f1 | f2) & f3, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [distsimp-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)
  expected = (t1 | t2) & t3
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [distsimp-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Distribution + simplification: %d tests, %d failures\n", n_tests, n_failures))

# === Near-identical ranges across clauses ===
cat("\n=== Near-identical ranges ===\n")
set.seed(113005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Multiple clauses with the same base range but one element changed
  base_a = sample(dom, 3)
  base_b = sample(dom, 3)

  clauses = list()
  for (j in 1:sample(3:6, 1)) {
    a_vals = base_a
    b_vals = base_b
    # Randomly modify one element
    if (runif(1) > 0.5) {
      swap = sample(dom, 1)
      if (length(a_vals) > 1) a_vals[sample(length(a_vals), 1)] = swap
    } else {
      swap = sample(dom, 1)
      if (length(b_vals) > 1) b_vals[sample(length(b_vals), 1)] = swap
    }
    a_vals = unique(a_vals)
    b_vals = unique(b_vals)
    clauses[[j]] = as.CnfClause(A %among% a_vals | B %among% b_vals)
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [near-%d]: %s\n", trial, result$message)); next
  }

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

  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [near-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Near-identical ranges: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
