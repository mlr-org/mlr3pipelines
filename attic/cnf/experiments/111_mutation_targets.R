#!/usr/bin/env Rscript
# Mutation-testing style: each test targets a specific line/condition in simplify_cnf
# If a hypothetical bug exists at that location, THIS test should catch it.
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

# === Target: char_intersect correctness (line 30) ===
# char_intersect = function(x, y) x[x %in% y]
# Bug would be: order matters, missing elements, duplicates
cat("=== char_intersect through unit propagation ===\n")
set.seed(111001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Unit restricts A, then clause with A is restricted via char_intersect
  unit_vals = sample(dom, sample(1:4, 1))
  clause_vals = sample(dom, sample(1:4, 1))
  b_vals = sample(dom, sample(1:4, 1))

  clauses = list(
    as.CnfClause(A %among% unit_vals),
    as.CnfClause(A %among% clause_vals | B %among% b_vals)
  )

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [intersect-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [intersect-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  char_intersect: %d tests, %d failures\n", n_tests, n_failures))

# === Target: return_entries(FALSE) vs return_entries(entries[!eliminated]) ===
# Contradiction detection must return FALSE, not just empty
cat("\n=== Contradiction returns FALSE ===\n")
set.seed(111002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Various contradiction patterns
  pattern = sample(1:4, 1)
  if (pattern == 1) {
    # Direct unit contradiction
    clauses = list(
      as.CnfClause(A %among% "0"),
      as.CnfClause(A %among% "1")
    )
  } else if (pattern == 2) {
    # Cascading to contradiction
    clauses = list(
      as.CnfClause(A %among% "0"),
      as.CnfClause(A %among% c("1", "2") | B %among% "0"),
      as.CnfClause(B %among% c("1", "2"))
    )
  } else if (pattern == 3) {
    # Three-way contradiction
    clauses = list(
      as.CnfClause(A %among% c("0", "1")),
      as.CnfClause(A %among% c("1", "2")),
      as.CnfClause(A %among% c("0", "2"))
    )
  } else {
    # Unit + domain restriction empties clause
    clauses = list(
      as.CnfClause(A %among% "0"),
      as.CnfClause(B %among% "0"),
      as.CnfClause(A %among% c("1", "2") | B %among% c("1", "2"))
    )
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [contr-%d]: %s\n", trial, result$message)); next
  }
  if (!isFALSE(as.logical(result))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [contr-%d]: should be FALSE (pattern %d)\n", trial, pattern))
  }
}
cat(sprintf("  Contradiction: %d tests, %d failures\n", n_tests, n_failures))

# === Target: eliminate_clause_update_sr removing from symbol_registry ===
# If symbol_registry isn't updated, further operations may reference eliminated clauses
cat("\n=== Symbol registry consistency ===\n")
set.seed(111003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2", "3")
  n_vars = sample(3:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create formulas with many subsumption opportunities
  # (shorter clause subsumes longer one)
  n_clauses = sample(5:10, 1)
  clauses = list()
  for (j in 1:n_clauses) {
    n_sym = sample(1:min(3, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:3, 1))
    })
    clauses[[j]] = as.CnfClause(Reduce(`|`, atoms))
  }

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sr-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sr-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Symbol registry: %d tests, %d failures\n", n_tests, n_failures))

# === Target: SSE direction (intersect correct symbol in correct clause) ===
# SSE should restrict symbol in meta_idx_other, not meta_idx
cat("\n=== SSE direction ===\n")
set.seed(111004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Constructed SSE: clause1 is subset of clause2 on all except one symbol
  a_subset = sample(dom, sample(1:3, 1))
  b_common = sample(dom, sample(1:3, 1))
  c1_vals = sample(dom, sample(1:3, 1))
  c2_vals = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a_subset | B %among% b_common | C %among% c1_vals),
    as.CnfClause(A %among% c(a_subset, setdiff(dom, a_subset)[1:min(1, length(setdiff(dom, a_subset)))]) | B %among% b_common | C %among% c2_vals)
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ssedir-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ssedir-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE direction: %d tests, %d failures\n", n_tests, n_failures))

# === Target: HLA complement calculation ===
# range_new = c(range_old, char_setdiff(universe[[symbol]], c(range_old, entries[[clause_idx_other]][[symbol]])))
cat("\n=== HLA complement ===\n")
set.seed(111005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2", "3", "4")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Create formula where HLA complement calculation matters
  # Multiple clauses sharing symbols with different ranges
  n_clauses = sample(4:7, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(c("A", "B", "C"), n_sym)
    atoms = lapply(chosen, function(s) {
      sym = switch(s, A = A, B = B, C = C)
      sym %among% sample(dom, sample(1:4, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [hla-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hla-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA complement: %d tests, %d failures\n", n_tests, n_failures))

# === Target: not_subset_count update correctness ===
# Verify that pairwise + SSE + cascading all maintain correct counts
cat("\n=== not_subset_count consistency ===\n")
set.seed(111006)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(3:4, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Many clauses with overlapping symbols -> lots of pairwise interactions
  n_clauses = sample(6:12, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(2:n_vars, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [nsc-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [nsc-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  not_subset_count: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
