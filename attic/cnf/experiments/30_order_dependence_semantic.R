#!/usr/bin/env Rscript
# Comprehensive test for semantic correctness despite order dependence
# For every formula, try ALL permutations of clauses (for small n_clauses)
# and verify they all produce semantically equivalent results
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

set.seed(42424)

# Helper: generate all permutations of 1:n
permutations = function(n) {
  if (n == 1) return(matrix(1, 1, 1))
  prev = permutations(n - 1)
  result = matrix(0, nrow = factorial(n), ncol = n)
  idx = 1
  for (i in seq_len(nrow(prev))) {
    for (j in 1:n) {
      perm = integer(n)
      perm[j] = n
      perm[-j] = prev[i, ]
      result[idx, ] = perm
      idx = idx + 1
    }
  }
  result
}

cat("=== Exhaustive permutation test (3-4 clauses) ===\n")
for (trial in 1:500) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("x1", "x2", "x3"))
  Y = CnfSymbol(u, "Y", c("y1", "y2"))
  Z = CnfSymbol(u, "Z", c("z1", "z2", "z3"))
  syms = list(X = X, Y = Y, Z = Z)

  n_clauses = sample(3:4, 1)
  clause_list = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  # Compute expected truth table from raw clauses
  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in clause_list) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }

  # Try all permutations
  perms = permutations(n_clauses)
  all_correct = TRUE
  for (p in seq_len(nrow(perms))) {
    perm = perms[p, ]
    f = tryCatch(CnfFormula(clause_list[perm]), error = function(e) e)
    if (inherits(f, "error")) {
      n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
      cat(sprintf("ERROR [trial %d, perm %d]: %s\n", trial, p, f$message))
      all_correct = FALSE
      next
    }
    simplified_truth = evaluate_formula(f, u)
    mismatches = which(raw_truth != simplified_truth)
    if (length(mismatches)) {
      n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
      cat(sprintf("SEMANTIC BUG [trial %d, perm %d]: mismatch at row %d\n", trial, p, mismatches[1]))
      idx = mismatches[1]
      cat(sprintf("  Assignment: %s\n",
        paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
      cat(sprintf("  Perm order: %s\n", paste(perm, collapse = ",")))
      cat("  Clauses:\n")
      for (i in seq_along(clause_list)) {
        cl = unclass(clause_list[[i]])
        cat(sprintf("    Clause %d: ", i))
        for (s in names(cl)) cat(sprintf("%s in {%s} ", s, paste(cl[[s]], collapse=",")))
        cat("\n")
      }
      all_correct = FALSE
    }
  }
  if (all_correct) {
    n_tests <<- n_tests + nrow(perms)
  }

  if (trial %% 100 == 0) cat(sprintf("  trial %d: %d tests, %d failures\n", trial, n_tests, n_failures))
}
cat(sprintf("  Exhaustive permutations: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== 5 clause random permutations ===\n")
for (trial in 1:500) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("x1", "x2"))
  Y = CnfSymbol(u, "Y", c("y1", "y2"))
  Z = CnfSymbol(u, "Z", c("z1", "z2"))
  syms = list(X = X, Y = Y, Z = Z)

  n_clauses = 5
  clause_list = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in clause_list) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }

  # Try 20 random permutations
  for (p in 1:20) {
    perm = sample(n_clauses)
    f = tryCatch(CnfFormula(clause_list[perm]), error = function(e) e)
    n_tests <<- n_tests + 1
    if (inherits(f, "error")) {
      n_failures <<- n_failures + 1
      cat(sprintf("ERROR [trial %d, perm %d]: %s\n", trial, p, f$message)); next
    }
    simplified_truth = evaluate_formula(f, u)
    mismatches = which(raw_truth != simplified_truth)
    if (length(mismatches)) {
      n_failures <<- n_failures + 1
      cat(sprintf("SEMANTIC BUG [trial %d, perm %d]: mismatch at row %d\n", trial, p, mismatches[1]))
    }
  }

  if (trial %% 100 == 0) cat(sprintf("  trial %d: %d tests, %d failures\n", trial, n_tests, n_failures))
}
cat(sprintf("  5 clause permutations: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
