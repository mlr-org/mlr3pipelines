#!/usr/bin/env Rscript
# Exhaustive test: 3 variables with domain size 4
# This creates a large clause pool and tests all 2-clause combos + many 3-clause combos
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Exhaustive 3-var 4-domain ===\n")
u = CnfUniverse()
dom = c("a", "b", "c", "d")
A = CnfSymbol(u, "A", dom)
B = CnfSymbol(u, "B", dom)
C = CnfSymbol(u, "C", dom)

# Generate all possible non-tautological clauses
# For each variable: absent, or any non-empty strict subset of dom
# Number of strict subsets of dom = 2^4 - 2 = 14 (excluding empty and full)
# For 3 vars: (14+1)^3 - 1 = 3374 possible clauses (including those with some vars absent)
# That's too many for exhaustive 2-clause combos. Let's use restricted subsets.

# Restrict to subsets of size 1 or 2 for each variable
# That's 4 + 6 = 10 options per variable, plus absent = 11
# 11^3 - 1 = 1330 clauses
all_subsets_12 = c(
  as.list(dom),  # size 1: 4 subsets
  combn(dom, 2, simplify = FALSE)  # size 2: 6 subsets
)

all_clauses = list()
for (sa in c(list(NULL), all_subsets_12)) {
  for (sb in c(list(NULL), all_subsets_12)) {
    for (sc in c(list(NULL), all_subsets_12)) {
      if (is.null(sa) && is.null(sb) && is.null(sc)) next
      cl = list()
      if (!is.null(sa)) cl[["A"]] = sa
      if (!is.null(sb)) cl[["B"]] = sb
      if (!is.null(sc)) cl[["C"]] = sc
      all_clauses[[length(all_clauses) + 1]] = structure(cl, universe = u, class = "CnfClause")
    }
  }
}
n_cl = length(all_clauses)
cat(sprintf("  Generated %d clauses\n", n_cl))

# Helper
check_formula = function(clauses, label) {
  n_tests <<- n_tests + 1
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR [%s]: %s\n", label, f$message))
    return(FALSE)
  }

  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
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

  simplified_truth = evaluate_formula(f, u)
  mismatches = which(raw_truth != simplified_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d\n", label, idx))
    return(FALSE)
  }
  TRUE
}

# All 2-clause combos (upper triangle + diagonal)
cat("  Testing all 2-clause combos...\n")
for (i in 1:n_cl) {
  if (i %% 200 == 0) cat(sprintf("    Progress: %d/%d\n", i, n_cl))
  for (j in i:n_cl) {
    check_formula(list(all_clauses[[i]], all_clauses[[j]]), sprintf("2cl-%d-%d", i, j))
  }
}
cat(sprintf("  2-clause combos: %d tests, %d failures\n", n_tests, n_failures))

# Random 3-clause combos
cat("  Testing random 3-clause combos...\n")
set.seed(114001)
for (trial in 1:5000) {
  indices = sample(n_cl, 3, replace = TRUE)
  check_formula(list(all_clauses[[indices[1]]], all_clauses[[indices[2]]], all_clauses[[indices[3]]]),
                sprintf("3cl-%d", trial))
}
cat(sprintf("  3-clause combos: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
