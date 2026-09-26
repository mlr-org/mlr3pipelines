source("experiments/test_harness.R")

# Exhaustive testing: 3 ternary variables (A, B, C with 3 values each = 27 assignments)
# We enumerate many clause combinations systematically

n_failures = 0
n_tests = 0

check_fc = function(formula, raw_clauses, universe, label) {
  n_tests <<- n_tests + 1
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in raw_clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }

  simplified_truth = evaluate_formula(formula, universe)
  mismatches = which(raw_truth != simplified_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d (raw=%s, simp=%s)\n",
      label, idx, raw_truth[idx], simplified_truth[idx]))
    cat(sprintf("  Assignment: %s\n",
      paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
    cat(sprintf("  Clauses:\n"))
    for (ci in seq_along(raw_clauses)) {
      cat(sprintf("    %d: %s\n", ci, deparse(unclass(raw_clauses[[ci]]))))
    }
    cat(sprintf("  Formula: "))
    print(formula)
    return(FALSE)
  }
  TRUE
}

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
B = CnfSymbol(u, "B", c("b1", "b2", "b3"))
C = CnfSymbol(u, "C", c("c1", "c2", "c3"))

# Generate all non-trivial clauses with at most 2 symbols
# Single-symbol clauses: A in {subsets of a1,a2,a3 that are not empty and not full}
single_sym_clauses = list()
for (sym_name in c("A", "B", "C")) {
  sym = switch(sym_name, A = A, B = B, C = C)
  dom = u[[sym_name]]
  for (k in 1:(length(dom) - 1)) {
    combos = combn(dom, k, simplify = FALSE)
    for (vals in combos) {
      single_sym_clauses[[length(single_sym_clauses) + 1]] = as.CnfClause(sym %among% vals)
    }
  }
}
cat(sprintf("Single-symbol clauses: %d\n", length(single_sym_clauses)))

# Two-symbol clauses: A in S_A | B in S_B (non-trivial: not full for any symbol that would make clause TRUE)
two_sym_clauses = list()
for (pair in list(c("A", "B"), c("A", "C"), c("B", "C"))) {
  sym1 = switch(pair[1], A = A, B = B, C = C)
  sym2 = switch(pair[2], A = A, B = B, C = C)
  dom1 = u[[pair[1]]]
  dom2 = u[[pair[2]]]
  for (k1 in 1:2) {  # 1-2 values for first symbol
    combos1 = combn(dom1, k1, simplify = FALSE)
    for (vals1 in combos1) {
      for (k2 in 1:2) {  # 1-2 values for second symbol
        combos2 = combn(dom2, k2, simplify = FALSE)
        for (vals2 in combos2) {
          cl = as.CnfClause(sym1 %among% vals1 | sym2 %among% vals2)
          if (!is.logical(unclass(cl))) {
            two_sym_clauses[[length(two_sym_clauses) + 1]] = cl
          }
        }
      }
    }
  }
}
cat(sprintf("Two-symbol clauses: %d\n", length(two_sym_clauses)))

all_clauses = c(single_sym_clauses, two_sym_clauses)
cat(sprintf("Total clauses: %d\n", length(all_clauses)))

# Test all 2-clause combinations
cat("\n=== All 2-clause combinations ===\n")
nc = length(all_clauses)
for (i in 1:nc) {
  for (j in i:nc) {
    cls = list(all_clauses[[i]], all_clauses[[j]])
    f = tryCatch(CnfFormula(cls), error = function(e) e)
    if (inherits(f, "error")) {
      n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
      cat(sprintf("ERROR [2-cl i=%d j=%d]: %s\n", i, j, f$message))
      next
    }
    check_fc(f, cls, u, sprintf("2-cl %d,%d", i, j))
  }
  if (i %% 10 == 0) cat(sprintf("  i=%d/%d: %d tests, %d failures\n", i, nc, n_tests, n_failures))
}
cat(sprintf("  2-clause done: %d tests, %d failures\n", n_tests, n_failures))

# Test random 3-clause combinations (too many to enumerate all)
cat("\n=== Random 3-clause combinations ===\n")
set.seed(2025)
for (trial in 1:5000) {
  indices = sort(sample(nc, 3, replace = TRUE))
  cls = list(all_clauses[[indices[1]]], all_clauses[[indices[2]]], all_clauses[[indices[3]]])
  f = tryCatch(CnfFormula(cls), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [3-cl trial %d]: %s\n", trial, f$message))
    next
  }
  check_fc(f, cls, u, sprintf("3-cl %d", trial))
}
cat(sprintf("  3-clause done: %d tests, %d failures\n", n_tests, n_failures))

# Test random 4-5 clause combinations
cat("\n=== Random 4-5 clause combinations ===\n")
for (trial in 1:3000) {
  n_cl = sample(4:5, 1)
  indices = sort(sample(nc, n_cl, replace = TRUE))
  cls = lapply(indices, function(i) all_clauses[[i]])
  f = tryCatch(CnfFormula(cls), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [4-5cl trial %d]: %s\n", trial, f$message))
    next
  }
  check_fc(f, cls, u, sprintf("4-5cl %d", trial))
}
cat(sprintf("  4-5clause done: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
