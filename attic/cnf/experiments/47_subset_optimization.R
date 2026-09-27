#!/usr/bin/env Rscript
# Test the outer_subset_of_inner optimization at line 588 of CnfFormula_simplify.R
# The optimization: outer_subset_of_inner = (inner_subset_of_outer && length(range_outer) == length(range_inner)) || all(range_outer %in% range_inner)
# This shortcuts the all() check when we already know inner is subset of outer and lengths match (equality)
source("experiments/test_harness.R")

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
    return(FALSE)
  }
  TRUE
}

# === Clauses with equal ranges (same length, same values) ===
# This is the case where the optimization kicks in: equal ranges mean both are subsets
cat("=== Equal range patterns ===\n")
set.seed(47001)

for (trial in 1:300) {
  u = CnfUniverse()
  n_vars = sample(3:4, 1)
  dom_sizes = sample(3:5, n_vars, replace = TRUE)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  # Create clauses where some symbols have exactly the same range
  base_range = list()
  for (s in names(syms)) {
    dom = u[[s]]
    base_range[[s]] = sample(dom, sample(1:max(1, length(dom)-1), 1))
  }

  clauses = list()
  # Clauses sharing some base ranges
  for (i in 1:sample(3:6, 1)) {
    n_atoms = sample(2:min(n_vars, 3), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      # 50% chance of using the base range, 50% chance of a random range
      if (runif(1) > 0.5) {
        syms[[s]] %among% base_range[[s]]
      } else {
        dom = u[[s]]
        syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
      }
    })
    clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [eqrange trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("eqrange-%d", trial))
}
cat(sprintf("  Equal range: %d tests, %d failures\n", n_tests, n_failures))

# === Clauses with near-equal ranges (one element different) ===
cat("\n=== Near-equal range patterns ===\n")
set.seed(47002)

for (trial in 1:300) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3", "a4"))
  B = CnfSymbol(u, "B", c("b1", "b2", "b3"))
  C = CnfSymbol(u, "C", c("c1", "c2", "c3"))

  syms = list(A = A, B = B, C = C)

  # Create clauses where ranges differ by exactly one element
  clauses = list()
  for (i in 1:sample(3:7, 1)) {
    n_atoms = sample(2:3, 1)
    chosen = sample(c("A", "B", "C"), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      base = sample(dom, max(1, length(dom) - 2))
      # Add one random element to make ranges slightly different between clauses
      extra = sample(setdiff(dom, base), min(1, length(setdiff(dom, base))))
      syms[[s]] %among% c(base, extra)
    })
    clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [neareq trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("neareq-%d", trial))
}
cat(sprintf("  Near-equal range: %d tests, %d failures\n", n_tests, n_failures))

# === Identical clauses with different symbol orderings ===
cat("\n=== Different symbol orderings ===\n")
set.seed(47003)

for (trial in 1:200) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2"))
  C = CnfSymbol(u, "C", c("c1", "c2"))

  val_a = sample(c("a1", "a2", "a3"), sample(1:2, 1))
  val_b = sample(c("b1", "b2"), 1)
  val_c = sample(c("c1", "c2"), 1)

  # Create the same clause with atoms in different orders
  cl1 = as.CnfClause(A %among% val_a | B %among% val_b | C %among% val_c)
  cl2 = as.CnfClause(C %among% val_c | A %among% val_a | B %among% val_b)
  cl3 = as.CnfClause(B %among% val_b | C %among% val_c | A %among% val_a)

  # Add another clause to make the formula non-trivial
  extra = as.CnfClause(A %among% sample(c("a1", "a2", "a3"), sample(1:2, 1)) |
                        B %among% sample(c("b1", "b2"), 1))

  f1 = tryCatch(CnfFormula(list(cl1, extra)), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(list(cl2, extra)), error = function(e) NULL)
  f3 = tryCatch(CnfFormula(list(cl3, extra)), error = function(e) NULL)

  if (!is.null(f1) && !is.null(f2) && !is.null(f3)) {
    n_tests = n_tests + 1
    t1 = evaluate_formula(f1, u)
    t2 = evaluate_formula(f2, u)
    t3 = evaluate_formula(f3, u)
    if (!all(t1 == t2) || !all(t1 == t3)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [symorder trial %d]: different results for different atom orderings\n", trial))
    }
  }
}
cat(sprintf("  Symbol ordering: %d tests, %d failures\n", n_tests, n_failures))

# === Stress test with many clauses on 2 large-domain variables ===
cat("\n=== Many clauses, 2 large-domain vars ===\n")
set.seed(47004)

for (trial in 1:100) {
  u = CnfUniverse()
  ds1 = sample(5:7, 1); ds2 = sample(5:7, 1)
  X = CnfSymbol(u, "X", paste0("x", 1:ds1))
  Y = CnfSymbol(u, "Y", paste0("y", 1:ds2))
  syms = list(X = X, Y = Y)

  # Many clauses (15-25) to stress the pairwise comparison
  n_clauses = sample(15:25, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:2, 1)
    chosen = sample(c("X", "Y"), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      n_vals = sample(1:(length(dom)-1), 1)
      syms[[s]] %among% sample(dom, n_vals)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [manycl trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("manycl-%d", trial))
}
cat(sprintf("  Many clauses: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
