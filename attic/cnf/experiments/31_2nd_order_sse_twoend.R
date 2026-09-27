#!/usr/bin/env Rscript
# Targeted test for 2nd-order SSE twoend patterns
# handle_sse_2nd_order_twoend is the most complex 2nd-order function
# It requires two non-subset symbols, and looks for pairs of clauses that
# together can eliminate a symbol from the target clause
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

set.seed(31337)

# Strategy 1: Classic resolution pattern that triggers 2nd-order SSE
# Clauses: C = (X in {a} | Y in {d}), D = (X in {b} | Y in {e}), E = (Y in {d,e})
# 2nd order: D is a twoend relative to C (X and Y both not subsets)
# If there's a oneend clause that shares X with C (like D1: X in {b} | Z in {g}),
# the twoend function should combine them.
cat("=== Classic resolution patterns ===\n")
for (trial in 1:500) {
  u = CnfUniverse()
  n_x = sample(2:4, 1); n_y = sample(2:4, 1); n_z = sample(2:3, 1)
  X = CnfSymbol(u, "X", paste0("x", 1:n_x))
  Y = CnfSymbol(u, "Y", paste0("y", 1:n_y))
  Z = CnfSymbol(u, "Z", paste0("z", 1:n_z))
  syms = list(X = X, Y = Y, Z = Z)

  dom_x = u[["X"]]; dom_y = u[["Y"]]; dom_z = u[["Z"]]

  # Build resolution-triggering pattern
  clauses = list()

  # Two clauses that partition X and have different Y values
  x_split = sample(dom_x, sample(1:(length(dom_x)-1), 1))
  x_rest = setdiff(dom_x, x_split)
  y_val1 = sample(dom_y, sample(1:max(1, length(dom_y)-1), 1))
  y_val2 = sample(dom_y, sample(1:max(1, length(dom_y)-1), 1))

  clauses[[1]] = as.CnfClause(X %among% x_split | Y %among% y_val1)
  clauses[[2]] = as.CnfClause(X %among% x_rest | Y %among% y_val2)

  # A clause that could be eliminated by resolution
  y_combined = union(y_val1, y_val2)
  if (length(y_combined) < length(dom_y)) {
    clauses[[3]] = as.CnfClause(Y %among% y_combined)
  } else {
    # If y_combined covers all of Y, use a different pattern
    clauses[[3]] = as.CnfClause(Y %among% y_combined | Z %among% sample(dom_z, sample(1:max(1, length(dom_z)-1), 1)))
  }

  # Add some random extra clauses
  for (i in 1:sample(0:3, 1)) {
    v1 = sample(names(syms), 1)
    v2 = sample(setdiff(names(syms), v1), 1)
    d1 = u[[v1]]; d2 = u[[v2]]
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[v1]] %among% sample(d1, sample(1:max(1, length(d1)-1), 1)) |
      syms[[v2]] %among% sample(d2, sample(1:max(1, length(d2)-1), 1))
    )
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [res trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("res-%d", trial))
}
cat(sprintf("  Resolution patterns: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 2: 4-variable patterns that may trigger twoend
cat("\n=== 4-variable twoend patterns ===\n")
for (trial in 1:500) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2"))
  B = CnfSymbol(u, "B", c("b1", "b2"))
  C = CnfSymbol(u, "C", c("c1", "c2"))
  D = CnfSymbol(u, "D", c("d1", "d2"))
  syms = list(A = A, B = B, C = C, D = D)

  # Create clauses with 2-3 symbols each (to get twoend relationships)
  n_clauses = sample(4:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(2:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [4var trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("4var-%d", trial))
}
cat(sprintf("  4-variable twoend: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 3: Patterns designed to trigger cascading 2nd-order SSE
# Create a chain where 2nd-order SSE on one pair enables further 2nd-order SSE
cat("\n=== Cascading 2nd-order SSE ===\n")
for (trial in 1:300) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("x1", "x2", "x3"))
  Y = CnfSymbol(u, "Y", c("y1", "y2", "y3"))
  Z = CnfSymbol(u, "Z", c("z1", "z2"))
  W = CnfSymbol(u, "W", c("w1", "w2"))
  syms = list(X = X, Y = Y, Z = Z, W = W)

  # Resolution subsumption pattern + extra clauses that get simplified by the resolution
  dom_x = u[["X"]]
  # Split X domain into two non-overlapping parts
  x_val1 = dom_x[1]
  x_val2 = dom_x[2]

  clauses = list(
    as.CnfClause(X %among% x_val1 | Y %among% sample(u[["Y"]], sample(1:2, 1))),
    as.CnfClause(X %among% x_val2 | Y %among% sample(u[["Y"]], sample(1:2, 1))),
    as.CnfClause(X %among% dom_x[3] | Z %among% sample(u[["Z"]], 1)),
    as.CnfClause(Y %among% sample(u[["Y"]], sample(1:2, 1)) | Z %among% sample(u[["Z"]], 1) | W %among% sample(u[["W"]], 1))
  )
  # Add 2-4 more random clauses
  for (i in 1:sample(2:4, 1)) {
    n_atoms = sample(2:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [cascade trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("cascade-%d", trial))
}
cat(sprintf("  Cascading 2nd-order: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
