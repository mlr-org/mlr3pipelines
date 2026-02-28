#!/usr/bin/env Rscript
# Stress test the apply_domain_restriction path specifically
# Focus on cases where domain restriction creates new units (cascading),
# eliminates symbols from clauses, and interacts with the is_not_subset_of matrix
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

set.seed(24680)

# Strategy 1: Formulas where multiple units restrict the same non-unit clause
# This forces apply_domain_restriction to be called multiple times on the same clause
cat("=== Multiple units restrict same clause ===\n")
for (trial in 1:300) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2", "b3"))
  C = CnfSymbol(u, "C", c("c1", "c2"))
  D = CnfSymbol(u, "D", c("d1", "d2", "d3"))
  syms = list(A = A, B = B, C = C, D = D)

  clauses = list()
  # Add 2-3 units with different symbols
  unit_syms = sample(names(syms), sample(2:3, 1))
  for (s in unit_syms) {
    dom = u[[s]]
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1)))
  }

  # Add clauses that overlap with ALL the unit symbols
  for (i in 1:sample(2:4, 1)) {
    # Include all unit symbols + possibly others
    included = union(unit_syms, sample(names(syms), sample(0:1, 1)))
    atoms = lapply(included, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [multi-unit trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("multi-unit-%d", trial))
}
cat(sprintf("  Multi-unit restriction: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 2: Domain restriction that creates new unit which creates further restriction
cat("\n=== Cascading domain restriction -> unit creation ===\n")
for (trial in 1:300) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("x1", "x2"))
  Y = CnfSymbol(u, "Y", c("y1", "y2"))
  Z = CnfSymbol(u, "Z", c("z1", "z2"))
  W = CnfSymbol(u, "W", c("w1", "w2"))
  syms = list(X = X, Y = Y, Z = Z, W = W)

  # Design: unit for X -> restricts clauses with X and Y -> Y becomes unit ->
  # restricts clauses with Y and Z -> Z becomes unit -> restricts clauses with Z and W
  clauses = list()

  # X unit
  clauses[[1]] = as.CnfClause(X %among% sample(c("x1", "x2"), 1))

  # Clause with X and Y that will become unit Y after X restriction
  x_val = unclass(clauses[[1]])[["X"]]
  x_other = setdiff(c("x1", "x2"), x_val)
  y_val = sample(c("y1", "y2"), 1)
  clauses[[2]] = as.CnfClause(X %among% x_other | Y %among% y_val)

  # Clause with Y and Z
  y_other = setdiff(c("y1", "y2"), y_val)
  z_val = sample(c("z1", "z2"), 1)
  clauses[[3]] = as.CnfClause(Y %among% y_other | Z %among% z_val)

  # Clause with Z and W
  z_other = setdiff(c("z1", "z2"), z_val)
  w_val = sample(c("w1", "w2"), 1)
  clauses[[4]] = as.CnfClause(Z %among% z_other | W %among% w_val)

  # Add some random extra clauses
  for (i in 1:sample(0:3, 1)) {
    s1 = sample(names(syms), 1)
    s2 = sample(setdiff(names(syms), s1), 1)
    d1 = u[[s1]]; d2 = u[[s2]]
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[s1]] %among% sample(d1, 1) |
      syms[[s2]] %among% sample(d2, 1)
    )
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [cascade-unit trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("cascade-unit-%d", trial))
}
cat(sprintf("  Cascading units: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 3: Domain restriction that makes a symbol empty (FALSE clause path)
cat("\n=== Domain restriction -> empty symbol -> FALSE clause ===\n")
for (trial in 1:300) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2"))
  C = CnfSymbol(u, "C", c("c1", "c2", "c3"))
  syms = list(A = A, B = B, C = C)

  # Create a formula where unit propagation empties a symbol
  a_unit = sample(c("a1", "a2", "a3"), sample(1:2, 1))
  a_rest = setdiff(c("a1", "a2", "a3"), a_unit)

  clauses = list(
    as.CnfClause(A %among% a_unit),
    # This clause's A range is disjoint from the unit - will be emptied
    as.CnfClause(A %among% a_rest | B %among% sample(c("b1", "b2"), 1) | C %among% sample(c("c1", "c2", "c3"), sample(1:2, 1)))
  )

  # Add more clauses
  for (i in 1:sample(1:3, 1)) {
    s1 = sample(names(syms), 1)
    s2 = sample(setdiff(names(syms), s1), 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[s1]] %among% sample(u[[s1]], sample(1:max(1, length(u[[s1]])-1), 1)) |
      syms[[s2]] %among% sample(u[[s2]], sample(1:max(1, length(u[[s2]])-1), 1))
    )
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [empty-sym trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("empty-sym-%d", trial))
}
cat(sprintf("  Empty symbol: %d tests, %d failures\n", n_tests, n_failures))

# Strategy 4: Conflicting units (should create FALSE)
cat("\n=== Conflicting units ===\n")
for (trial in 1:300) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("x1", "x2", "x3"))
  Y = CnfSymbol(u, "Y", c("y1", "y2"))
  syms = list(X = X, Y = Y)

  # Two units for X with disjoint ranges
  x_val1 = sample(c("x1", "x2", "x3"), sample(1:2, 1))
  x_val2 = sample(c("x1", "x2", "x3"), sample(1:2, 1))

  clauses = list(
    as.CnfClause(X %among% x_val1),
    as.CnfClause(X %among% x_val2)
  )

  # Add some other clauses
  for (i in 1:sample(1:3, 1)) {
    clauses[[length(clauses) + 1]] = as.CnfClause(
      X %among% sample(c("x1", "x2", "x3"), sample(1:2, 1)) |
      Y %among% sample(c("y1", "y2"), 1)
    )
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [conflict trial %d]: %s\n", trial, f$message)); next
  }

  # Check: if ranges are disjoint, result should be FALSE
  is_contra = length(intersect(x_val1, x_val2)) == 0
  check_fc(f, clauses, u, sprintf("conflict-%d", trial))
}
cat(sprintf("  Conflicting units: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
