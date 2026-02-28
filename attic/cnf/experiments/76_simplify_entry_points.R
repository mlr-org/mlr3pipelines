#!/usr/bin/env Rscript
# Test simplify_cnf entry points: empty input, single clause, all units,
# all eliminated by units, etc.
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
    return(FALSE)
  }
  TRUE
}

# === All-unit formulas ===
cat("=== All-unit formulas ===\n")
set.seed(76001)

for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(2:5, 1)
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # All clauses are units
  n_units = sample(n_vars:min(n_vars * 2, 8), 1)
  clauses = lapply(1:n_units, function(j) {
    s = sample(names(syms), 1)
    as.CnfClause(syms[[s]] %among% sample(dom, 1))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [all-unit-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("all-unit-%d", trial))
}
cat(sprintf("  All-unit: %d tests, %d failures\n", n_tests, n_failures))

# === Only units, no non-unit processing ===
cat("\n=== Unit-only (all same symbol) ===\n")
set.seed(76002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)

  n_units = sample(2:5, 1)
  clauses = lapply(1:n_units, function(j) {
    as.CnfClause(A %among% sample(dom, sample(1:3, 1)))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [unit-only-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("unit-only-%d", trial))
}
cat(sprintf("  Unit-only: %d tests, %d failures\n", n_tests, n_failures))

# === Single non-unit clause ===
cat("\n=== Single clause formulas ===\n")
set.seed(76003)

for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(2:4, 1)
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  chosen = sample(names(syms), sample(2:min(n_vars, 3), 1))
  atoms = lapply(chosen, function(s) {
    syms[[s]] %among% sample(dom, sample(1:2, 1))
  })
  cl = as.CnfClause(Reduce(`|`, atoms))

  f = tryCatch(CnfFormula(list(cl)), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [single-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, list(cl), u, sprintf("single-%d", trial))
}
cat(sprintf("  Single clause: %d tests, %d failures\n", n_tests, n_failures))

# === All clauses subsumed by a single unit ===
cat("\n=== All subsumed by unit ===\n")
set.seed(76004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  unit_val = sample(dom, 1)
  clauses = list(
    as.CnfClause(A %among% unit_val)  # the unit
  )

  # Add clauses that include the unit's value (will be subsumed or reduced)
  for (i in 1:sample(2:4, 1)) {
    vals = unique(c(unit_val, sample(dom, sample(1:2, 1))))
    atoms = list(A %among% vals)
    if (sample(c(TRUE, FALSE), 1)) {
      atoms[[2]] = B %among% sample(dom, sample(1:2, 1))
    }
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [subsumed-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("subsumed-%d", trial))
}
cat(sprintf("  All subsumed: %d tests, %d failures\n", n_tests, n_failures))

# === Formula that simplifies to TRUE ===
cat("\n=== Simplifies to TRUE ===\n")
set.seed(76005)

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)

  # All values of A appear across units -> should simplify to TRUE
  clauses = list(
    as.CnfClause(A %among% c("a", "b")),
    as.CnfClause(A %among% c("b", "c"))
  )
  # Intersection is {b}, so not TRUE. Let me use different strategy.

  # Just check that formulas with full-domain clauses simplify to TRUE
  clauses2 = list(as.CnfClause(A %among% dom))
  f = tryCatch(CnfFormula(clauses2), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [to-true-%d]: %s\n", trial, f$message)); next
  }
  n_tests = n_tests + 1
  if (!isTRUE(as.logical(f))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [to-true-%d]: full-domain clause should give TRUE formula\n", trial))
  }
}
cat(sprintf("  Simplifies to TRUE: %d tests, %d failures\n", n_tests, n_failures))

# === Formula that simplifies to FALSE ===
cat("\n=== Simplifies to FALSE ===\n")
set.seed(76006)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Contradicting units
  a_val = sample(dom, 1)
  b_val = sample(setdiff(dom, a_val), 1)
  clauses = list(
    as.CnfClause(A %among% a_val),
    as.CnfClause(A %among% b_val)
  )

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [to-false-%d]: %s\n", trial, f$message)); next
  }
  n_tests = n_tests + 1
  if (!isFALSE(as.logical(f))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [to-false-%d]: contradicting units should give FALSE formula\n", trial))
  }
}
cat(sprintf("  Simplifies to FALSE: %d tests, %d failures\n", n_tests, n_failures))

# === Large domains ===
cat("\n=== Large domains ===\n")
set.seed(76007)

for (trial in 1:100) {
  u = CnfUniverse()
  dom_size = sample(8:15, 1)
  dom = paste0("v", 1:dom_size)
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    atoms = list()
    if (sample(c(TRUE, FALSE), 1)) {
      atoms[[length(atoms) + 1]] = A %among% sample(dom, sample(1:max(1, dom_size - 2), 1))
    }
    if (sample(c(TRUE, FALSE), 1) || !length(atoms)) {
      atoms[[length(atoms) + 1]] = B %among% sample(dom, sample(1:max(1, dom_size - 2), 1))
    }
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [large-dom-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("large-dom-%d", trial))
}
cat(sprintf("  Large domains: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
