#!/usr/bin/env Rscript
# Extreme edge cases that could trip up the simplifier:
# - All clauses identical
# - All clauses are tautologies
# - All clauses contradict each other
# - Single variable formulas
# - Formulas where every clause contains every variable
# - Very long single clauses
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

# === All identical clauses ===
cat("=== All identical clauses ===\n")
set.seed(66001)

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a", "b", "c"))
  B = CnfSymbol(u, "B", c("x", "y"))

  cl = as.CnfClause(A %among% sample(c("a", "b", "c"), sample(1:2, 1)) |
                     B %among% sample(c("x", "y"), 1))

  # N identical clauses
  n_copies = sample(2:10, 1)
  clauses = rep(list(cl), n_copies)

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [identical-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("identical-%d", trial))
}
cat(sprintf("  All identical: %d tests, %d failures\n", n_tests, n_failures))

# === Single variable formulas ===
cat("\n=== Single variable formulas ===\n")
set.seed(66002)

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a", "b", "c", "d"))

  n_cl = sample(2:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    as.CnfClause(A %among% sample(c("a", "b", "c", "d"), sample(1:3, 1)))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [single-var-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("single-var-%d", trial))
}
cat(sprintf("  Single variable: %d tests, %d failures\n", n_tests, n_failures))

# === Every clause contains every variable ===
cat("\n=== Full-width clauses ===\n")
set.seed(66003)

for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(3:5, 1)
  dom_size = sample(2:3, 1)
  dom = paste0("d", 1:dom_size)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    # Every clause uses ALL variables
    atoms = lapply(names(syms), function(s) {
      syms[[s]] %among% sample(dom, sample(1:max(1, dom_size-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [full-width-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("full-width-%d", trial))
}
cat(sprintf("  Full-width clauses: %d tests, %d failures\n", n_tests, n_failures))

# === Clauses that are almost tautologies ===
cat("\n=== Almost-tautology clauses ===\n")
set.seed(66004)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c", "d"))
  }

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    # Each atom covers most of the domain (3 out of 4 values)
    n_atoms = sample(2:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c", "d"), 3)  # 3 of 4 values
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [almost-taut-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("almost-taut-%d", trial))
}
cat(sprintf("  Almost-tautology: %d tests, %d failures\n", n_tests, n_failures))

# === Clauses that are almost contradictions ===
cat("\n=== Almost-contradiction clauses ===\n")
set.seed(66005)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c", "d"))
  }

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    # Each atom covers only 1 value (very narrow)
    n_atoms = sample(1:2, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c", "d"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [almost-contra-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("almost-contra-%d", trial))
}
cat(sprintf("  Almost-contradiction: %d tests, %d failures\n", n_tests, n_failures))

# === Mix of unit and very long clauses ===
cat("\n=== Units + very long clauses ===\n")
set.seed(66006)

for (trial in 1:100) {
  u = CnfUniverse()
  n_vars = 7
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }

  clauses = list()
  # 1-2 units
  for (i in 1:sample(1:2, 1)) {
    s = sample(names(syms), 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(c("0", "1"), 1))
  }
  # Very long clauses (5-7 variables each)
  for (i in 1:sample(3:5, 1)) {
    n_atoms = sample(5:7, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1"), 1)
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [unit-long-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("unit-long-%d", trial))
}
cat(sprintf("  Units + very long: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
