#!/usr/bin/env Rscript
# Test with larger domain sizes (5-7 values per symbol)
# This stresses the set operations and subset checking more
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

# === 2 variables with domain size 5-7 ===
cat("=== 2 vars, domain 5-7 ===\n")
set.seed(46001)

for (trial in 1:300) {
  u = CnfUniverse()
  ds1 = sample(5:7, 1); ds2 = sample(5:7, 1)
  X = CnfSymbol(u, "X", paste0("x", 1:ds1))
  Y = CnfSymbol(u, "Y", paste0("y", 1:ds2))
  syms = list(X = X, Y = Y)

  n_clauses = sample(3:8, 1)
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
    cat(sprintf("ERROR [2var trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("2var-%d", trial))
}
cat(sprintf("  2 vars large domain: %d tests, %d failures\n", n_tests, n_failures))

# === 3 variables with domain size 4-5 ===
cat("\n=== 3 vars, domain 4-5 ===\n")
set.seed(46002)

for (trial in 1:200) {
  u = CnfUniverse()
  ds = sample(4:5, 3, replace = TRUE)
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:ds[v]))
  }

  n_clauses = sample(4:10, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
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
    cat(sprintf("ERROR [3var trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("3var-%d", trial))
}
cat(sprintf("  3 vars medium domain: %d tests, %d failures\n", n_tests, n_failures))

# === 2 variables with domain size 8-10 (very large) ===
cat("\n=== 2 vars, domain 8-10 ===\n")
set.seed(46003)

for (trial in 1:100) {
  u = CnfUniverse()
  ds1 = sample(8:10, 1); ds2 = sample(8:10, 1)
  X = CnfSymbol(u, "X", paste0("x", 1:ds1))
  Y = CnfSymbol(u, "Y", paste0("y", 1:ds2))
  syms = list(X = X, Y = Y)

  n_clauses = sample(4:12, 1)
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
    cat(sprintf("ERROR [bigdom trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("bigdom-%d", trial))
}
cat(sprintf("  2 vars very large domain: %d tests, %d failures\n", n_tests, n_failures))

# === Clauses with range sizes close to domain size (near-tautological) ===
cat("\n=== Near-tautological clauses ===\n")
set.seed(46004)

for (trial in 1:200) {
  u = CnfUniverse()
  ds = sample(4:6, 3, replace = TRUE)
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:ds[v]))
  }

  n_clauses = sample(3:6, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(2:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      # Use large ranges (near domain size)
      n_vals = max(1, length(dom) - sample(1:2, 1))
      syms[[s]] %among% sample(dom, n_vals)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [neartaut trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("neartaut-%d", trial))
}
cat(sprintf("  Near-tautological: %d tests, %d failures\n", n_tests, n_failures))

# === Clauses with single-value ranges (very constrained) ===
cat("\n=== Single-value ranges (highly constrained) ===\n")
set.seed(46005)

for (trial in 1:200) {
  u = CnfUniverse()
  ds = sample(3:5, 3, replace = TRUE)
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:ds[v]))
  }

  n_clauses = sample(3:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, 1)  # single value only
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [single trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("single-%d", trial))
}
cat(sprintf("  Single-value: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
