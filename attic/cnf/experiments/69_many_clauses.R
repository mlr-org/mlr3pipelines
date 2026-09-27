#!/usr/bin/env Rscript
# Test with many clauses (30-100) to stress the N^2 pairwise comparison
# and find issues with matrix indexing at scale
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

# === Many binary clauses, binary domains ===
cat("=== Many binary clauses (30-50), binary domains ===\n")
set.seed(69001)

for (trial in 1:100) {
  u = CnfUniverse()
  n_vars = sample(3:5, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }

  n_cl = sample(30:50, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:min(n_vars, 3), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [many-bin-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("many-bin-%d", trial))
}
cat(sprintf("  Many binary: %d tests, %d failures\n", n_tests, n_failures))

# === Many clauses with larger domains ===
cat("\n=== Many clauses (20-40), domain size 3 ===\n")
set.seed(69002)

for (trial in 1:100) {
  u = CnfUniverse()
  n_vars = sample(3:4, 1)
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(20:40, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:min(n_vars, 3), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [many-dom3-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("many-dom3-%d", trial))
}
cat(sprintf("  Many domain-3: %d tests, %d failures\n", n_tests, n_failures))

# === Very many clauses, few variables ===
cat("\n=== Very many clauses (50-100), 2 variables ===\n")
set.seed(69003)

for (trial in 1:50) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  n_cl = sample(50:100, 1)
  clauses = lapply(1:n_cl, function(j) {
    if (sample(c(TRUE, FALSE), 1)) {
      as.CnfClause(A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1)))
    } else {
      as.CnfClause(A %among% sample(dom, sample(1:3, 1)))
    }
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [very-many-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("very-many-%d", trial))
}
cat(sprintf("  Very many clauses: %d tests, %d failures\n", n_tests, n_failures))

# === Many clauses with high variable count ===
cat("\n=== Many clauses (30-60), 6-8 variables, binary ===\n")
set.seed(69004)

for (trial in 1:50) {
  u = CnfUniverse()
  n_vars = sample(6:8, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }

  n_cl = sample(30:60, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:min(n_vars, 4), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [many-hivar-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("many-hivar-%d", trial))
}
cat(sprintf("  Many clauses high-var: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
