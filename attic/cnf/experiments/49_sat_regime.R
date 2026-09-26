#!/usr/bin/env Rscript
# Test in the SAT regime: many binary variables, many clauses
# This is where the simplifier operates most like a SAT preprocessor
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

# === 7 binary variables, 3-SAT like ===
cat("=== 7 binary vars, 3-SAT ===\n")
set.seed(49001)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:7) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }

  # 3-SAT: each clause has exactly 3 literals
  n_clauses = sample(10:20, 1)
  clauses = lapply(1:n_clauses, function(j) {
    chosen = sample(names(syms), 3)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [3sat trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("3sat-%d", trial))
}
cat(sprintf("  3-SAT: %d tests, %d failures\n", n_tests, n_failures))

# === 8 binary variables, mixed clause widths ===
cat("\n=== 8 binary vars, mixed widths ===\n")
set.seed(49002)

for (trial in 1:100) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:8) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }

  n_clauses = sample(8:15, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:5, 1)
    chosen = sample(names(syms), min(n_atoms, 8))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [8var trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("8var-%d", trial))
}
cat(sprintf("  8 binary vars: %d tests, %d failures\n", n_tests, n_failures))

# === Heavy clause ratio (many more clauses than variables) ===
cat("\n=== Heavy clause ratio ===\n")
set.seed(49003)

for (trial in 1:100) {
  u = CnfUniverse()
  syms = list()
  n_vars = sample(4:6, 1)
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }

  # Very many clauses relative to variables
  n_clauses = sample(20:40, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(2:min(4, n_vars), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [heavy trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("heavy-%d", trial))
}
cat(sprintf("  Heavy clause ratio: %d tests, %d failures\n", n_tests, n_failures))

# === Phase transition region (clause ratio ~4.26 for 3-SAT) ===
# This is where SAT instances are hardest
cat("\n=== Phase transition region ===\n")
set.seed(49004)

for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(5:7, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }

  # clause-to-variable ratio near 4.26 (3-SAT phase transition)
  n_clauses = round(n_vars * runif(1, 3.5, 5.0))
  clauses = lapply(1:n_clauses, function(j) {
    chosen = sample(names(syms), 3)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [phase trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("phase-%d", trial))
}
cat(sprintf("  Phase transition: %d tests, %d failures\n", n_tests, n_failures))

# === Non-binary SAT-like: 3-valued variables ===
cat("\n=== 3-valued SAT-like ===\n")
set.seed(49005)

for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(4:5, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1", "2"))
  }

  n_clauses = sample(6:15, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(2:min(4, n_vars), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1", "2"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [3val trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("3val-%d", trial))
}
cat(sprintf("  3-valued SAT-like: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
