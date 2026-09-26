#!/usr/bin/env Rscript
# Extreme configurations:
# - 2 variables, domain 2 (binary SAT, minimal), many clauses
# - 8+ variables binary, sparse (few clauses per variable)
# - 3 variables domain 6+ (large domain, many range combinations)
# - Single variable (all clauses are units)
# - All clauses identical
# - All clauses complementary (x & !x patterns)
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

evaluate_raw_clauses = function(clauses, universe) {
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
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
  raw_truth
}

# === 2 vars binary, many clauses ===
cat("=== 2 vars binary, many clauses ===\n")
set.seed(154001)

for (trial in 1:500) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("0", "1"))
  B = CnfSymbol(u, "B", c("0", "1"))

  # With binary domain, only 1 possible non-trivial range per symbol: 1 value
  # All clauses are of form: A in {0} or A in {1} or B in {0} or B in {1}
  # Combined: A in {x} | B in {y} for x,y in {0,1}
  # There are only 4 such clauses, but also units: A in {x}, B in {y}
  n_cl = sample(3:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    if (n_sym == 1) {
      sym = sample(c("A", "B"), 1)
      val = sample(c("0", "1"), 1)
      if (sym == "A") as.CnfClause(A %among% val) else as.CnfClause(B %among% val)
    } else {
      a_val = sample(c("0", "1"), 1)
      b_val = sample(c("0", "1"), 1)
      as.CnfClause(A %among% a_val | B %among% b_val)
    }
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [2bin-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [2bin-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2 vars binary: %d tests, %d failures\n", n_tests, n_failures))

# === 8 vars binary, sparse (3-SAT) ===
cat("\n=== 8 vars binary 3-SAT ===\n")
set.seed(154002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1")
  syms = list()
  for (i in 1:8) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # 3-SAT: each clause has exactly 3 variables
  n_cl = sample(10:30, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), 3)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [8sat-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [8sat-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  8 vars 3-SAT: %d tests, %d failures\n", n_tests, n_failures))

# === 3 vars domain 6 ===
cat("\n=== 3 vars domain 6 ===\n")
set.seed(154003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e", "f")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:5, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [3d6-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3d6-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  3 vars domain 6: %d tests, %d failures\n", n_tests, n_failures))

# === Single variable formulas (all units) ===
cat("\n=== Single variable ===\n")
set.seed(154004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom_size = sample(2:8, 1)
  dom = paste0("v", 1:dom_size)
  A = CnfSymbol(u, "A", dom)

  n_cl = sample(2:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    as.CnfClause(A %among% sample(dom, sample(1:(dom_size - 1), 1)))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [single-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [single-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Single variable: %d tests, %d failures\n", n_tests, n_failures))

# === All identical clauses ===
cat("\n=== All identical clauses ===\n")
set.seed(154005)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  chosen = sample(names(syms), sample(1:2, 1))
  atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  base_clause = as.CnfClause(Reduce(`|`, atoms))
  if (isTRUE(unclass(base_clause))) next

  n_dupes = sample(2:8, 1)
  clauses = replicate(n_dupes, base_clause, simplify = FALSE)

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ident-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ident-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  All identical: %d tests, %d failures\n", n_tests, n_failures))

# === Complementary clauses ===
cat("\n=== Complementary clauses ===\n")
set.seed(154006)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create a formula and its negation, then AND them (should be FALSE)
  n_cl = sample(1:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) == 0) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next
  neg_f = tryCatch(!f, error = function(e) NULL)
  if (is.null(neg_f)) next

  # f & !f should be FALSE
  n_tests = n_tests + 1
  result = tryCatch(f & neg_f, error = function(e) NULL)
  if (is.null(result)) next
  truth = evaluate_formula(result, u)
  if (any(truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [comp-%d]: f & !f not FALSE\n", trial))
  }

  # f | !f should be TRUE
  n_tests = n_tests + 1
  result2 = tryCatch(f | neg_f, error = function(e) NULL)
  if (is.null(result2)) next
  truth2 = evaluate_formula(result2, u)
  if (!all(truth2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [comp-or-%d]: f | !f not TRUE\n", trial))
  }
}
cat(sprintf("  Complementary: %d tests, %d failures\n", n_tests, n_failures))

# === Large scale random: many configs ===
cat("\n=== Large scale mixed ===\n")
set.seed(154007)

configs = list(
  list(n_vars = 6, dom_size = 2, n_cl_range = c(8, 20)),   # many binary
  list(n_vars = 2, dom_size = 8, n_cl_range = c(3, 8)),     # few vars, large domain
  list(n_vars = 4, dom_size = 4, n_cl_range = c(5, 12)),    # balanced
  list(n_vars = 3, dom_size = 3, n_cl_range = c(10, 25)),   # many ternary clauses
  list(n_vars = 5, dom_size = 3, n_cl_range = c(6, 15))     # 5 ternary vars
)

for (config in configs) {
  for (trial in 1:200) {
    u = CnfUniverse()
    dom = paste0("v", 1:config$dom_size)
    syms = list()
    for (i in 1:config$n_vars) {
      vname = paste0("X", i)
      syms[[vname]] = CnfSymbol(u, vname, dom)
    }

    n_cl = sample(config$n_cl_range[1]:config$n_cl_range[2], 1)
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:min(4, config$n_vars), 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:(config$dom_size - 1), 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) < 2) next

    n_tests = n_tests + 1
    result = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [large-%d]: %s\n", trial, result$message)); next
    }
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [large-%d]: semantic mismatch\n", trial))
    }
  }
}
cat(sprintf("  Large scale: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
