#!/usr/bin/env Rscript
# Heavy parallel fuzzer: Run with different seed from command line
# Usage: Rscript 12_parallel_heavy_fuzzer.R <seed>

args = commandArgs(trailingOnly = TRUE)
seed = as.integer(args[1])
if (is.na(seed)) seed = 1

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
    cat(sprintf("FAIL [%s] seed=%d: row %d (raw=%s, simp=%s)\n",
      label, seed, idx, raw_truth[idx], simplified_truth[idx]))
    cat(sprintf("  Assignment: %s\n",
      paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
    cat(sprintf("  Clauses (%d):\n", length(raw_clauses)))
    for (ci in seq_along(raw_clauses)) {
      cl = unclass(raw_clauses[[ci]])
      if (is.logical(cl)) {
        cat(sprintf("    %d: %s\n", ci, cl))
      } else {
        cat(sprintf("    %d: %s\n", ci, paste(names(cl), "=", sapply(cl, paste, collapse = ","), collapse = " | ")))
      }
    }
    return(FALSE)
  }
  TRUE
}

make_clause = function(syms, universe, n_atoms_range = 1:3) {
  sym_names = names(syms)
  n_atoms = min(sample(n_atoms_range, 1), length(sym_names))
  chosen = sample(sym_names, n_atoms)
  atoms = lapply(chosen, function(s) {
    dom = universe[[s]]
    n_vals = sample(1:max(1, length(dom) - 1), 1)
    syms[[s]] %among% sample(dom, n_vals)
  })
  as.CnfClause(Reduce(`|`, atoms))
}

set.seed(seed * 7919 + 42)

# Strategy A: 4 ternary variables, many clauses (tests SSE, HLA, unit propagation)
cat(sprintf("=== Strategy A: 4 ternary vars, seed=%d ===\n", seed))
for (trial in 1:500) {
  u_a = CnfUniverse()
  s_a = list(
    A = CnfSymbol(u_a, "A", c("a1", "a2", "a3")),
    B = CnfSymbol(u_a, "B", c("b1", "b2", "b3")),
    C = CnfSymbol(u_a, "C", c("c1", "c2", "c3")),
    D = CnfSymbol(u_a, "D", c("d1", "d2", "d3"))
  )
  n_clauses = sample(3:12, 1)
  clauses = lapply(1:n_clauses, function(j) make_clause(s_a, u_a, 1:3))
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [A trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u_a, sprintf("A-%d", trial))
}
cat(sprintf("  A done: %d tests, %d failures\n", n_tests, n_failures))

# Strategy B: 5 binary variables (more symbols, simpler domains)
cat(sprintf("=== Strategy B: 5 binary vars, seed=%d ===\n", seed))
for (trial in 1:500) {
  u_b = CnfUniverse()
  s_b = list(
    A = CnfSymbol(u_b, "A", c("0", "1")),
    B = CnfSymbol(u_b, "B", c("0", "1")),
    C = CnfSymbol(u_b, "C", c("0", "1")),
    D = CnfSymbol(u_b, "D", c("0", "1")),
    E = CnfSymbol(u_b, "E", c("0", "1"))
  )
  n_clauses = sample(4:15, 1)
  clauses = lapply(1:n_clauses, function(j) make_clause(s_b, u_b, 1:4))
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [B trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u_b, sprintf("B-%d", trial))
}
cat(sprintf("  B done: %d tests, %d failures\n", n_tests, n_failures))

# Strategy C: 3 larger-domain variables (4-5 values)
cat(sprintf("=== Strategy C: 3 larger-domain vars, seed=%d ===\n", seed))
for (trial in 1:400) {
  u_c = CnfUniverse()
  s_c = list(
    P = CnfSymbol(u_c, "P", c("p1", "p2", "p3", "p4")),
    Q = CnfSymbol(u_c, "Q", c("q1", "q2", "q3", "q4", "q5")),
    R = CnfSymbol(u_c, "R", c("r1", "r2", "r3", "r4"))
  )
  n_clauses = sample(3:10, 1)
  clauses = lapply(1:n_clauses, function(j) make_clause(s_c, u_c, 1:3))
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [C trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u_c, sprintf("C-%d", trial))
}
cat(sprintf("  C done: %d tests, %d failures\n", n_tests, n_failures))

# Strategy D: Combining formulas (& and |)
cat(sprintf("=== Strategy D: Combining formulas, seed=%d ===\n", seed))
for (trial in 1:200) {
  u_d = CnfUniverse()
  s_d = list(
    X = CnfSymbol(u_d, "X", c("x1", "x2", "x3")),
    Y = CnfSymbol(u_d, "Y", c("y1", "y2", "y3")),
    Z = CnfSymbol(u_d, "Z", c("z1", "z2", "z3"))
  )

  # Create two sub-formulas
  f1_clauses = lapply(1:sample(2:4, 1), function(j) make_clause(s_d, u_d, 1:2))
  f2_clauses = lapply(1:sample(2:4, 1), function(j) make_clause(s_d, u_d, 1:2))

  f1 = tryCatch(CnfFormula(f1_clauses), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(f2_clauses), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  # Test &
  f_and = tryCatch(f1 & f2, error = function(e) e)
  if (!inherits(f_and, "error")) {
    truth1 = evaluate_formula(f1, u_d)
    truth2 = evaluate_formula(f2, u_d)
    truth_and = evaluate_formula(f_and, u_d)
    n_tests <<- n_tests + 1
    if (!all(truth_and == (truth1 & truth2))) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [D-and trial %d]\n", trial))
    }
  }

  # Test |
  f_or = tryCatch(f1 | f2, error = function(e) e)
  if (!inherits(f_or, "error")) {
    truth1 = evaluate_formula(f1, u_d)
    truth2 = evaluate_formula(f2, u_d)
    truth_or = evaluate_formula(f_or, u_d)
    n_tests <<- n_tests + 1
    if (!all(truth_or == (truth1 | truth2))) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [D-or trial %d]\n", trial))
    }
  }

  # Test negation
  if (!is.logical(unclass(f1))) {
    neg_f1 = tryCatch(!f1, error = function(e) e)
    if (!inherits(neg_f1, "error")) {
      truth1 = evaluate_formula(f1, u_d)
      truth_neg = evaluate_formula(neg_f1, u_d)
      n_tests <<- n_tests + 1
      if (!all(truth_neg == !truth1)) {
        n_failures <<- n_failures + 1
        cat(sprintf("FAIL [D-neg trial %d]\n", trial))
      }
    }
  }
}
cat(sprintf("  D done: %d tests, %d failures\n", n_tests, n_failures))

# Strategy E: Mixed domain sizes
cat(sprintf("=== Strategy E: Mixed domains, seed=%d ===\n", seed))
for (trial in 1:300) {
  u_e = CnfUniverse()
  s_e = list(
    A = CnfSymbol(u_e, "A", c("0", "1")),
    B = CnfSymbol(u_e, "B", c("b1", "b2", "b3")),
    C = CnfSymbol(u_e, "C", c("c1", "c2", "c3", "c4")),
    D = CnfSymbol(u_e, "D", c("d1", "d2"))
  )
  n_clauses = sample(4:12, 1)
  clauses = lapply(1:n_clauses, function(j) make_clause(s_e, u_e, 1:3))
  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [E trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u_e, sprintf("E-%d", trial))
}
cat(sprintf("  E done: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== SEED %d TOTAL: %d tests, %d failures ===\n", seed, n_tests, n_failures))
