#!/usr/bin/env Rscript
# Combined operations stress test:
# - Complex expression trees: f1 & (f2 | f3) & !(f4 | f5)
# - Absorption: f & (f | g) == f
# - Consensus: (f & g) | (f & !g) == f
# - XOR pattern: (f | g) & !(f & g)
# - Triple formula operations: ((f1 & f2) | f3) & f4
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Complex expression trees ===
cat("=== Complex expression trees ===\n")
set.seed(110001)

for (trial in 1:300) {
  u = CnfUniverse()
  d = c("0", "1", "2")
  n_vars = sample(2:3, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  make_formula = function() {
    n_cl = sample(1:2, 1)
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:n_vars, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(d, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula(); f2 = make_formula(); f3 = make_formula()
  f4 = make_formula(); f5 = make_formula()
  if (is.null(f1) || is.null(f2) || is.null(f3) || is.null(f4) || is.null(f5)) next

  # f1 & (f2 | f3) & !(f4 | f5)
  n_tests = n_tests + 1
  result = tryCatch(f1 & (f2 | f3) & !(f4 | f5), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [tree-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u); t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u); t4 = evaluate_formula(f4, u); t5 = evaluate_formula(f5, u)
  expected = t1 & (t2 | t3) & !(t4 | t5)
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [tree-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Complex trees: %d tests, %d failures\n", n_tests, n_failures))

# === Absorption law: f & (f | g) == f ===
cat("\n=== Absorption law ===\n")
set.seed(110002)

for (trial in 1:300) {
  u = CnfUniverse()
  d = c("a", "b", "c")
  n_vars = sample(2:3, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("A", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  make_formula = function() {
    n_cl = sample(1:3, 1)
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:n_vars, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(d, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f = make_formula(); g = make_formula()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1
  f_or_g = tryCatch(f | g, error = function(e) NULL)
  if (is.null(f_or_g)) next

  absorbed = tryCatch(f & f_or_g, error = function(e) e)
  if (inherits(absorbed, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [absorb-%d]: %s\n", trial, absorbed$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_absorbed = evaluate_formula(absorbed, u)
  if (!all(t_absorbed == t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [absorb-%d]: f & (f | g) != f\n", trial))
  }
}
cat(sprintf("  Absorption: %d tests, %d failures\n", n_tests, n_failures))

# === Resolution/consensus: (a & b) | (a & !b) == a ===
cat("\n=== Resolution/consensus ===\n")
set.seed(110003)

for (trial in 1:200) {
  u = CnfUniverse()
  d = c("0", "1", "2")
  A = CnfSymbol(u, "A", d)
  B = CnfSymbol(u, "B", d)
  C = CnfSymbol(u, "C", d)

  # a: some formula
  a_clause = as.CnfClause(A %among% sample(d, sample(1:2, 1)) | C %among% sample(d, sample(1:2, 1)))
  if (isTRUE(unclass(a_clause))) next
  a = CnfFormula(list(a_clause))

  # b: a single-clause formula
  b_clause = as.CnfClause(B %among% sample(d, sample(1:2, 1)))
  if (isTRUE(unclass(b_clause))) next
  b = CnfFormula(list(b_clause))

  n_tests = n_tests + 1
  not_b = tryCatch(!b, error = function(e) NULL)
  if (is.null(not_b)) next

  lhs = tryCatch((a & b) | (a & not_b), error = function(e) e)
  if (inherits(lhs, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [consens-%d]: %s\n", trial, lhs$message)); next
  }

  t_a = evaluate_formula(a, u)
  t_lhs = evaluate_formula(lhs, u)
  if (!all(t_lhs == t_a)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [consens-%d]: (a & b) | (a & !b) != a\n", trial))
  }
}
cat(sprintf("  Resolution/consensus: %d tests, %d failures\n", n_tests, n_failures))

# === XOR pattern: (f | g) & !(f & g) ===
cat("\n=== XOR pattern ===\n")
set.seed(110004)

for (trial in 1:200) {
  u = CnfUniverse()
  d = c("a", "b", "c")
  n_vars = sample(2:3, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  make_formula = function() {
    clauses = list(as.CnfClause(syms[[sample(names(syms), 1)]] %among% sample(d, sample(1:2, 1))))
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f = make_formula(); g = make_formula()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1
  xor_result = tryCatch((f | g) & !(f & g), error = function(e) e)
  if (inherits(xor_result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [xor-%d]: %s\n", trial, xor_result$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_g = evaluate_formula(g, u)
  expected = (t_f | t_g) & !(t_f & t_g)
  t_xor = evaluate_formula(xor_result, u)
  if (!all(t_xor == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [xor-%d]: XOR semantic mismatch\n", trial))
  }
}
cat(sprintf("  XOR pattern: %d tests, %d failures\n", n_tests, n_failures))

# === Chained mixed operations ===
cat("\n=== Chained mixed ops ===\n")
set.seed(110005)

for (trial in 1:200) {
  u = CnfUniverse()
  d = c("0", "1")
  n_vars = sample(3:4, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("B", i)
    syms[[vname]] = CnfSymbol(u, vname, d)
  }

  make_formula = function() {
    n_cl = sample(1:2, 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:min(2, n_vars), 1))
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(d, 1))
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_formula(); f2 = make_formula()
  f3 = make_formula(); f4 = make_formula()
  if (is.null(f1) || is.null(f2) || is.null(f3) || is.null(f4)) next

  n_tests = n_tests + 1
  # ((f1 & f2) | f3) & f4
  result = tryCatch(((f1 & f2) | f3) & f4, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u); t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u); t4 = evaluate_formula(f4, u)
  expected = ((t1 & t2) | t3) & t4
  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Chained mixed ops: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
