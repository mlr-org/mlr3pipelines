#!/usr/bin/env Rscript
# Stress test formula building through various API paths:
# - CnfFormula from mixed CnfClause and CnfFormula lists
# - Repeated AND operations building up large formulas
# - Complex expression: (a & b) | (c & d) | (e & f)
# - Nested negation of complex formulas
# - Building same formula via different paths, checking equivalence
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Build formula incrementally via & ===
cat("=== Incremental AND build ===\n")
set.seed(129001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  syms = list(A = A, B = B, C = C)

  # Build formula by repeatedly ANDing clauses
  n_clauses = sample(3:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  # Path 1: CnfFormula(clauses) - all at once
  n_tests = n_tests + 1
  f_all = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f_all, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [all-%d]: %s\n", trial, f_all$message)); next
  }

  # Path 2: Build incrementally with &
  f_inc = tryCatch({
    f = CnfFormula(list(clauses[[1]]))
    for (i in 2:length(clauses)) {
      f = f & CnfFormula(list(clauses[[i]]))
    }
    f
  }, error = function(e) e)
  if (inherits(f_inc, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [inc-%d]: %s\n", trial, f_inc$message)); next
  }

  # Both should be semantically identical
  t_all = evaluate_formula(f_all, u)
  t_inc = evaluate_formula(f_inc, u)
  if (!all(t_all == t_inc)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [equiv-%d]: batch != incremental\n", trial))
  }
}
cat(sprintf("  Incremental AND: %d tests, %d failures\n", n_tests, n_failures))

# === Complex expression: (f1 & f2) | (f3 & f4) ===
cat("\n=== Complex composite ===\n")
set.seed(129002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  a1 = sample(dom, sample(1:2, 1)); b1 = sample(dom, sample(1:2, 1))
  a2 = sample(dom, sample(1:2, 1)); b2 = sample(dom, sample(1:2, 1))
  a3 = sample(dom, sample(1:2, 1)); b3 = sample(dom, sample(1:2, 1))
  a4 = sample(dom, sample(1:2, 1)); b4 = sample(dom, sample(1:2, 1))

  f1 = CnfFormula(list(as.CnfClause(A %among% a1 | B %among% b1)))
  f2 = CnfFormula(list(as.CnfClause(A %among% a2 | B %among% b2)))
  f3 = CnfFormula(list(as.CnfClause(A %among% a3 | B %among% b3)))
  f4 = CnfFormula(list(as.CnfClause(A %among% a4 | B %among% b4)))

  # (f1 & f2) | (f3 & f4)
  n_tests = n_tests + 1
  result = tryCatch((f1 & f2) | (f3 & f4), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [comp-%d]: %s\n", trial, result$message)); next
  }

  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  # Manual: (A in a1 | B in b1) & (A in a2 | B in b2) | (A in a3 | B in b3) & (A in a4 | B in b4)
  part1 = ((assignments[["A"]] %in% a1) | (assignments[["B"]] %in% b1)) &
    ((assignments[["A"]] %in% a2) | (assignments[["B"]] %in% b2))
  part2 = ((assignments[["A"]] %in% a3) | (assignments[["B"]] %in% b3)) &
    ((assignments[["A"]] %in% a4) | (assignments[["B"]] %in% b4))
  expected = part1 | part2

  truth = evaluate_formula(result, u)
  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [comp-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Complex composite: %d tests, %d failures\n", n_tests, n_failures))

# === Deeply nested negation ===
cat("\n=== Deep negation ===\n")
set.seed(129003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("x", "y")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  f = CnfFormula(list(
    as.CnfClause(A %among% sample(dom, 1) | B %among% sample(dom, 1))
  ))
  t_f = evaluate_formula(f, u)

  # !!f should equal f
  n_tests = n_tests + 1
  dbl = tryCatch(!!f, error = function(e) e)
  if (inherits(dbl, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dbl-%d]: %s\n", trial, dbl$message)); next
  }
  t_dbl = evaluate_formula(dbl, u)
  if (!all(t_dbl == t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dbl-%d]: !!f != f\n", trial))
  }

  # !!!f should equal !f
  n_tests = n_tests + 1
  neg_f = tryCatch(!f, error = function(e) e)
  if (inherits(neg_f, "error")) { next }
  t_neg = evaluate_formula(neg_f, u)

  trpl = tryCatch(!!!f, error = function(e) e)
  if (inherits(trpl, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [trpl-%d]: %s\n", trial, trpl$message)); next
  }
  t_trpl = evaluate_formula(trpl, u)
  if (!all(t_trpl == t_neg)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [trpl-%d]: !!!f != !f\n", trial))
  }
}
cat(sprintf("  Deep negation: %d tests, %d failures\n", n_tests, n_failures))

# === Build from CnfFormula list (constructor) ===
cat("\n=== CnfFormula from formula list ===\n")
set.seed(129004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Create two separate formulas
  f1 = CnfFormula(list(as.CnfClause(A %among% sample(dom, sample(1:2, 1)))))
  f2 = CnfFormula(list(as.CnfClause(B %among% sample(dom, sample(1:2, 1)))))

  # Build combined formula two ways:
  # 1. CnfFormula(list(f1, f2)) - constructor with formula list
  # 2. f1 & f2 - AND operator
  n_tests = n_tests + 1
  f_constr = tryCatch(CnfFormula(list(f1, f2)), error = function(e) e)
  f_and = tryCatch(f1 & f2, error = function(e) e)

  if (inherits(f_constr, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [constr-%d]: %s\n", trial, f_constr$message)); next
  }
  if (inherits(f_and, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [and-%d]: %s\n", trial, f_and$message)); next
  }

  t_constr = evaluate_formula(f_constr, u)
  t_and = evaluate_formula(f_and, u)
  if (!all(t_constr == t_and)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [paths-%d]: constructor != AND operator\n", trial))
  }
}
cat(sprintf("  Formula from formula list: %d tests, %d failures\n", n_tests, n_failures))

# === De Morgan's law for complex formulas ===
cat("\n=== De Morgan complex ===\n")
set.seed(129005)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  syms = list(A = A, B = B, C = C)

  # Create two formulas
  make_formula = function() {
    n_cl = sample(1:3, 1)
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    CnfFormula(clauses)
  }

  f1 = make_formula()
  f2 = make_formula()
  if (is.null(f1) || is.null(f2)) next

  # De Morgan: !(f1 & f2) == !f1 | !f2
  n_tests = n_tests + 1
  lhs = tryCatch(!(f1 & f2), error = function(e) e)
  rhs = tryCatch((!f1) | (!f2), error = function(e) e)
  if (inherits(lhs, "error") || inherits(rhs, "error")) { next }

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dm1-%d]: !(f&g) != !f|!g\n", trial))
  }

  # De Morgan: !(f1 | f2) == !f1 & !f2
  n_tests = n_tests + 1
  lhs2 = tryCatch(!(f1 | f2), error = function(e) e)
  rhs2 = tryCatch((!f1) & (!f2), error = function(e) e)
  if (inherits(lhs2, "error") || inherits(rhs2, "error")) { next }

  t_lhs2 = evaluate_formula(lhs2, u)
  t_rhs2 = evaluate_formula(rhs2, u)
  if (!all(t_lhs2 == t_rhs2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dm2-%d]: !(f|g) != !f&!g\n", trial))
  }
}
cat(sprintf("  De Morgan complex: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
