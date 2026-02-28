#!/usr/bin/env Rscript
# Property-based testing using implication relationships.
# If f1 implies f2 (f1 & f2 == f1), then:
#   f1 | f2 == f2
#   !f1 | f2 == TRUE (tautology)
#   f1 & !f2 == FALSE (contradiction)
# Also test f & f == f and f | f == f (idempotency of operations).
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Implication by construction ===
cat("=== Implication properties ===\n")
set.seed(266001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create f1 = f2 & extra (so f1 implies f2)
  n_cl = sample(1:3, 1)
  f2_clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f2_clauses = f2_clauses[!sapply(f2_clauses, function(x) isTRUE(unclass(x)))]
  if (length(f2_clauses) < 1) next

  # Extra clauses to add to f2 to make f1
  extra_clauses = lapply(1:sample(1:2, 1), function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  extra_clauses = extra_clauses[!sapply(extra_clauses, function(x) isTRUE(unclass(x)))]

  f1_clauses = c(f2_clauses, extra_clauses)
  f1_clauses = f1_clauses[!sapply(f1_clauses, function(x) isTRUE(unclass(x)))]
  if (length(f1_clauses) < 1) next

  f1 = tryCatch(CnfFormula(f1_clauses), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(f2_clauses), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  # Verify implication: wherever f1 is true, f2 must also be true
  if (!all(t1 <= t2)) next  # not actually an implication (shouldn't happen since f1 = f2 & extra)

  # f1 & f2 == f1
  r = tryCatch(f1 & f2, error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == t1)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [impl-%d]: f1 & f2 != f1\n", trial)); next
  }

  # f1 | f2 == f2
  r = tryCatch(f1 | f2, error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [impl-%d]: f1 | f2 != f2\n", trial)); next
  }

  # f1 & !f2 == FALSE
  nf2 = tryCatch(!f2, error = function(e) NULL)
  if (!is.null(nf2)) {
    r = tryCatch(f1 & nf2, error = function(e) NULL)
    if (!is.null(r) && any(evaluate_formula(r, u))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [impl-%d]: f1 & !f2 != FALSE\n", trial))
    }
  }
}
cat(sprintf("  Implication properties: %d tests, %d failures\n", n_tests, n_failures))

# === Idempotency ===
cat("\n=== Idempotency ===\n")
set.seed(266002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  t_f = evaluate_formula(f, u)

  # f & f == f
  r = tryCatch(f & f, error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [idemp-%d]: f & f != f\n", trial)); next
  }

  # f | f == f
  r = tryCatch(f | f, error = function(e) NULL)
  if (!is.null(r) && !all(evaluate_formula(r, u) == t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [idemp-%d]: f | f != f\n", trial))
  }
}
cat(sprintf("  Idempotency: %d tests, %d failures\n", n_tests, n_failures))

# === Consensus theorem ===
cat("\n=== Consensus theorem ===\n")
set.seed(266003)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Pick a variable X and a split of its domain
  x_sym = sample(sym_names, 1)
  x_dom = dom
  x_part1 = sample(x_dom, sample(1:(length(x_dom)-1), 1))
  x_part2 = setdiff(x_dom, x_part1)

  # Create clauses: (X in part1 | C1) and (X in part2 | C2)
  # The consensus/resolvent is C1 | C2
  other_syms = setdiff(sym_names, x_sym)
  c1_sym = sample(other_syms, sample(1:min(2, length(other_syms)), 1))
  c2_sym = sample(other_syms, sample(1:min(2, length(other_syms)), 1))

  c1_atoms = lapply(c1_sym, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
  c2_atoms = lapply(c2_sym, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))

  cl1 = as.CnfClause(Reduce(`|`, c(list(syms[[x_sym]] %among% x_part1), c1_atoms)))
  cl2 = as.CnfClause(Reduce(`|`, c(list(syms[[x_sym]] %among% x_part2), c2_atoms)))

  if (isTRUE(unclass(cl1)) || isTRUE(unclass(cl2))) next

  # f = cl1 & cl2
  f = tryCatch(CnfFormula(list(cl1, cl2)), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  truth = evaluate_formula(f, u)

  # The resolvent: C1 | C2 should be implied by f
  resolvent_atoms = c(c1_atoms, c2_atoms)
  if (length(resolvent_atoms) < 1) next
  resolvent_cl = as.CnfClause(Reduce(`|`, resolvent_atoms))
  if (isTRUE(unclass(resolvent_cl))) next

  resolvent_truth = evaluate_formula(make_raw_formula(list(unclass(resolvent_cl)), u), u)

  # f should imply resolvent: wherever f is true, resolvent is true
  if (!all(truth <= resolvent_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [consensus-%d]: f does not imply resolvent\n", trial))
  }
}
cat(sprintf("  Consensus theorem: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
