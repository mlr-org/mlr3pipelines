#!/usr/bin/env Rscript
# Mutation testing: take a formula, make a small change to one clause
# (add/remove a value, add/remove a symbol), and verify the result
# either stays the same or changes correctly. This tests that the
# simplifier is sensitive to small input changes.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Mutate range: add a value ===
cat("=== Add value mutation ===\n")
set.seed(270001)

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
  if (length(clauses) < 2) next

  # Original formula
  f_orig = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f_orig)) next
  t_orig = evaluate_formula(f_orig, u)

  # Mutate: pick a clause and symbol, add a value to its range
  mut_cl_idx = sample(length(clauses), 1)
  mut_cl = unclass(clauses[[mut_cl_idx]])
  if (is.logical(mut_cl)) next
  mut_sym = sample(names(mut_cl), 1)
  missing_vals = setdiff(dom, mut_cl[[mut_sym]])
  if (length(missing_vals) == 0) next  # already full domain

  new_range = c(mut_cl[[mut_sym]], sample(missing_vals, 1))
  # Create mutated clause
  mut_cl[[mut_sym]] = new_range
  if (all(dom %in% new_range)) {
    # This symbol covers entire domain -> clause is tautological (since it's an OR)
    # Remove the entire clause from the formula
    mut_clauses = clauses[-mut_cl_idx]
  } else {
    mut_clauses = clauses
    mut_clauses[[mut_cl_idx]] = structure(mut_cl, universe = u, class = "CnfClause")
  }

  mut_clauses = mut_clauses[!sapply(mut_clauses, function(x) isTRUE(unclass(x)))]
  if (length(mut_clauses) < 1) {
    # Formula is TRUE, which is always at least as permissive
    n_tests = n_tests + 1
    next
  }

  f_mut = tryCatch(CnfFormula(mut_clauses), error = function(e) NULL)
  if (is.null(f_mut)) next

  n_tests = n_tests + 1
  t_mut = evaluate_formula(f_mut, u)

  # Adding a value to a clause makes it weaker (more permissive)
  # So the mutated formula should be at least as permissive as original
  if (!all(t_orig <= t_mut)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [addval-%d]: adding value made formula more restrictive\n", trial))
  }
}
cat(sprintf("  Add value mutation: %d tests, %d failures\n", n_tests, n_failures))

# === Mutate range: remove a value ===
cat("\n=== Remove value mutation ===\n")
set.seed(270002)

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
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(2:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f_orig = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f_orig)) next
  t_orig = evaluate_formula(f_orig, u)

  # Mutate: remove a value from a clause
  mut_cl_idx = sample(length(clauses), 1)
  mut_cl = unclass(clauses[[mut_cl_idx]])
  if (is.logical(mut_cl)) next
  # Find a symbol with >1 value
  sym_candidates = names(mut_cl)[lengths(mut_cl) > 1]
  if (length(sym_candidates) == 0) next
  mut_sym = sample(sym_candidates, 1)

  new_range = mut_cl[[mut_sym]][-sample(length(mut_cl[[mut_sym]]), 1)]
  mut_cl[[mut_sym]] = new_range
  mut_clauses = clauses
  mut_clauses[[mut_cl_idx]] = structure(mut_cl, universe = u, class = "CnfClause")

  f_mut = tryCatch(CnfFormula(mut_clauses), error = function(e) NULL)
  if (is.null(f_mut)) next

  n_tests = n_tests + 1
  t_mut = evaluate_formula(f_mut, u)

  # Removing a value from a clause makes it stronger (more restrictive)
  # So the mutated formula should be at most as permissive as original
  if (!all(t_mut <= t_orig)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [rmval-%d]: removing value made formula more permissive\n", trial))
  }
}
cat(sprintf("  Remove value mutation: %d tests, %d failures\n", n_tests, n_failures))

# === Add a clause mutation ===
cat("\n=== Add clause mutation ===\n")
set.seed(270003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(2:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  f_orig = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f_orig)) next
  t_orig = evaluate_formula(f_orig, u)

  # Add a random clause (AND conjunction adds a constraint)
  extra_atoms = lapply(sample(sym_names, sample(1:2, 1)), function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
  extra_cl = as.CnfClause(Reduce(`|`, extra_atoms))
  if (isTRUE(unclass(extra_cl))) next

  f_stronger = tryCatch(CnfFormula(c(clauses, list(extra_cl))), error = function(e) NULL)
  if (is.null(f_stronger)) next

  n_tests = n_tests + 1
  t_stronger = evaluate_formula(f_stronger, u)

  # Adding a clause (AND) makes the formula at most as permissive
  if (!all(t_stronger <= t_orig)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [addcl-%d]: adding clause made formula more permissive\n", trial))
  }
}
cat(sprintf("  Add clause mutation: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
