#!/usr/bin/env Rscript
# Test patterns designed to trigger eliminate_symbol_from_clause cascading.
# When a symbol is eliminated from a clause (its range becomes empty after
# domain restriction), the clause becomes shorter. If it becomes length 1,
# it becomes a unit, triggering further propagation. If it becomes length 0,
# it's a contradiction.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Elimination to unit ===
cat("=== Elimination to unit ===\n")
set.seed(263001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create a unit that will restrict another 2-symbol clause to a unit
  s_unit = sample(sym_names, 1)
  s_other1 = sample(setdiff(sym_names, s_unit), 1)
  s_other2 = sample(setdiff(sym_names, c(s_unit, s_other1)), 1)

  unit_range = sample(dom, sample(1:2, 1))
  # The 2-symbol clause's range for s_unit should NOT overlap with unit_range,
  # so unit propagation eliminates s_unit, making the clause a unit
  other_range = setdiff(dom, unit_range)
  if (length(other_range) == 0) next  # degenerate case

  clauses = list(
    as.CnfClause(syms[[s_unit]] %among% unit_range),  # unit
    as.CnfClause(syms[[s_unit]] %among% other_range | syms[[s_other1]] %among% sample(dom, sample(1:2, 1)))  # becomes unit
  )

  # Add clauses affected by the cascade
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[sample(length(clauses))]  # shuffle
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [elunit-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [elunit-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Elimination to unit: %d tests, %d failures\n", n_tests, n_failures))

# === Elimination to contradiction ===
cat("\n=== Elimination to contradiction ===\n")
set.seed(263002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create clauses that should produce a contradiction via cascading elimination
  # V1 in {a}, V1 in {b,c} | V2 in {a}, V2 in {b,c} | V3 in {a}, V3 in {b,c}
  # After unit prop: V1={a}, V2={a}, V3={a}, then V3 in {b,c} contradicts
  chain_length = sample(2:4, 1)
  clauses = list()

  first_val = sample(dom, 1)
  rest_vals = setdiff(dom, first_val)

  # Starting unit
  clauses[[1]] = as.CnfClause(syms[[sym_names[1]]] %among% first_val)

  for (k in 1:chain_length) {
    s_cur = sym_names[k]
    s_next = sym_names[min(k + 1, length(sym_names))]
    next_val = sample(dom, 1)
    # Clause: s_cur in rest_vals | s_next in {next_val}
    # After unit prop restricts s_cur, this might become a unit for s_next
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[s_cur]] %among% rest_vals | syms[[s_next]] %among% next_val
    )
  }

  # Add some random clauses
  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[sample(length(clauses))]
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [elcontrad-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [elcontrad-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Elimination to contradiction: %d tests, %d failures\n", n_tests, n_failures))

# === Multi-symbol elimination from same clause ===
cat("\n=== Multi-symbol elimination ===\n")
set.seed(263003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create multiple units, and a wide clause that will have multiple symbols eliminated
  units_to_create = sample(2:3, 1)
  unit_syms = sample(sym_names, units_to_create)
  clauses = list()
  for (s in unit_syms) {
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
  }

  # Wide clause using all symbols, with ranges that partially conflict with units
  all_atoms = lapply(sym_names, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
  clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, all_atoms))

  # More clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  clauses = clauses[sample(length(clauses))]
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multielim-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multielim-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multi-symbol elimination: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
