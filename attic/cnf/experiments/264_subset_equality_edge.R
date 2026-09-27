#!/usr/bin/env Rscript
# Test edge cases around subset/equality detection.
# Focus on: ranges that are exactly equal, ranges that differ by one value,
# and patterns where the subset relationship changes during simplification.
# Tests the pairwise loop's subset detection at lines 587-588.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Equal ranges across clauses ===
cat("=== Equal ranges ===\n")
set.seed(264001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create clauses with deliberately equal ranges for some symbols
  shared_sym = sample(sym_names, 1)
  shared_range = sample(dom, sample(2:3, 1))

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    if (!shared_sym %in% chosen) chosen[1] = shared_sym
    atoms = lapply(chosen, function(s) {
      if (s == shared_sym) {
        syms[[s]] %among% shared_range  # same range in all clauses
      } else {
        syms[[s]] %among% sample(dom, sample(1:3, 1))
      }
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [eqrange-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [eqrange-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Equal ranges: %d tests, %d failures\n", n_tests, n_failures))

# === Near-subset ranges (differ by exactly one value) ===
cat("\n=== Near-subset ranges ===\n")
set.seed(264002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create pairs of clauses where one symbol's range is nearly a subset of the other
  s1 = sample(sym_names, 1); s2 = sample(setdiff(sym_names, s1), 1)
  base_range = sample(dom, 2)
  extra_val = sample(setdiff(dom, base_range), 1)

  clauses = list(
    as.CnfClause(syms[[s1]] %among% base_range | syms[[s2]] %among% sample(dom, sample(1:2, 1))),
    as.CnfClause(syms[[s1]] %among% c(base_range, extra_val) | syms[[s2]] %among% sample(dom, sample(1:2, 1)))
  )

  # More random clauses
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
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
    cat(sprintf("ERROR [nearsub-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [nearsub-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Near-subset ranges: %d tests, %d failures\n", n_tests, n_failures))

# === Identical clauses (should be subsumed) ===
cat("\n=== Identical clauses ===\n")
set.seed(264003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create a clause and duplicate it
  n_sym = sample(2:3, 1)
  chosen = sample(sym_names, n_sym)
  atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
  cl = as.CnfClause(Reduce(`|`, atoms))
  if (isTRUE(unclass(cl))) next

  # Put duplicates with other clauses
  clauses = list(cl, cl)
  for (j in 1:sample(1:4, 1)) {
    n_sym2 = sample(1:3, 1)
    chosen2 = sample(sym_names, n_sym2)
    atoms2 = lapply(chosen2, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl2 = as.CnfClause(Reduce(`|`, atoms2))
    if (!isTRUE(unclass(cl2))) clauses[[length(clauses) + 1]] = cl2
  }

  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ident-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ident-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Identical clauses: %d tests, %d failures\n", n_tests, n_failures))

# === Clauses that are subsets/supersets of each other ===
cat("\n=== Subset/superset clauses ===\n")
set.seed(264004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create a clause and a wider version of it (the narrow one subsumes the wide one)
  n_sym = sample(2:3, 1)
  chosen = sample(sym_names, n_sym)
  narrow_atoms = lapply(chosen, function(s) {
    r = sample(dom, sample(1:2, 1))
    list(sym = s, range = r)
  })

  # Narrow clause
  cl_narrow = as.CnfClause(Reduce(`|`, lapply(narrow_atoms, function(a) syms[[a$sym]] %among% a$range)))

  # Wide clause: same symbols, wider or equal ranges
  wide_atoms = lapply(narrow_atoms, function(a) {
    extra = sample(setdiff(dom, a$range), min(1, length(setdiff(dom, a$range))))
    syms[[a$sym]] %among% c(a$range, extra)
  })
  cl_wide = as.CnfClause(Reduce(`|`, wide_atoms))

  if (isTRUE(unclass(cl_narrow)) || isTRUE(unclass(cl_wide))) next

  clauses = list(cl_narrow, cl_wide)

  # More random clauses
  for (j in 1:sample(1:3, 1)) {
    n_sym2 = sample(1:3, 1)
    chosen2 = sample(sym_names, n_sym2)
    atoms2 = lapply(chosen2, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl2 = as.CnfClause(Reduce(`|`, atoms2))
    if (!isTRUE(unclass(cl2))) clauses[[length(clauses) + 1]] = cl2
  }

  clauses = clauses[sample(length(clauses))]
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [subsup-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [subsup-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Subset/superset clauses: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
