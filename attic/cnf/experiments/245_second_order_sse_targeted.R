#!/usr/bin/env Rscript
# 2nd-order SSE targeted testing:
# The 2nd-order self-subsumption elimination checks for disjointness
# between pairs of clauses on a shared symbol. Test various patterns
# that could trigger or nearly trigger this.
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

# === Pattern 1: Disjoint ranges on intersect symbol ===
# Two clauses A and B are both non-subsets of target C on a shared symbol X.
# If A.X and B.X are disjoint outside C.X, then 2nd-order SSE can restrict C.Y.
cat("=== Disjoint ranges triggering 2nd order SSE ===\n")
set.seed(245001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  clauses = list()

  # Target clause: V1 in r1, V2 in r2
  r1 = sample(dom, sample(2:3, 1))
  r2 = sample(dom, sample(2:3, 1))
  clauses[[1]] = as.CnfClause(syms[["V1"]] %among% r1 | syms[["V2"]] %among% r2)

  # Clause A: V1 in disjoint_part1, V2 in ra2 -- subset of target on V1, not on V2
  # Actually for 2nd order: A is non-subset of target on V1, and B is also non-subset on V1
  r1_comp = setdiff(dom, r1)
  if (length(r1_comp) >= 2) {
    split = sample(1:(length(r1_comp)-1), 1)
    a_r1 = c(sample(r1, sample(0:min(1, length(r1)), 1)), r1_comp[1:split])
    b_r1 = c(sample(r1, sample(0:min(1, length(r1)), 1)), r1_comp[(split+1):length(r1_comp)])
    a_r2 = sample(dom, sample(1:3, 1))
    b_r2 = sample(dom, sample(1:3, 1))
    clauses[[2]] = as.CnfClause(syms[["V1"]] %among% a_r1 | syms[["V2"]] %among% a_r2)
    clauses[[3]] = as.CnfClause(syms[["V1"]] %among% b_r1 | syms[["V2"]] %among% b_r2)
  } else {
    clauses[[2]] = as.CnfClause(syms[["V1"]] %among% sample(dom, sample(1:3, 1)) | syms[["V2"]] %among% sample(dom, sample(1:3, 1)))
    clauses[[3]] = as.CnfClause(syms[["V1"]] %among% sample(dom, sample(1:3, 1)) | syms[["V2"]] %among% sample(dom, sample(1:3, 1)))
  }

  # More random clauses
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [disjoint-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [disjoint-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Disjoint 2nd order: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Twoend + oneend interaction ===
cat("\n=== Twoend + oneend interaction ===\n")
set.seed(245002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create clauses with specific not_subset_count patterns
  # Target: 3 symbols
  target_syms = sample(sym_names, 3)
  target_atoms = lapply(target_syms, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
  clauses = list(as.CnfClause(Reduce(`|`, target_atoms)))

  # Twoend: shares 2 of 3 target symbols, non-subset on 2
  two_syms = sample(target_syms, 2)
  extra_sym = sample(setdiff(sym_names, target_syms), 1)
  twoend_atoms = c(
    lapply(two_syms, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1))),
    list(syms[[extra_sym]] %among% sample(dom, sample(1:2, 1)))
  )
  clauses[[2]] = as.CnfClause(Reduce(`|`, twoend_atoms))

  # Oneend: shares 2 of 3 target symbols, non-subset on 1
  one_syms = sample(target_syms, 2)
  oneend_atoms = lapply(one_syms, function(s) {
    target_range = unclass(clauses[[1]])[[s]]
    if (is.null(target_range) || runif(1) > 0.5) {
      syms[[s]] %among% sample(dom, sample(1:2, 1))
    } else {
      syms[[s]] %among% sample(target_range, sample(1:length(target_range), 1))
    }
  })
  clauses[[3]] = as.CnfClause(Reduce(`|`, oneend_atoms))

  # More random clauses
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [twoone-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [twoone-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Twoend+oneend: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Many clauses with not_subset_count exactly 2 ===
cat("\n=== Many twoend clauses ===\n")
set.seed(245003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create many 2-symbol clauses (high chance of not_subset_count = 2 between pairs)
  n_cl = sample(6:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(sym_names, 2)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [manytwo-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [manytwo-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Many twoend: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: 2nd order SSE with resolution-like patterns ===
cat("\n=== Resolution-like 2nd order ===\n")
set.seed(245004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Resolution-like: A|X and B|~X can resolve to A|B
  # In our setting: clause1 = {V1 in r1, V2 in r2_part1}
  #                  clause2 = {V1 in r1, V2 in r2_part2}
  # where r2_part1 and r2_part2 are disjoint and cover r2_full
  # Target = {V1 in ..., V2 in r2_full}

  v1_range = sample(dom, sample(1:2, 1))
  v2_parts = sample(dom, sample(2:3, 1))
  if (length(v2_parts) >= 2) {
    split = sample(1:(length(v2_parts)-1), 1)
    v2_a = v2_parts[1:split]
    v2_b = v2_parts[(split+1):length(v2_parts)]
  } else {
    v2_a = v2_parts[1]
    v2_b = sample(setdiff(dom, v2_parts), 1)
  }

  clauses = list(
    as.CnfClause(syms[["V1"]] %among% v1_range | syms[["V2"]] %among% v2_a),
    as.CnfClause(syms[["V1"]] %among% v1_range | syms[["V2"]] %among% v2_b),
    as.CnfClause(syms[["V1"]] %among% sample(dom, sample(2:3, 1)) | syms[["V2"]] %among% c(v2_a, v2_b))
  )

  # More random clauses
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [reslike-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [reslike-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Resolution-like: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
