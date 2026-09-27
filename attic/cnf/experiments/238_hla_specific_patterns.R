#!/usr/bin/env Rscript
# HLA (Hidden Literal Addition) specific patterns:
# Test scenarios designed to trigger hidden tautology and hidden subsumption
# elimination in both the non-unit and unit HLA phases.
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

# === Pattern 1: Clauses designed to trigger hidden subsumption ===
# If clause D is almost a subset of clause C (except 1 symbol), HLA expands C.
# If after expansion C subsumes another clause E, E is eliminated via hidden subsumption.
cat("=== HLA hidden subsumption triggers ===\n")
set.seed(238001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom_x = c("a", "b", "c", "d")
  dom_y = c("p", "q", "r", "s")
  X = CnfSymbol(u, "X", dom_x)
  Y = CnfSymbol(u, "Y", dom_y)
  Z = CnfSymbol(u, "Z", c("1", "2", "3"))

  syms = list(X = X, Y = Y, Z = Z)
  all_doms = list(X = dom_x, Y = dom_y, Z = c("1", "2", "3"))

  # Create clauses where HLA could discover hidden subsumption
  # C: {X in subset, Y in subset} -- the clause to be potentially eliminated
  x_range_c = sample(dom_x, sample(2:3, 1))
  y_range_c = sample(dom_y, sample(2:3, 1))

  clauses = list()
  clauses[[1]] = as.CnfClause(X %among% x_range_c | Y %among% y_range_c)

  # D: {X in subset_of_x_range_c, Y in something_else} -- HLA helper
  x_range_d = sample(x_range_c, sample(1:min(2, length(x_range_c)), 1))
  y_range_d = sample(dom_y, sample(1:3, 1))
  clauses[[2]] = as.CnfClause(X %among% x_range_d | Y %among% y_range_d)

  # More random clauses
  for (j in 1:sample(3:6, 1)) {
    sym_names = names(syms)
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(all_doms[[s]], sample(1:max(1, length(all_doms[[s]])-1), 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [hs-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hs-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA hidden subsumption: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Unit HLA hidden subsumption ===
# Units go through a separate HLA loop. Test that path.
cat("\n=== Unit HLA hidden subsumption ===\n")
set.seed(238002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create a unit clause
  unit_sym = sample(sym_names, 1)
  unit_range = sample(dom, sample(1:3, 1))
  clauses = list(as.CnfClause(syms[[unit_sym]] %among% unit_range))

  # Create clauses that are "almost subsets" of the unit
  # (i.e., they share the unit symbol but have one extra symbol where they're not subsets)
  for (k in 1:sample(2:4, 1)) {
    other_syms = setdiff(sym_names, unit_sym)
    n_sym = sample(1:min(3, length(other_syms)), 1)
    chosen = sample(other_syms, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    # Include the unit symbol with a subset of its range
    unit_sub = sample(unit_range, sample(1:length(unit_range), 1))
    atoms = c(list(syms[[unit_sym]] %among% unit_sub), atoms)
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # More random clauses
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [uhla-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [uhla-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Unit HLA: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Multi-step HLA chain ===
# Create scenarios where multiple HLA steps chain together
cat("\n=== Multi-step HLA chains ===\n")
set.seed(238003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Target clause with many symbols
  n_target = sample(3:5, 1)
  target_syms = sample(sym_names, n_target)
  target_atoms = lapply(target_syms, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  clauses = list(as.CnfClause(Reduce(`|`, target_atoms)))

  # Create multiple "almost subset" clauses - each one could contribute to HLA
  for (k in 1:sample(3:6, 1)) {
    # Choose a subset of target symbols and create a clause that's subset on all but one
    n_sym = sample(2:min(4, n_target), 1)
    chosen = sample(target_syms, n_sym)
    atoms = lapply(chosen, function(s) {
      target_range = unclass(clauses[[1]])[[s]]
      if (is.null(target_range)) {
        syms[[s]] %among% sample(dom, sample(1:3, 1))
      } else {
        # Sometimes subset, sometimes not
        if (runif(1) > 0.3) {
          syms[[s]] %among% sample(target_range, sample(1:length(target_range), 1))
        } else {
          syms[[s]] %among% sample(dom, sample(1:3, 1))
        }
      }
    })
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # More random clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mhla-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mhla-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multi-step HLA: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: HLA with clauses that share many symbols ===
# High symbol overlap increases chances of subset relationships
cat("\n=== HLA high symbol overlap ===\n")
set.seed(238004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # All clauses use all 3 symbols (maximum overlap)
  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    atoms = lapply(sym_names, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [hso-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hso-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA high overlap: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
