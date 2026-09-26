#!/usr/bin/env Rscript
# Regression resilience testing:
# Tests that exercise specific code paths identified from code analysis:
# 1. apply_domain_restriction when is_unit_propagation=FALSE (SSE path)
# 2. eliminate_symbol_from_clause reverse direction updates
# 3. on_updated_subset_relations cascading
# 4. The second_order_enabled_matrix transition
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

# === Pattern 1: SSE that triggers apply_domain_restriction (non-unit path) ===
cat("=== SSE apply_domain_restriction ===\n")
set.seed(227001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Pairs where SSE will restrict a symbol (not unit propagation)
  a_vals = sample(dom, sample(2:3, 1))
  b_vals = sample(dom, sample(1:3, 1))
  c_vals = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a_vals | B %among% b_vals),
    as.CnfClause(A %among% a_vals | B %among% unique(c(b_vals, sample(dom, 1))) | C %among% c_vals)
  )

  # More clauses
  for (j in 1:sample(4:8, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
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
    cat(sprintf("ERROR [sseadr-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sseadr-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  SSE apply_domain_restriction: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Symbol elimination reverse direction stress ===
cat("\n=== Symbol elimination reverse direction ===\n")
set.seed(227002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create clauses where unit propagation will eliminate symbols,
  # requiring reverse-direction updates in is_not_subset_of
  unit_sym = sample(sym_names, 1)
  unit_val = sample(dom, 1)
  non_unit_val = setdiff(dom, unit_val)

  clauses = list(
    as.CnfClause(syms[[unit_sym]] %among% unit_val)  # unit
  )

  # Clauses that reference the unit symbol with conflicting ranges
  # (will have the symbol eliminated)
  for (j in 1:sample(4:7, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(sym_names, n_sym)
    # Include unit symbol sometimes
    if (!unit_sym %in% chosen && runif(1) < 0.6) {
      chosen[sample(length(chosen), 1)] = unit_sym
    }
    atoms = lapply(chosen, function(s) {
      if (s == unit_sym) {
        # Use range that includes unit_val (won't be eliminated) or excludes it (will be)
        if (runif(1) < 0.5) {
          syms[[s]] %among% sample(dom, sample(1:3, 1))
        } else {
          syms[[s]] %among% sample(non_unit_val, sample(1:length(non_unit_val), 1))
        }
      } else {
        syms[[s]] %among% sample(dom, sample(1:3, 1))
      }
    })
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [symrev-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [symrev-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Symbol elim reverse: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Cascading on_updated_subset_relations ===
cat("\n=== Cascading subset relation updates ===\n")
set.seed(227003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:6)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create chains of clauses where SSE in one pair cascades to another
  clauses = list()
  for (j in 1:sample(6:12, 1)) {
    n_sym = sample(2:3, 1)
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
    cat(sprintf("ERROR [cascade-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cascade-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Cascading subset: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Second-order trigger loop stress ===
cat("\n=== Second-order trigger loop ===\n")
set.seed(227004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Many pairs with not_subset_count==2 to trigger the second-order loop
  clauses = list()
  for (j in 1:sample(8:15, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 6) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [2ndtrig-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [2ndtrig-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2nd-order trigger: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
