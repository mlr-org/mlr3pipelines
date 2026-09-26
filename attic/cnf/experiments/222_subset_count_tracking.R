#!/usr/bin/env Rscript
# Tests for not_subset_count tracking correctness:
# After simplification, verify that structural properties hold.
# Also tests formulas where not_subset_count transitions from 3->2->1->0
# through cascading operations.
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

# === Pattern 1: Many 3-symbol clauses with shared symbols (high subset count transitions) ===
cat("=== 3-symbol shared clauses ===\n")
set.seed(222001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:5)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # All clauses have exactly 3 symbols, sharing many
  n_cl = sample(6:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(sym_names, 3)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 5) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [3sym-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3sym-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  3-symbol shared: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Clauses with gradually overlapping ranges ===
cat("\n=== Gradually overlapping ranges ===\n")
set.seed(222002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create clauses with increasing overlap
  base_a = sample(dom, 2)
  base_b = sample(dom, 2)
  base_c = sample(dom, 2)

  clauses = list()
  for (j in 1:sample(5:8, 1)) {
    # Each clause uses ranges that overlap with base by varying amounts
    a_shift = sample(0:2, 1)  # how many values to change
    b_shift = sample(0:2, 1)
    c_shift = sample(0:2, 1)

    a_range = if (a_shift == 0) base_a else sample(dom, length(base_a))
    b_range = if (b_shift == 0) base_b else sample(dom, length(base_b))
    c_range = if (c_shift == 0) base_c else sample(dom, length(base_c))

    n_sym = sample(2:3, 1)
    if (n_sym == 2) {
      which_2 = sample(1:3, 2)
      ranges = list(a_range, b_range, c_range)[which_2]
      chosen = names(syms)[which_2]
    } else {
      ranges = list(a_range, b_range, c_range)
      chosen = names(syms)
    }
    atoms = mapply(function(s, r) syms[[s]] %among% r, chosen, ranges, SIMPLIFY = FALSE)
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [overlap-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [overlap-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Gradually overlapping: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Structural verification after simplification ===
cat("\n=== Post-simplification structural checks ===\n")
set.seed(222003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  n_vars = sample(3:5, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(4:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:min(4, n_vars), 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [struct-%d]: %s\n", trial, result$message)); next
  }

  # Semantic check
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [struct-%d]: semantic mismatch\n", trial)); next
  }

  # Structural checks on output
  f_bare = unclass(result)
  if (is.logical(f_bare)) next  # TRUE or FALSE, nothing structural to check

  for (i in seq_along(f_bare)) {
    cl = f_bare[[i]]
    # No empty ranges
    for (sym in names(cl)) {
      if (length(cl[[sym]]) == 0) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [struct-%d]: empty range in clause %d symbol %s\n", trial, i, sym)); break
      }
      # No tautological symbols (range == full domain)
      if (length(cl[[sym]]) == length(get(sym, u))) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [struct-%d]: tautological symbol in clause %d symbol %s\n", trial, i, sym)); break
      }
    }

    # No subsumption between any pair
    for (j in seq_along(f_bare)) {
      if (i == j) next
      cl_j = f_bare[[j]]
      # Check if cl_i subsumes cl_j (all ranges of cl_i are subsets of cl_j)
      all_subset = TRUE
      for (sym in names(cl)) {
        if (!sym %in% names(cl_j) || !all(cl[[sym]] %in% cl_j[[sym]])) {
          all_subset = FALSE
          break
        }
      }
      if (all_subset) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [struct-%d]: clause %d subsumes clause %d\n", trial, i, j))
      }
    }
  }
}
cat(sprintf("  Structural checks: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Idempotence - simplifying again should give same result ===
cat("\n=== Idempotence verification ===\n")
set.seed(222004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  n_vars = sample(3:5, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:min(4, n_vars), 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1

  # Re-simplify: CnfFormula(as.list(f))
  f2 = tryCatch({
    cl_list = as.list(f)
    if (length(cl_list) == 0) {
      # TRUE formula
      as.CnfFormula(TRUE)
    } else {
      CnfFormula(cl_list)
    }
  }, error = function(e) e)

  if (inherits(f2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [idemp-%d]: %s\n", trial, f2$message)); next
  }

  t1 = evaluate_formula(f, u)
  t2 = evaluate_formula(f2, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [idemp-%d]: idempotence mismatch\n", trial))
  }
}
cat(sprintf("  Idempotence: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
