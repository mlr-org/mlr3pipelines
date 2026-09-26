source("experiments/test_harness.R")

# Targeted test: 2nd-order SSE where oneend clause does NOT have symbol_target
# This is a specific edge case in try_sse_2nd_order where clause_oneend[[symbol_target]] is NULL
# char_union(NULL, twoend_range) = twoend_range

n_failures = 0
n_tests = 0

check_fc = function(formula, raw_clauses, universe, label) {
  n_tests <<- n_tests + 1
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in raw_clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }

  simplified_truth = evaluate_formula(formula, universe)
  mismatches = which(raw_truth != simplified_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d (raw=%s, simp=%s)\n",
      label, idx, raw_truth[idx], simplified_truth[idx]))
    cat(sprintf("  Assignment: %s\n",
      paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
    cat(sprintf("  Clauses (%d):\n", length(raw_clauses)))
    for (ci in seq_along(raw_clauses)) {
      cl = unclass(raw_clauses[[ci]])
      if (is.logical(cl)) {
        cat(sprintf("    %d: %s\n", ci, cl))
      } else {
        cat(sprintf("    %d: %s\n", ci, paste(names(cl), "=", sapply(cl, paste, collapse = ","), collapse = " | ")))
      }
    }
    return(FALSE)
  }
  TRUE
}

cat("=== Case 1: Oneend missing symbol_target, ternary symbol_intersect ===\n")
# Setup: A has 3 values, B has 3 values, C has 3 values
# Target: A in {a1} | B in {b1} | C in {c1}  (3 symbols)
# Oneend: A in {a2} | B in {b1}  (subset on B, NOT subset on A) -- no C!
# Twoend: A in {a3} | C in {c2}  (NOT subset on A and C)
# Oneend and twoend are disjoint on A outside target's A range: {a2} ∩ {a3} = empty

for (trial in 1:50) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2", "b3"))
  C = CnfSymbol(u, "C", c("c1", "c2", "c3"))

  # Vary the exact values while maintaining the pattern
  a_target = sample(c("a1", "a2", "a3"), 1)
  a_rest = setdiff(c("a1", "a2", "a3"), a_target)
  b_target = sample(c("b1", "b2", "b3"), sample(1:2, 1))
  c_target = sample(c("c1", "c2", "c3"), sample(1:2, 1))

  # oneend: A in a_rest[1] | B in b_target (subset of target on B, not on A)
  # -- doesn't have C!
  cl_oneend = as.CnfClause(A %among% a_rest[1] | B %among% b_target)

  # twoend: A in a_rest[2] | C in {something not in c_target}
  c_twoend = sample(setdiff(c("c1", "c2", "c3"), c_target), sample(1:max(1, length(setdiff(c("c1", "c2", "c3"), c_target))), 1))
  if (length(c_twoend) == 0) c_twoend = sample(c("c1", "c2", "c3"), 1)  # fallback
  cl_twoend = as.CnfClause(A %among% a_rest[2] | C %among% c_twoend)

  # target: A in a_target | B in b_target | C in c_target
  cl_target = as.CnfClause(A %among% a_target | B %among% b_target | C %among% c_target)

  all_clauses = list(cl_target, cl_oneend, cl_twoend)
  f = tryCatch(CnfFormula(all_clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [case1 trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, all_clauses, u, sprintf("case1-%d", trial))
}

cat(sprintf("  Case 1 done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Case 2: Multiple missing symbols, 4 variables ===\n")
# More complex: oneend has only 2 of 4 variables

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2"))
  C = CnfSymbol(u, "C", c("c1", "c2"))
  D = CnfSymbol(u, "D", c("d1", "d2"))

  syms = list(A = A, B = B, C = C, D = D)
  sym_names = names(syms)

  # Target: has 3-4 symbols
  n_target_syms = sample(3:4, 1)
  target_syms = sample(sym_names, n_target_syms)
  target_atoms = lapply(target_syms, function(s) {
    dom = u[[s]]
    syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
  })
  cl_target = as.CnfClause(Reduce(`|`, target_atoms))

  # Oneend: has 2 symbols, subset of target on all except sym_intersect
  oneend_syms = sample(target_syms, 2)
  sym_intersect = oneend_syms[1]
  # Make oneend NOT subset of target on sym_intersect
  target_bare = unclass(cl_target)
  target_sym_intersect_range = target_bare[[sym_intersect]]
  oneend_intersect_range = sample(setdiff(u[[sym_intersect]], target_sym_intersect_range),
    min(sample(1:2, 1), length(setdiff(u[[sym_intersect]], target_sym_intersect_range))))
  if (length(oneend_intersect_range) == 0) next  # can't make non-subset

  # Make oneend subset of target on the other symbol
  other_sym = oneend_syms[2]
  other_range_target = target_bare[[other_sym]]
  if (is.null(other_range_target)) next
  oneend_other_range = sample(other_range_target, sample(1:length(other_range_target), 1))

  cl_oneend = as.CnfClause(syms[[sym_intersect]] %among% oneend_intersect_range | syms[[other_sym]] %among% oneend_other_range)

  # Twoend: has sym_intersect + one more target symbol that oneend doesn't have
  sym_target_candidates = setdiff(target_syms, c(sym_intersect, other_sym))
  if (length(sym_target_candidates) == 0) next
  sym_target = sample(sym_target_candidates, 1)

  # twoend NOT subset of target on sym_intersect AND sym_target
  twoend_intersect_range = sample(setdiff(u[[sym_intersect]], target_sym_intersect_range),
    min(sample(1:2, 1), length(setdiff(u[[sym_intersect]], target_sym_intersect_range))))
  if (length(twoend_intersect_range) == 0) next

  target_sym_target_range = target_bare[[sym_target]]
  twoend_target_range = sample(setdiff(u[[sym_target]], target_sym_target_range),
    min(sample(1:2, 1), length(setdiff(u[[sym_target]], target_sym_target_range))))
  if (length(twoend_target_range) == 0) next

  cl_twoend = as.CnfClause(syms[[sym_intersect]] %among% twoend_intersect_range | syms[[sym_target]] %among% twoend_target_range)

  all_clauses = list(cl_target, cl_oneend, cl_twoend)

  # Add some noise clauses
  n_noise = sample(0:2, 1)
  for (i in seq_len(n_noise)) {
    chosen = sample(sym_names, sample(1:3, 1))
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    all_clauses[[length(all_clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(all_clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [case2 trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, all_clauses, u, sprintf("case2-%d", trial))
}
cat(sprintf("  Case 2 done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Case 3: Brute force many patterns triggering 2nd order SSE ===\n")
# Create many formulas with asymmetric clause structures that could trigger 2nd order SSE

set.seed(999)
for (trial in 1:500) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2"))
  C = CnfSymbol(u, "C", c("c1", "c2"))

  syms = list(A = A, B = B, C = C)

  # Generate clauses where some have 1 symbol, some have 2, some have 3
  n_clauses = sample(3:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_syms = sample(1:3, 1)
    chosen = sample(names(syms), n_syms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [case3 trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("case3-%d", trial))
}
cat(sprintf("  Case 3 done: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
