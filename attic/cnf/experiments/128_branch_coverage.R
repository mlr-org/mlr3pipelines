#!/usr/bin/env Rscript
# Branch coverage experiment: construct inputs that exercise specific code paths
# in simplify_cnf that might be rare in random testing:
# 1. register_unit with existing unit (intersection path)
# 2. apply_domain_restriction where range becomes empty (eliminate_symbol_from_clause)
# 3. eliminate_symbol_from_clause where clause becomes unit (new unit)
# 4. on_updated_subset_relations with rowsum=0 (subsumption)
# 5. on_updated_subset_relations with rowsum=1 (SSE)
# 6. on_updated_subset_relations with rowsum=2 (2nd-order twoend)
# 7. HLA hidden tautology
# 8. HLA hidden subsumption
# 9. Unit HLA hidden tautology
# 10. Unit HLA hidden subsumption
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

# === 1. register_unit intersection: two units for same symbol ===
cat("=== Register unit intersection ===\n")
set.seed(128001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Two units for A with overlapping but different ranges
  a1 = sample(dom, sample(2:3, 1))
  a2 = sample(dom, sample(2:3, 1))
  isct = intersect(a1, a2)

  clauses = list(
    as.CnfClause(A %among% a1),
    as.CnfClause(A %among% a2)
  )
  # Add non-unit clauses
  for (j in 1:sample(1:3, 1)) {
    clauses[[length(clauses) + 1]] = as.CnfClause(
      A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1))
    )
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ru-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ru-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Register unit isct: %d tests, %d failures\n", n_tests, n_failures))

# === 2-3. apply_domain_restriction -> eliminate_symbol -> new unit ===
cat("\n=== Domain restriction chain ===\n")
set.seed(128002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("x", "y", "z")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Unit A in {x} + clause A in {y} | B in {z}
  # apply_domain_restriction: A in {y} intersect {x} = empty
  # -> eliminate_symbol(A) -> clause becomes B in {z} (unit)
  # -> register_unit(B in {z}) -> propagates to other clauses

  unit_val = sample(dom, 1)
  other_vals = setdiff(dom, unit_val)

  clauses = list(
    as.CnfClause(A %among% unit_val),
    as.CnfClause(A %among% sample(other_vals, min(length(other_vals), sample(1:2, 1))) |
      B %among% sample(dom, sample(1:2, 1)))
  )
  # Add more clauses to exercise the cascade
  for (j in 1:sample(1:4, 1)) {
    syms = list(A = A, B = B, C = C)
    chosen = sample(names(syms), sample(2:3, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Domain chain: %d tests, %d failures\n", n_tests, n_failures))

# === 4. Subsumption in pairwise (rowsum=0) ===
cat("\n=== Pairwise subsumption ===\n")
set.seed(128003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Make clause2 subsume clause1 (clause2 is subset of clause1)
  a_base = sample(dom, sample(1:3, 1))
  b_base = sample(dom, sample(1:3, 1))
  a_ext = unique(c(a_base, sample(dom, sample(0:1, 1))))
  b_ext = unique(c(b_base, sample(dom, sample(0:1, 1))))

  clauses = list(
    as.CnfClause(A %among% a_ext | B %among% b_ext),  # wider (should be eliminated)
    as.CnfClause(A %among% a_base | B %among% b_base)  # narrower (subsumes)
  )
  # Add more random clauses
  for (j in 1:sample(0:3, 1)) {
    clauses[[length(clauses) + 1]] = as.CnfClause(
      A %among% sample(dom, sample(1:3, 1)) | B %among% sample(dom, sample(1:3, 1))
    )
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [subs-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [subs-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Pairwise subsumption: %d tests, %d failures\n", n_tests, n_failures))

# === 5. SSE in pairwise (rowsum=1) ===
cat("\n=== Pairwise SSE ===\n")
set.seed(128004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # SSE: clause1 has A in {a} | B in {a,b}
  #       clause2 has A in {a} | B in {b,c}
  # A subset is same, B is different -> SSE: B in clause2 gets intersected
  a_common = sample(dom, sample(1:2, 1))
  b1 = sample(dom, sample(1:2, 1))
  b2 = sample(dom, sample(1:2, 1))

  clauses = list(
    as.CnfClause(A %among% a_common | B %among% b1),
    as.CnfClause(A %among% a_common | B %among% b2)
  )
  for (j in 1:sample(0:2, 1)) {
    clauses[[length(clauses) + 1]] = as.CnfClause(
      A %among% sample(dom, sample(1:2, 1)) | B %among% sample(dom, sample(1:2, 1))
    )
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sse-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sse-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Pairwise SSE: %d tests, %d failures\n", n_tests, n_failures))

# === 7-8. HLA hidden tautology and hidden subsumption ===
cat("\n=== HLA hidden tautology/subsumption ===\n")
set.seed(128005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2", "3")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  syms = list(A = A, B = B, C = C)

  # For HLA hidden tautology, we need:
  # - clause C1 with A in a1 | B in b1
  # - clause C2 with A in a2
  # - HLA: C1 gets B complement from C2 added -> if B becomes full, C1 is tautological
  # For this: C2 = A in a1 (same as C1's A), so HLA adds complement of B in C2 to C1
  # But C2 doesn't have B, so its complement for B is all of B's domain
  # Actually HLA works differently - let me re-think

  # HLA: if all symbols except one in C2 are subsets of C1, add complement of remaining symbol
  # e.g. C1 = A in {0,1} | B in {0}
  #      C2 = A in {0} | B in {1}
  # A in C2 ({0}) is subset of A in C1 ({0,1})
  # So HLA adds complement of B from C2: dom \ B_C2 = {0,2,3}
  # C1's B becomes {0} union {0,2,3} = {0,1,2,3} = full domain -> hidden tautology!

  # Let's construct this
  n_clauses = sample(4:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [hla-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hla-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA paths: %d tests, %d failures\n", n_tests, n_failures))

# === 9-10. Unit HLA hidden tautology/subsumption ===
cat("\n=== Unit HLA hidden tautology/subsumption ===\n")
set.seed(128006)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  syms = list(A = A, B = B, C = C)

  # Unit HLA works with units and non-unit clauses
  # Unit: A in {a,b}
  # Non-unit: A in {a} | B in {c}
  # HLA: the non-unit's A is a subset of the unit's A
  # So we add complement of B from non-unit to unit: dom \ {c} = {a,b,d}
  # Unit's "B" doesn't exist, so it starts as full -> adding anything doesn't change it
  # Actually unit HLA works differently - the unit checks non-units

  # Let's just generate many units + non-units
  clauses = list()
  # 1-2 units
  for (j in 1:sample(1:2, 1)) {
    s = sample(names(syms), 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
  }
  # 3-6 non-units
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

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

# === Edge: clause that loses all symbols during pairwise ===
cat("\n=== Clause loses all symbols ===\n")
set.seed(128007)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b")  # binary domain: easier to empty ranges
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  syms = list(A = A, B = B, C = C)

  # Create tight clauses that can cascade to emptying
  n_clauses = sample(4:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [empty-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [empty-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Loses all symbols: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
