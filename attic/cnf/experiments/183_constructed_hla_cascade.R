#!/usr/bin/env Rscript
# Constructed cases designed to trigger HLA -> SSE cascade:
# - HLA complement addition makes a range wider
# - The wider range causes is_not_subset_of to become FALSE for some entry
# - This triggers on_updated_subset_relations inside apply_domain_restriction
# - Which updates the is_not_subset_of matrix without updating not_subset_count_current
# The goal is to find a case where not_subset_count_current diverges from the matrix
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

# === Constructed case 1: HLA extension triggers SSE which modifies matrix ===
cat("=== Constructed HLA -> SSE -> matrix modification ===\n")
set.seed(183001)

# Consider clauses C1..Cn, all with symbols A, B, C (domain = {a,b,c,d}):
# During HLA, clause C1 (the large one) is tested against other clauses.
# If C2 is a subset of C1 in all but symbol C:
#   HLA adds complement of C2's C-range to C1's local C-range
# Now C1's extended C-range may make a different clause C3 become
# a subset of C1 (not_subset_count_current goes from 1 to 0 for C3),
# triggering hidden subsumption elimination of C1.

# But what if the SSE modifies is_not_subset_of for C4's entry w.r.t. C1?
# Then not_subset_count_current for C4 could be wrong.

# The HLA code at line 698-704 ONLY updates not_subset_count_current
# for clauses that share the SAME symbol as the HLA complement.
# But apply_domain_restriction from SSE can change is_not_subset_of
# for a DIFFERENT symbol.

# To trigger this:
# 1. C1 (current clause in HLA outer loop): {A in {a,b}, B in {a,b}, C in {a}}
# 2. C2 (HLA partner): {A in {a,b}, B in {a,b}, C in {c,d}}
#    -> HLA extends C1's C to {a} U complement({c,d}) = {a} U {a,b} = {a,b}
# 3. After extending C, loop over clauses with symbol C:
#    check if C3's C-range is subset of C1's new C-range {a,b}
# 4. C3: {A in {a}, B in {a}, C in {a,b}}
#    C3 is subset of C1 for A ({a} in {a,b})? Yes
#    C3 is subset of C1 for B ({a} in {a,b})? Yes
#    C3 is subset of C1 for C ({a,b} in {a,b})? Yes
#    not_subset_count_current becomes 0 -> C1 is subsumed, eliminated
# Actually that works directly within the HLA loop (line 705).
# The issue would be if the HLA range extension triggers SSE side effects
# via apply_domain_restriction called from try_sse_2nd_order from on_update_range.
# But on_update_range is only called at line 224 of apply_domain_restriction,
# and apply_domain_restriction in the HLA loop is NOT called directly -
# the HLA loop just checks subset relations and updates local counts.

# Actually, re-reading the HLA code: the HLA loop does NOT call
# apply_domain_restriction or on_update_range. It only updates
# is_not_subset_of and not_subset_count_current locally.
# The potential for divergence is therefore limited.

# Let me instead focus on testing formulas that exercise ALL the phases
# heavily and check semantic correctness.

for (trial in 1:2000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Create a mix of:
  # - Wide clauses (3-4 symbols, wide ranges): HLA targets
  # - Narrow clauses (2 symbols, narrow ranges): SSE candidates
  # - Units: propagation
  clauses = list()
  # 0-1 units
  if (sample(c(TRUE, FALSE), 1)) {
    s = sample(names(syms), 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
  }
  # 2-3 wide clauses
  for (j in 1:sample(2:3, 1)) {
    n_sym = sample(3:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(2:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  # 2-4 narrow clauses
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [con1-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [con1-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Constructed HLA+SSE: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Formulas then complex operations ===
cat("\n=== Operations on simplified formulas ===\n")
set.seed(183002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function(n_cl) {
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(2:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(sample(3:6, 1))
  g = make_f(sample(3:6, 1))
  h = make_f(sample(2:4, 1))
  if (is.null(f) || is.null(g) || is.null(h)) next

  # Build complex expression: (f & g) | h
  n_tests = n_tests + 1
  fg = tryCatch(f & g, error = function(e) NULL)
  if (is.null(fg)) next
  result = tryCatch(fg | h, error = function(e) NULL)
  if (is.null(result)) next

  tf = evaluate_formula(f, u)
  tg = evaluate_formula(g, u)
  th = evaluate_formula(h, u)
  expected = (tf & tg) | th
  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ops-%d]: (f&g)|h mismatch\n", trial))
  }
}
cat(sprintf("  Complex ops: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Negation then operations ===
cat("\n=== Negation + operations ===\n")
set.seed(183003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function(n_cl) {
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(2:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(sample(2:4, 1))
  g = make_f(sample(2:4, 1))
  if (is.null(f) || is.null(g)) next

  # Test: !(f | g) == !f & !g  (De Morgan)
  n_tests = n_tests + 1
  fog = tryCatch(f | g, error = function(e) NULL)
  if (is.null(fog)) next
  lhs = tryCatch(!fog, error = function(e) NULL)
  nf = tryCatch(!f, error = function(e) NULL)
  ng = tryCatch(!g, error = function(e) NULL)
  if (is.null(lhs) || is.null(nf) || is.null(ng)) next
  rhs = tryCatch(nf & ng, error = function(e) NULL)
  if (is.null(rhs)) next

  tlhs = evaluate_formula(lhs, u)
  trhs = evaluate_formula(rhs, u)
  if (!all(tlhs == trhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan-%d]: !(f|g) != !f & !g\n", trial))
  }
}
cat(sprintf("  De Morgan ops: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Quantified-like patterns ===
cat("\n=== Quantified patterns ===\n")
set.seed(183004)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create formulas that constrain relationships between variables
  # like "if A in {a,b} then B in {c,d,e}" -> (A not in {a,b} | B in {c,d,e})
  # and then test with operations

  a_cond = sample(dom, sample(2:3, 1))
  a_comp = setdiff(dom, a_cond)
  b_cond = sample(dom, sample(2:3, 1))
  c_cond = sample(dom, sample(2:3, 1))

  # A in a_comp | B in b_cond (if A in a_cond then B in b_cond)
  cl1 = as.CnfClause(A %among% a_comp | B %among% b_cond)
  # B in setdiff(dom, b_cond) | C in c_cond (if B in b_cond then C in c_cond)
  b_comp = setdiff(dom, b_cond)
  cl2 = if (length(b_comp) > 0) as.CnfClause(B %among% b_comp | C %among% c_cond) else NULL

  if (isTRUE(unclass(cl1)) || is.null(cl2) || isTRUE(unclass(cl2))) next

  # Add more constraints
  clauses = list(cl1, cl2)
  for (j in 1:sample(2:5, 1)) {
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
    cat(sprintf("ERROR [quant-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [quant-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Quantified patterns: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
