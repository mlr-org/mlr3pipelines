#!/usr/bin/env Rscript
# handle_sse_2nd_order_twoend and handle_sse_2nd_order_oneend interaction:
# - twoend finds oneend candidates with specific symbol patterns
# - The match() with nomatch=0L at line 429 and sum check at line 430
# - Multiple oneend candidates for same twoend
# - twoend where both symbols loop
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

# === Pattern 1: Twoend with both symbol iterations ===
cat("=== Twoend both symbol iterations ===\n")
set.seed(208001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Twoend clause: not-subset on both A and B w.r.t. target
  # Target: (A in ta | B in tb | C in tc)
  # Twoend: (A in tw_a | B in tw_b) where tw_a ⊄ ta and tw_b ⊄ tb
  ta = sample(dom, sample(2:3, 1))
  tb = sample(dom, sample(2:3, 1))
  tc = sample(dom, sample(2:3, 1))

  # Make twoend ranges that extend beyond target
  tw_a = unique(c(sample(setdiff(dom, ta), min(1, length(setdiff(dom, ta)))),
                  sample(dom, sample(1:2, 1))))
  tw_b = unique(c(sample(setdiff(dom, tb), min(1, length(setdiff(dom, tb)))),
                  sample(dom, sample(1:2, 1))))

  # Oneend candidates for each symbol
  oe_a1 = sample(dom, sample(1:3, 1))
  oe_a2 = sample(dom, sample(1:3, 1))
  oe_b1 = sample(dom, sample(1:3, 1))
  oe_b2 = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% ta | B %among% tb | C %among% tc),
    as.CnfClause(A %among% tw_a | B %among% tw_b),
    as.CnfClause(A %among% oe_a1 | B %among% oe_b1),
    as.CnfClause(A %among% oe_a2 | B %among% oe_b2)
  )

  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
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
    cat(sprintf("ERROR [twoboth-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [twoboth-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Twoend both symbols: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Oneend with missing symbol_target ===
cat("\n=== Oneend missing symbol_target ===\n")
set.seed(208002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  # Oneend has symbol A (non-subset of target on A)
  # Twoend has symbols A and B (non-subset on both)
  # But target doesn't have B -> the check at line 390 should catch this
  # and line 413 should catch it in the twoend path

  a1 = sample(dom, sample(1:2, 1))
  b1 = sample(dom, sample(1:2, 1))
  a2 = sample(dom, sample(1:2, 1))
  b2 = sample(dom, sample(1:2, 1))

  # Target with only A and C (no B)
  target_a = sample(dom, sample(2:3, 1))
  target_c = sample(dom, sample(2:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a2 | B %among% b2),
    as.CnfClause(A %among% target_a | C %among% target_c)
  )

  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
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
    cat(sprintf("ERROR [oemiss-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [oemiss-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Oneend missing target: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Many oneend candidates compete ===
cat("\n=== Many oneend candidates ===\n")
set.seed(208003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Target clause
  ta = sample(dom, sample(2:3, 1))
  tb = sample(dom, sample(2:3, 1))
  tc = sample(dom, sample(2:3, 1))

  clauses = list(
    as.CnfClause(A %among% ta | B %among% tb | C %among% tc)
  )

  # Many 2-symbol clauses sharing A (potential oneend/twoend candidates)
  for (j in 1:sample(4:8, 1)) {
    a_range = sample(dom, sample(1:3, 1))
    other_sym = sample(c("B", "C"), 1)
    other_range = sample(dom, sample(1:3, 1))
    cl = as.CnfClause(A %among% a_range | syms[[other_sym]] %among% other_range)
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
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
    cat(sprintf("ERROR [manyoe-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [manyoe-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Many oneend candidates: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Twoend where oneend has symbol that target lacks ===
cat("\n=== Oneend has extra symbols ===\n")
set.seed(208004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  E = CnfSymbol(u, "E", dom)
  syms = list(A = A, B = B, C = C, D = D, E = E)

  # Target: (A | B)
  # Twoend: (A | B) with different ranges -> not_subset_count 2
  # Oneend candidates have A plus extra symbols (C, D, E) not in target
  ta = sample(dom, sample(2:3, 1))
  tb = sample(dom, sample(2:3, 1))

  clauses = list(
    as.CnfClause(A %among% ta | B %among% tb)
  )

  # Twoend with both A and B non-subset
  tw_a = sample(dom, sample(1:2, 1))
  tw_b = sample(dom, sample(1:2, 1))
  clauses[[length(clauses) + 1]] = as.CnfClause(A %among% tw_a | B %among% tw_b)

  # Oneends with extra symbols
  for (j in 1:sample(3:5, 1)) {
    n_extra = sample(1:3, 1)
    extra_syms = sample(c("C", "D", "E"), n_extra)
    chosen = c("A", extra_syms)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  for (j in 1:sample(2:3, 1)) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), min(n_sym, 5))
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
    cat(sprintf("ERROR [oeextra-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [oeextra-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Oneend extra symbols: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
