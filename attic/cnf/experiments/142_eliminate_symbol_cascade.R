#!/usr/bin/env Rscript
# Test eliminate_symbol_from_clause cascading:
# - When SSE causes a clause to lose a symbol, creating a unit
# - When that unit propagates and causes further symbol eliminations
# - When cascading reaches meta_idx > meta_idx_outer (early return path)
# Also test the specific condition at line 277 where not_subset_count
# check happens after cascading.
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

# === SSE eliminates symbol, creating unit that cascades ===
cat("=== SSE -> unit -> cascade ===\n")
set.seed(142001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  # Clause 1: A|B (short, processed first in pairwise)
  # Clause 2: A|B|C (longer, where SSE might shrink B or eliminate it)
  # Clause 3: B|C|D (would be affected by unit propagation from B)
  # Clause 4: C|D (would be affected by cascading)
  a1 = sample(dom, sample(1:3, 1))
  b1 = sample(dom, sample(1:3, 1))
  a2 = sample(dom, sample(1:3, 1))
  b2 = sample(dom, sample(1:3, 1))
  c2 = sample(dom, sample(1:3, 1))
  b3 = sample(dom, sample(1:3, 1))
  c3 = sample(dom, sample(1:3, 1))
  d3 = sample(dom, sample(1:3, 1))
  c4 = sample(dom, sample(1:3, 1))
  d4 = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a2 | B %among% b2 | C %among% c2),
    as.CnfClause(B %among% b3 | C %among% c3 | D %among% d3),
    as.CnfClause(C %among% c4 | D %among% d4)
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

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
cat(sprintf("  SSE->unit->cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Unit propagation creates unit that cascades through later clauses ===
cat("\n=== Unit creates unit in later clause ===\n")
set.seed(142002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Unit A restricts clause 2 (A|B) which may become unit B
  # Unit B then restricts clause 3 (B|C) etc.
  a_unit = sample(dom, 1)
  a2 = sample(dom, 1)  # might conflict with a_unit
  b2 = sample(dom, sample(1:2, 1))
  b3 = sample(dom, sample(1:2, 1))
  c3 = sample(dom, sample(1:2, 1))

  clauses = list(
    as.CnfClause(A %among% a_unit),
    as.CnfClause(A %among% a2 | B %among% b2),
    as.CnfClause(B %among% b3 | C %among% c3),
    as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | C %among% sample(dom, sample(1:2, 1)))
  )
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [unit-chain-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [unit-chain-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Unit creates unit: %d tests, %d failures\n", n_tests, n_failures))

# === Dense clauses with overlapping symbols ===
cat("\n=== Dense symbol overlap ===\n")
set.seed(142003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(3:5, 1)
  syms = list()
  for (i in 1:n_vars) {
    vname = paste0("X", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # All clauses share at least 2 symbols (dense overlap)
  n_cl = sample(5:12, 1)
  base_syms = sample(names(syms), 2)
  clauses = lapply(1:n_cl, function(j) {
    chosen = unique(c(base_syms, sample(names(syms), sample(0:min(2, n_vars - 2), 1))))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dense-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dense-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Dense overlap: %d tests, %d failures\n", n_tests, n_failures))

# === Many short clauses (lots of pairwise comparisons) ===
cat("\n=== Many short clauses ===\n")
set.seed(142004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Many 1-2 symbol clauses
  n_cl = sample(15:40, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(c("A", "B", "C"), n_sym)
    syms = list(A = A, B = B, C = C)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 10) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [short-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [short-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Many short: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
