#!/usr/bin/env Rscript
# Test with highly mixed domain sizes:
# - Some vars with domain size 1 (singleton)
# - Some vars with domain size 2 (binary)
# - Some vars with very large domains (5-8)
# This stresses the subsumption/SSE logic with asymmetric ranges.
source("experiments/test_harness.R")

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
    return(FALSE)
  }
  TRUE
}

# === Config 1: singleton + binary + large ===
cat("=== Singleton + binary + large domains ===\n")
set.seed(90001)

for (trial in 1:500) {
  u = CnfUniverse()
  # Singleton
  S = CnfSymbol(u, "S", c("only"))
  # Binary
  B = CnfSymbol(u, "B", c("T", "F"))
  # Large
  large_dom = paste0("v", 1:6)
  L = CnfSymbol(u, "L", large_dom)

  syms = list(S = S, B = B, L = L)
  doms = list(S = c("only"), B = c("T", "F"), L = large_dom)

  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:3, 1))
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:max(1, length(d) - 1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [sbl-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("sbl-%d", trial))
}
cat(sprintf("  Singleton+binary+large: %d tests, %d failures\n", n_tests, n_failures))

# === Config 2: multiple singletons (should not affect formula) ===
cat("\n=== Multiple singletons ===\n")
set.seed(90002)

for (trial in 1:300) {
  u = CnfUniverse()
  S1 = CnfSymbol(u, "S1", c("x"))
  S2 = CnfSymbol(u, "S2", c("y"))
  A = CnfSymbol(u, "A", c("a", "b", "c"))
  B = CnfSymbol(u, "B", c("1", "2", "3"))

  syms = list(S1 = S1, S2 = S2, A = A, B = B)
  doms = list(S1 = c("x"), S2 = c("y"), A = c("a", "b", "c"), B = c("1", "2", "3"))

  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:3, 1))
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:max(1, length(d) - 1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [multi-sing-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("multi-sing-%d", trial))
}
cat(sprintf("  Multiple singletons: %d tests, %d failures\n", n_tests, n_failures))

# === Config 3: 2 binary + 2 large (8 values) ===
cat("\n=== Binary + large(8) ===\n")
set.seed(90003)

for (trial in 1:300) {
  u = CnfUniverse()
  B1 = CnfSymbol(u, "B1", c("0", "1"))
  B2 = CnfSymbol(u, "B2", c("0", "1"))
  large_dom = paste0("d", 1:8)
  L1 = CnfSymbol(u, "L1", large_dom)
  L2 = CnfSymbol(u, "L2", large_dom)

  syms = list(B1 = B1, B2 = B2, L1 = L1, L2 = L2)
  doms = list(B1 = c("0", "1"), B2 = c("0", "1"), L1 = large_dom, L2 = large_dom)

  n_cl = sample(3:7, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:3, 1))
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:max(1, length(d) - 2), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [bl8-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("bl8-%d", trial))
}
cat(sprintf("  Binary+large(8): %d tests, %d failures\n", n_tests, n_failures))

# === Config 4: all different domain sizes (1,2,3,4,5) ===
cat("\n=== All different domain sizes ===\n")
set.seed(90004)

for (trial in 1:300) {
  u = CnfUniverse()
  V1 = CnfSymbol(u, "V1", c("a"))  # 1
  V2 = CnfSymbol(u, "V2", c("a", "b"))  # 2
  V3 = CnfSymbol(u, "V3", c("a", "b", "c"))  # 3
  V4 = CnfSymbol(u, "V4", c("a", "b", "c", "d"))  # 4
  V5 = CnfSymbol(u, "V5", c("a", "b", "c", "d", "e"))  # 5

  syms = list(V1 = V1, V2 = V2, V3 = V3, V4 = V4, V5 = V5)
  doms = list(
    V1 = c("a"), V2 = c("a", "b"), V3 = c("a", "b", "c"),
    V4 = c("a", "b", "c", "d"), V5 = c("a", "b", "c", "d", "e")
  )

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(2:4, 1))
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:max(1, length(d) - 1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [allsizes-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("allsizes-%d", trial))
}
cat(sprintf("  All different sizes: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
