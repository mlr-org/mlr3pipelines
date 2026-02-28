#!/usr/bin/env Rscript
# Test patterns where unit propagation during the pairwise comparison phase
# causes cascading effects. This targets the meta_idx > meta_idx_outer
# path in eliminate_symbol_from_clause (line 254).
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

# === SSE creates unit during pairwise, cascades to later clauses ===
cat("=== SSE -> unit cascade during pairwise ===\n")
set.seed(89001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create clauses designed to trigger SSE -> new unit -> cascading propagation
  # Clause 1 (2 syms): V1 %among% {a,b} | V2 %among% {a}
  # Clause 2 (2 syms): V1 %among% {a}   | V2 %among% {a,b}
  # SSE should narrow one clause's symbol, possibly creating a unit
  # That unit then propagates to later clauses in the pairwise loop

  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(2:3, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [sse-cascade-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("sse-cascade-%d", trial))
}
cat(sprintf("  SSE cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Specifically constructed cascading patterns ===
cat("\n=== Constructed cascading patterns ===\n")
set.seed(89002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  V1 = CnfSymbol(u, "V1", dom)
  V2 = CnfSymbol(u, "V2", dom)
  V3 = CnfSymbol(u, "V3", dom)
  V4 = CnfSymbol(u, "V4", dom)

  # Chain: SSE on clause pair (1,2) creates V1 unit -> propagates to clause 3 -> might create V3 unit -> ...
  v1_vals = sample(dom, 2)
  v2_vals = sample(dom, 2)
  v3_vals = sample(dom, 2)

  clauses = list(
    # Pair that triggers SSE on V1
    as.CnfClause(V1 %among% v1_vals[1] | V2 %among% sample(dom, 1)),
    as.CnfClause(V1 %among% v1_vals    | V2 %among% sample(dom, 1)),
    # Clause affected by V1 propagation
    as.CnfClause(V1 %among% v1_vals[2] | V3 %among% v3_vals),
    # Clause affected by potential V3 unit
    as.CnfClause(V3 %among% sample(dom, 1) | V4 %among% sample(dom, 2))
  )
  # Add some random clauses
  for (i in 1:sample(0:3, 1)) {
    chosen = sample(c("V1", "V2", "V3", "V4"), sample(2:3, 1))
    atoms = lapply(chosen, function(s) get(s) %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [construct-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("construct-%d", trial))
}
cat(sprintf("  Constructed cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Subsumption cascade during pairwise ===
cat("\n=== Subsumption cascade during pairwise ===\n")
set.seed(89003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:5) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create a mix of short and long clauses to trigger subsumption during pairwise
  clauses = list()
  # Some short clauses (2 symbols) that might subsume longer ones
  for (i in 1:sample(2:4, 1)) {
    chosen = sample(names(syms), 2)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  # Some longer clauses (3-4 symbols) that might be subsumed
  for (i in 1:sample(2:5, 1)) {
    chosen = sample(names(syms), sample(3:4, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [sub-cascade-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("sub-cascade-%d", trial))
}
cat(sprintf("  Subsumption cascade: %d tests, %d failures\n", n_tests, n_failures))

# === Contradiction cascade during pairwise ===
cat("\n=== Contradiction cascade during pairwise ===\n")
set.seed(89004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create clauses that when simplified, produce contradicting units
  # This tests the TRUE (contradiction) return path
  v1 = sample(dom, 1)
  v1_comp = setdiff(dom, v1)

  clauses = list(
    # Two clauses that SSE down to contradicting units
    as.CnfClause(syms$V1 %among% v1 | syms$V2 %among% sample(dom, 1)),
    as.CnfClause(syms$V1 %among% sample(v1_comp, 1) | syms$V2 %among% sample(dom, 1)),
    # Maybe these will cascade with other clauses
    as.CnfClause(syms$V2 %among% sample(dom, 1) | syms$V3 %among% sample(dom, 2)),
    as.CnfClause(syms$V3 %among% sample(dom, 1) | syms$V4 %among% sample(dom, 2))
  )

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [contra-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("contra-%d", trial))
}
cat(sprintf("  Contradiction cascade: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
