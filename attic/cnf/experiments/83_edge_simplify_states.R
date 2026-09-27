#!/usr/bin/env Rscript
# Test specific edge states within the simplifier:
# - All clauses eliminated during unit phase
# - Clause becomes unit during pairwise phase
# - SSE during pairwise phase
# - HLA when no candidates exist
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

# === All non-units subsumed by units ===
cat("=== All non-units subsumed by units ===\n")
set.seed(83001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  unit_a = sample(dom, 1)
  unit_b = sample(dom, 1)

  clauses = list(
    as.CnfClause(A %among% unit_a),
    as.CnfClause(B %among% unit_b),
    # This clause contains unit_a and unit_b, so it's subsumed
    as.CnfClause(A %among% c(unit_a, sample(setdiff(dom, unit_a), 1)) |
                 B %among% c(unit_b, sample(setdiff(dom, unit_b), 1)))
  )

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [subsumed-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("subsumed-%d", trial))
}
cat(sprintf("  All subsumed by units: %d tests, %d failures\n", n_tests, n_failures))

# === SSE creates new unit during pairwise ===
cat("\n=== SSE creates unit during pairwise ===\n")
set.seed(83002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create clauses where SSE should reduce one to a unit
  # Clause 1: V1 %among% {a, b} | V2 %among% {a}
  # Clause 2: V1 %among% {a} | V2 %among% {a, b}
  # SSE should narrow V2 in clause 1 from {a} to intersect with clause 2's V2 range
  # If it reduces to a single symbol, it becomes a unit

  n_cl = sample(3:6, 1)
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
    cat(sprintf("ERROR [sse-unit-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("sse-unit-%d", trial))
}
cat(sprintf("  SSE creates unit: %d tests, %d failures\n", n_tests, n_failures))

# === Formulas where HLA eliminates all candidates ===
cat("\n=== HLA with all candidates used ===\n")
set.seed(83003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create wide clauses (many symbols) to trigger HLA
  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(2:4, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:3, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [hla-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("hla-%d", trial))
}
cat(sprintf("  HLA all candidates: %d tests, %d failures\n", n_tests, n_failures))

# === Asymmetric domains ===
cat("\n=== Asymmetric domains ===\n")
set.seed(83004)

for (trial in 1:300) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a"))  # singleton domain
  B = CnfSymbol(u, "B", c("x", "y", "z", "w", "v"))  # large domain
  C = CnfSymbol(u, "C", c("1", "2"))  # binary

  syms = list(A = A, B = B, C = C)
  doms = list(A = c("a"), B = c("x", "y", "z", "w", "v"), C = c("1", "2"))

  n_cl = sample(3:6, 1)
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
    cat(sprintf("ERROR [asym-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("asym-%d", trial))
}
cat(sprintf("  Asymmetric domains: %d tests, %d failures\n", n_tests, n_failures))

# === Formulas with only 2 non-units (minimal pairwise) ===
cat("\n=== Minimal pairwise (2 non-units) ===\n")
set.seed(83005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # 1-2 units + exactly 2 non-units
  clauses = list()
  for (i in 1:sample(0:2, 1)) {
    s = sample(names(syms), 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, 1))
  }
  for (i in 1:2) {
    chosen = sample(names(syms), sample(2:3, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [min-pair-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("min-pair-%d", trial))
}
cat(sprintf("  Minimal pairwise: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
