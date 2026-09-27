#!/usr/bin/env Rscript
# Test formulas that should simplify to TRUE after simplification.
# These exercise the return_entries(entries[!eliminated]) path where all entries are eliminated.
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

# === Subsumption: one clause subsumes another ===
cat("=== Subsumption eliminates clause ===\n")
set.seed(84001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # Clause 1 subsumes clause 2: clause 1's ranges are subsets of clause 2's
  v1a = sample(dom, 1)
  v1b = sample(dom, 1)
  clauses = list(
    as.CnfClause(A %among% v1a | B %among% v1b),
    as.CnfClause(A %among% c(v1a, sample(setdiff(dom, v1a), 1)) |
                 B %among% c(v1b, sample(setdiff(dom, v1b), 1)))
  )

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [subsume-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("subsume-%d", trial))
}
cat(sprintf("  Subsumption: %d tests, %d failures\n", n_tests, n_failures))

# === Tautological formulas: clauses that cover entire domain ===
cat("\n=== Tautological formulas ===\n")
set.seed(84002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  # A single clause covering full domain for one symbol
  clauses = list(
    as.CnfClause(A %among% dom | B %among% sample(dom, 1))
  )

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [taut-%d]: %s\n", trial, f$message)); next
  }
  # Formula should be TRUE
  if (!isTRUE(as.logical(f))) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("FAIL [taut-%d]: expected TRUE formula\n", trial)); next
  }
  check_fc(f, clauses, u, sprintf("taut-%d", trial))
}
cat(sprintf("  Tautological: %d tests, %d failures\n", n_tests, n_failures))

# === Unit + clause where unit makes clause tautological ===
cat("\n=== Unit makes clause tautological ===\n")
set.seed(84003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  unit_val = sample(dom, 1)

  # Unit: A %among% {val}
  # Clause: A %among% {val} | B %among% {something}
  # After unit propagation, A's range in the clause is intersected with {val},
  # which is {val}. But that doesn't make it tautological.
  # For tautology: need A %among% complement(val) | B %among% dom
  # After unit propagation: A restricted to val, complement(val) intersect val = empty,
  # B already covers full domain -> depends

  # Better: create situations where unit propagation removes symbols until clause becomes tautological
  # Unit: A %among% {val}
  # Clause: A %among% comp(val) | B %among% dom  (B covers full domain)
  # After unit prop: A intersect with val -> A range = comp(val) intersect val = empty -> A eliminated
  # Only B remains, covering full domain -> tautological -> clause eliminated

  comp_a = setdiff(dom, unit_val)
  clauses = list(
    as.CnfClause(A %among% unit_val),
    as.CnfClause(A %among% comp_a | B %among% dom)
  )

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [unit-taut-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("unit-taut-%d", trial))
}
cat(sprintf("  Unit makes tautological: %d tests, %d failures\n", n_tests, n_failures))

# === SSE reduces to subsumable ===
cat("\n=== SSE reduces to subsumable ===\n")
set.seed(84004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(3:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:3, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [sse-sub-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("sse-sub-%d", trial))

  # Also check: if f is TRUE, verify the clauses really do simplify to TRUE
  if (isTRUE(as.logical(f))) {
    n_tests = n_tests + 1
    truth = evaluate_formula(f, u)
    if (!all(truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [sse-sub-true-check-%d]: TRUE formula but not all TRUE\n", trial))
    }
  }
}
cat(sprintf("  SSE reduces: %d tests, %d failures\n", n_tests, n_failures))

# === HLA eliminates all remaining non-units ===
cat("\n=== HLA eliminates all non-units ===\n")
set.seed(84005)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(2:3, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:3, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [hla-all-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("hla-all-%d", trial))
}
cat(sprintf("  HLA eliminates: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
