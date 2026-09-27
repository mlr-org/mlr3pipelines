#!/usr/bin/env Rscript
# Test scenarios specifically designed to trigger cascading eliminations.
# When one unit propagation creates another unit, which creates another, etc.
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

# === Deep unit chains ===
# A=a -> B has only 1 value left -> C has only 1 value left -> ...
cat("=== Deep unit chains ===\n")
set.seed(68001)

for (trial in 1:200) {
  u = CnfUniverse()
  chain_len = sample(3:6, 1)
  syms = list()
  dom_size = sample(2:3, 1)
  dom = paste0("d", 1:dom_size)
  for (v in 1:chain_len) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  clauses = list()
  # Unit for V1
  clauses[[1]] = as.CnfClause(syms[["V1"]] %among% sample(dom, 1))
  # For each consecutive pair: (Vi %among% complement_val | Vi+1 %among% one_val)
  # When Vi gets restricted, this becomes a unit for Vi+1
  for (i in 1:(chain_len - 1)) {
    v_curr = paste0("V", i)
    v_next = paste0("V", i + 1)
    # clause: (Vi %among% {all except unit_val}) | (Vi+1 %among% {one_val})
    vi_vals = sample(dom, dom_size - 1)  # leave one out
    vi1_val = sample(dom, 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(
      syms[[v_curr]] %among% vi_vals | syms[[v_next]] %among% vi1_val
    )
  }

  # Add some random clauses for noise
  n_extra = sample(0:3, 1)
  for (i in seq_len(n_extra)) {
    chosen = sample(names(syms), sample(1:min(3, chain_len), 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:max(1, dom_size - 1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("chain-%d", trial))
}
cat(sprintf("  Deep unit chains: %d tests, %d failures\n", n_tests, n_failures))

# === Contradicting unit chains ===
# Similar to above, but the chain leads to a contradiction
cat("\n=== Contradicting chains ===\n")
set.seed(68002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  clauses = list()
  # Force A = "a"
  clauses[[1]] = as.CnfClause(A %among% "a")
  # A not "a" -> B = "b" (i.e. A %among% c("b","c") | B %among% "b")
  clauses[[2]] = as.CnfClause(A %among% sample(dom, sample(1:2, 1)) | B %among% sample(dom, sample(1:2, 1)))
  # B restricted -> C forced
  clauses[[3]] = as.CnfClause(B %among% sample(dom, sample(1:2, 1)) | C %among% sample(dom, sample(1:2, 1)))
  # Add conflicting clause sometimes
  if (sample(c(TRUE, FALSE), 1)) {
    clauses[[4]] = as.CnfClause(C %among% sample(dom, sample(1:2, 1)))
  }
  # Add more random clauses
  for (i in 1:sample(1:3, 1)) {
    s = sample(list(A, B, C), sample(1:2, 1))
    atoms = lapply(s, function(sym) sym %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [contra-chain-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("contra-chain-%d", trial))
}
cat(sprintf("  Contradicting chains: %d tests, %d failures\n", n_tests, n_failures))

# === Multiple simultaneous units ===
cat("\n=== Multiple simultaneous units ===\n")
set.seed(68003)

for (trial in 1:300) {
  u = CnfUniverse()
  n_vars = sample(3:5, 1)
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  clauses = list()
  # Create 2-3 units
  n_units = sample(2:min(3, n_vars), 1)
  unit_vars = sample(names(syms), n_units)
  for (uv in unit_vars) {
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[uv]] %among% sample(dom, 1))
  }
  # Add non-unit clauses that mention unit vars
  for (i in 1:sample(3:6, 1)) {
    chosen = sample(names(syms), sample(2:min(3, n_vars), 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:2, 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [multi-unit-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("multi-unit-%d", trial))
}
cat(sprintf("  Multiple simultaneous units: %d tests, %d failures\n", n_tests, n_failures))

# === Unit creates unit creates subsumption ===
cat("\n=== Unit-cascade-subsumption ===\n")
set.seed(68004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  clauses = list()
  # Unit: V1 = "a"
  clauses[[1]] = as.CnfClause(syms[["V1"]] %among% "a")
  # Clause: V1 not in {a} | V2 in {b} --> becomes unit V2 = "b"
  clauses[[2]] = as.CnfClause(syms[["V1"]] %among% sample(dom, sample(1:3, 1)) |
                               syms[["V2"]] %among% sample(dom, sample(1:3, 1)))
  # Clause that should be subsumed after V2 gets restricted
  clauses[[3]] = as.CnfClause(syms[["V2"]] %among% sample(dom, sample(2:3, 1)) |
                               syms[["V3"]] %among% sample(dom, sample(1:3, 1)))
  # More clauses
  for (i in 1:sample(2:4, 1)) {
    chosen = sample(names(syms), sample(2:3, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(dom, sample(1:3, 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [cascade-sub-%d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("cascade-sub-%d", trial))
}
cat(sprintf("  Unit-cascade-subsumption: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
