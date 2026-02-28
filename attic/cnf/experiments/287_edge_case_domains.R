#!/usr/bin/env Rscript
# Edge case domain sizes:
# - Domain size 1: everything is trivially determined
# - Domain size 2 with many vars: SAT-like problems
# - Very large domains (15+) with few vars
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Domain size 1 ===
cat("=== Domain size 1 ===\n")
set.seed(287001)

for (trial in 1:100) {
  u = CnfUniverse()
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, "x")

  # With domain size 1, every atom is either TRUE (val = "x") or we can't create FALSE atoms
  # Actually, CnfAtom with the only value should be TRUE
  # Let's check: V1 %among% "x" should be TRUE
  atom = syms[["V1"]] %among% "x"
  n_tests = n_tests + 1
  if (!isTRUE(unclass(as.CnfClause(atom)))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dom1-%d]: single-value atom not TRUE\n", trial))
  }
}
cat(sprintf("  Domain size 1: %d tests, %d failures\n", n_tests, n_failures))

# === Domain size 2, SAT-like ===
cat("\n=== Domain size 2 SAT-like ===\n")
set.seed(287002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("0", "1")
  sym_names = paste0("V", 1:8)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Each clause has 2-3 "literals" (single value atoms)
  n_cl = sample(4:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sat-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sat-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Domain size 2 SAT-like: %d tests, %d failures\n", n_tests, n_failures))

# === Large domain ===
cat("\n=== Large domain (15) ===\n")
set.seed(287003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = paste0("v", 1:15)
  V1 = CnfSymbol(u, "V1", dom)
  V2 = CnfSymbol(u, "V2", dom)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    if (n_sym == 1) {
      sym = sample(c("V1", "V2"), 1)
      r = sample(dom, sample(1:5, 1))
      if (sym == "V1") as.CnfClause(V1 %among% r) else as.CnfClause(V2 %among% r)
    } else {
      r1 = sample(dom, sample(1:5, 1))
      r2 = sample(dom, sample(1:5, 1))
      as.CnfClause(V1 %among% r1 | V2 %among% r2)
    }
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ldom-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ldom-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Large domain (15): %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
