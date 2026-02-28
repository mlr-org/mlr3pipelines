#!/usr/bin/env Rscript
# Test Hidden Literal Addition (HLA) specifically:
# - Constructed patterns where HLA can find hidden tautologies
# - Constructed patterns where HLA can find hidden subsumptions
# - Unit HLA with delayedAssign roe_inverse path
# - Many clauses to increase HLA iteration depth
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

# === Constructed hidden tautology: from the docs ===
cat("=== Constructed HLA patterns ===\n")
set.seed(161001)

u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("d", "e", "f"))
Z = CnfSymbol(u, "Z", c("g", "h", "i"))

# From the docs: this is a hidden tautology
clauses = list(
  as.CnfClause(X %among% c("a", "b") | Y %among% c("d", "e")),
  as.CnfClause(X %among% "a" | Z %among% c("g", "h")),
  as.CnfClause(X %among% "b" | Z %among% c("h", "i")),
  as.CnfClause(Y %among% c("d", "e") | Z %among% c("g", "i"))
)

n_tests = n_tests + 1
f = CnfFormula(clauses)
truth = evaluate_formula(f, u)
raw_truth = evaluate_raw_clauses(clauses, u)
if (!all(truth == raw_truth)) {
  n_failures = n_failures + 1
  cat("FAIL: docs hidden tautology example\n")
}
# Check that the formula has fewer clauses (the first clause should be eliminated)
n_clauses_out = if (isTRUE(unclass(f))) 0 else length(unclass(f))
cat(sprintf("  Docs example: %d clauses in, %d clauses out\n", length(clauses), n_clauses_out))

# === Random patterns that should trigger HLA ===
cat("\n=== Random HLA patterns ===\n")
set.seed(161002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create clauses where HLA can work:
  # Mix of 2-symbol and 3-symbol clauses with partial overlaps
  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    # Use overlapping ranges to encourage subset relationships
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
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
cat(sprintf("  Random HLA: %d tests, %d failures\n", n_tests, n_failures))

# === Unit HLA (roe_inverse path) ===
cat("\n=== Unit HLA ===\n")
set.seed(161003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create formulas with units mixed with multi-symbol clauses
  # This exercises the unit HLA phase
  n_units = sample(1:2, 1)
  n_nonunits = sample(3:6, 1)

  clauses = list()
  for (j in 1:n_units) {
    sym = sample(names(syms), 1)
    clauses[[j]] = as.CnfClause(syms[[sym]] %among% sample(dom, sample(2:3, 1)))
  }
  for (j in 1:n_nonunits) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[n_units + j]] = as.CnfClause(Reduce(`|`, atoms))
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

# === Deep HLA: many clauses to force multiple HLA iterations ===
cat("\n=== Deep HLA ===\n")
set.seed(161004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (i in 1:4) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Many clauses (10-20) to encourage deep HLA chains
  n_cl = sample(10:20, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 8) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [deep-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [deep-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Deep HLA: %d tests, %d failures\n", n_tests, n_failures))

# === HLA where hidden literal addition changes future pair relations ===
cat("\n=== HLA cascade ===\n")
set.seed(161005)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(5:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [hlac-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hlac-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA cascade: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
