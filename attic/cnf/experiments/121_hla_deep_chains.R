#!/usr/bin/env Rscript
# Deep HLA chain testing:
# - Constructs patterns where HLA can propagate through many steps
# - Tests hidden tautology elimination with large formulas
# - Tests hidden subsumption with cascading HLA
# - Verifies HLA unit phase interacts correctly with non-unit phase
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

# === Constructed HLA tautology patterns ===
cat("=== Constructed HLA tautology ===\n")
set.seed(121001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  # Create clauses where HLA can derive a tautology:
  # Clause 1: A in {a,b} | B in {c,d}
  # Clause 2: A in {c,d} | C in {a}
  # Clause 3: B in {a,b} | C in {b,c,d}
  # HLA on clause 3 using clause 1: B covers {a,b}, complement is {c,d}, add to clause 3's B
  # clause 3 becomes: B in {a,b,c,d} | C in {b,c,d} = tautology

  a_split1 = sample(dom, 2)
  a_split2 = setdiff(dom, a_split1)
  b_split1 = sample(dom, 2)
  b_split2 = setdiff(dom, b_split1)

  clauses = list(
    as.CnfClause(A %among% a_split1 | B %among% b_split2),
    as.CnfClause(A %among% a_split2 | C %among% sample(dom, sample(1:3, 1))),
    as.CnfClause(B %among% b_split1 | C %among% sample(dom, sample(1:3, 1)))
  )
  # Add some extra random clauses
  n_extra = sample(0:3, 1)
  for (j in seq_len(n_extra)) {
    syms = list(A = A, B = B, C = C)
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [hla-taut-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [hla-taut-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA tautology: %d tests, %d failures\n", n_tests, n_failures))

# === HLA subsumption chains ===
cat("\n=== HLA subsumption chains ===\n")
set.seed(121002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2", "3")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)

  syms = list(A = A, B = B, C = C, D = D)

  # Generate formulas that are likely to trigger HLA chains
  n_clauses = sample(4:10, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  HLA chains: %d tests, %d failures\n", n_tests, n_failures))

# === Units + non-units interleaved (exercises HLA unit phase) ===
cat("\n=== Units + non-units HLA ===\n")
set.seed(121003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  syms = list(A = A, B = B, C = C)

  # Mix units and multi-symbol clauses
  clauses = list()
  # Add 1-2 units
  for (j in 1:sample(1:2, 1)) {
    s = sample(names(syms), 1)
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
  }
  # Add 3-6 non-units
  for (j in 1:sample(3:6, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mix-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mix-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Units+non-units HLA: %d tests, %d failures\n", n_tests, n_failures))

# === 5 vars ternary, heavy clause load (HLA stress) ===
cat("\n=== 5v ternary HLA stress ===\n")
set.seed(121004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  syms = list()
  for (i in 1:5) {
    vname = paste0("V", i)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Many clauses with 2-3 symbols each
  n_clauses = sample(8:15, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [5v-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [5v-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  5v ternary: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
