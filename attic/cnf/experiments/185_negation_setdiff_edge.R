#!/usr/bin/env Rscript
# Test negation (!) edge cases focusing on setdiff in the negation operator:
# - Large domain negation (complement of large clause)
# - Negation of negation of complex formulas
# - Negation where domain values are substrings of each other
# - Negation of formulas with many clauses (deep Reduce path)
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: Negation with large domains ===
cat("=== Large domain negation ===\n")
set.seed(185001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = paste0("v", 1:10)
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:8, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  nf = tryCatch(!f, error = function(e) e)
  if (inherits(nf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neg1-%d]: %s\n", trial, nf$message)); next
  }

  tf = evaluate_formula(f, u)
  tnf = evaluate_formula(nf, u)
  if (!all(tnf == !tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [neg1-%d]: !f != complement of f\n", trial))
  }
}
cat(sprintf("  Large domain neg: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Double negation with complex formulas ===
cat("\n=== Double negation complex ===\n")
set.seed(185002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  nnf = tryCatch(!!f, error = function(e) e)
  if (inherits(nnf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dblneg-%d]: %s\n", trial, nnf$message)); next
  }

  tf = evaluate_formula(f, u)
  tnnf = evaluate_formula(nnf, u)
  if (!all(tnnf == tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dblneg-%d]: !!f != f\n", trial))
  }
}
cat(sprintf("  Double negation: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Negation with confusing domain values ===
cat("\n=== Confusing value negation ===\n")
set.seed(185003)

for (trial in 1:300) {
  u = CnfUniverse()
  # Values that could cause issues: substrings, empty-like, etc.
  dom = c("ab", "a", "abc", "b", "bc")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  nf = tryCatch(!f, error = function(e) e)
  if (inherits(nf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [confval-%d]: %s\n", trial, nf$message)); next
  }

  tf = evaluate_formula(f, u)
  tnf = evaluate_formula(nf, u)
  if (!all(tnf == !tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [confval-%d]: negation incorrect\n", trial))
  }
}
cat(sprintf("  Confusing values: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Negation of many-clause formulas (deep Reduce) ===
cat("\n=== Many-clause negation ===\n")
set.seed(185004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  nf = tryCatch(!f, error = function(e) e)
  if (inherits(nf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [manyneg-%d]: %s\n", trial, nf$message)); next
  }

  tf = evaluate_formula(f, u)
  tnf = evaluate_formula(nf, u)
  if (any(tf & tnf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [manyneg-%d]: f and !f overlap\n", trial))
    next
  }
  if (any(!tf & !tnf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [manyneg-%d]: f and !f don't cover everything\n", trial))
  }
}
cat(sprintf("  Many-clause neg: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 5: De Morgan with 3-4 clause formulas ===
cat("\n=== De Morgan 3-4 clause ===\n")
set.seed(185005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(2:4, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(); g = make_f()
  if (is.null(f) || is.null(g)) next

  # De Morgan: !(f & g) == !f | !g
  n_tests = n_tests + 1
  fg = tryCatch(f & g, error = function(e) NULL)
  if (is.null(fg)) next
  lhs = tryCatch(!fg, error = function(e) NULL)
  nf = tryCatch(!f, error = function(e) NULL)
  ng = tryCatch(!g, error = function(e) NULL)
  if (is.null(lhs) || is.null(nf) || is.null(ng)) next
  rhs = tryCatch(nf | ng, error = function(e) NULL)
  if (is.null(rhs)) next

  tlhs = evaluate_formula(lhs, u)
  trhs = evaluate_formula(rhs, u)
  if (!all(tlhs == trhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan2-%d]: !(f&g) != !f|!g\n", trial))
  }
}
cat(sprintf("  De Morgan 3-4cl: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
