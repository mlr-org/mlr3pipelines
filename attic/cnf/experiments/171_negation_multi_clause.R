#!/usr/bin/env Rscript
# Test negation of multi-clause formulas:
# - !.CnfFormula does De Morgan: !(C1 & C2 & ...) = !C1 | !C2 | ...
# - Each !Ci creates a formula (from atoms); then Reduce(|, ...) distributes
# - This can create exponentially many clauses
# - Test that semantic correctness is maintained despite possible simplification losses
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Negation of 2-clause formulas ===
cat("=== Negation of 2-clause formulas ===\n")
set.seed(171001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create a 2-clause formula
  clauses = lapply(1:2, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f) || is.logical(unclass(f))) next

  tf = evaluate_formula(f, u)

  n_tests = n_tests + 1
  nf = tryCatch(!f, error = function(e) e)
  if (inherits(nf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neg2-%d]: %s\n", trial, nf$message)); next
  }
  tnf = evaluate_formula(nf, u)
  if (!all(tnf == !tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [neg2-%d]: !f != !truth(f)\n", trial))
  }

  # Double negation
  n_tests = n_tests + 1
  nnf = tryCatch(!!f, error = function(e) e)
  if (inherits(nnf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dblneg-%d]: %s\n", trial, nnf$message)); next
  }
  tnnf = evaluate_formula(nnf, u)
  if (!all(tnnf == tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dblneg-%d]: !!f != f\n", trial))
  }
}
cat(sprintf("  2-clause negation: %d tests, %d failures\n", n_tests, n_failures))

# === Negation of 3-clause formulas ===
cat("\n=== Negation of 3-clause formulas ===\n")
set.seed(171002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  clauses = lapply(1:3, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f) || is.logical(unclass(f))) next
  tf = evaluate_formula(f, u)

  n_tests = n_tests + 1
  nf = tryCatch(!f, error = function(e) e)
  if (inherits(nf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neg3-%d]: %s\n", trial, nf$message)); next
  }
  tnf = evaluate_formula(nf, u)
  if (!all(tnf == !tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [neg3-%d]: !f != !truth(f)\n", trial))
  }
}
cat(sprintf("  3-clause negation: %d tests, %d failures\n", n_tests, n_failures))

# === Negation of unit formulas ===
cat("\n=== Negation of unit formulas ===\n")
set.seed(171003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Formula with units and multi-symbol clauses
  n_units = sample(1:2, 1)
  clauses = lapply(1:n_units, function(j) {
    s = sample(names(syms), 1)
    as.CnfClause(syms[[s]] %among% sample(dom, sample(1:3, 1)))
  })
  for (j in 1:sample(1:2, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f) || is.logical(unclass(f))) next
  tf = evaluate_formula(f, u)

  n_tests = n_tests + 1
  nf = tryCatch(!f, error = function(e) e)
  if (inherits(nf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [negunit-%d]: %s\n", trial, nf$message)); next
  }
  tnf = evaluate_formula(nf, u)
  if (!all(tnf == !tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [negunit-%d]: !f != !truth(f)\n", trial))
  }
}
cat(sprintf("  Negation of unit formulas: %d tests, %d failures\n", n_tests, n_failures))

# === De Morgan: !(f1 & f2) == !f1 | !f2 ===
cat("\n=== De Morgan verification ===\n")
set.seed(171004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  # !(f1 & f2) should equal !f1 | !f2
  n_tests = n_tests + 1
  lhs = tryCatch(!(f1 & f2), error = function(e) NULL)
  rhs = tryCatch(!f1 | !f2, error = function(e) NULL)
  if (is.null(lhs) || is.null(rhs)) next
  tlhs = evaluate_formula(lhs, u)
  trhs = evaluate_formula(rhs, u)
  expected = !(t1 & t2)
  if (!all(tlhs == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan-lhs-%d]\n", trial))
  }
  if (!all(trhs == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan-rhs-%d]\n", trial))
  }
  if (!all(tlhs == trhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan-eq-%d]\n", trial))
  }

  # !(f1 | f2) should equal !f1 & !f2
  n_tests = n_tests + 1
  lhs2 = tryCatch(!(f1 | f2), error = function(e) NULL)
  rhs2 = tryCatch(!f1 & !f2, error = function(e) NULL)
  if (is.null(lhs2) || is.null(rhs2)) next
  tlhs2 = evaluate_formula(lhs2, u)
  trhs2 = evaluate_formula(rhs2, u)
  expected2 = !(t1 | t2)
  if (!all(tlhs2 == expected2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan2-lhs-%d]\n", trial))
  }
  if (!all(trhs2 == expected2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan2-rhs-%d]\n", trial))
  }
}
cat(sprintf("  De Morgan: %d tests, %d failures\n", n_tests, n_failures))

# === Complement: f & !f == FALSE, f | !f == TRUE ===
cat("\n=== Complement laws ===\n")
set.seed(171005)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  clauses = lapply(1:sample(1:3, 1), function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next
  nf = tryCatch(!f, error = function(e) NULL)
  if (is.null(nf)) next

  # f & !f should be FALSE
  n_tests = n_tests + 1
  r = tryCatch(f & nf, error = function(e) e)
  if (inherits(r, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [comp-and-%d]: %s\n", trial, r$message)); next
  }
  tr = evaluate_formula(r, u)
  if (any(tr)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [comp-and-%d]: f & !f not FALSE\n", trial))
  }

  # f | !f should be TRUE
  n_tests = n_tests + 1
  r2 = tryCatch(f | nf, error = function(e) e)
  if (inherits(r2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [comp-or-%d]: %s\n", trial, r2$message)); next
  }
  tr2 = evaluate_formula(r2, u)
  if (!all(tr2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [comp-or-%d]: f | !f not TRUE\n", trial))
  }
}
cat(sprintf("  Complement laws: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
