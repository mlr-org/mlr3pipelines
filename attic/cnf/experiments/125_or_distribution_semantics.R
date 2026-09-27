#!/usr/bin/env Rscript
# Test |.CnfFormula distribution semantics more deeply:
# - Verify that (f1 | f2) == distribute(f1, f2) by checking truth tables
# - Test with formulas of different sizes (1-clause | 1-clause up to 5 | 5)
# - Verify commutativity: f1 | f2 == f2 | f1
# - Distribution followed by AND: (f1 | f2) & f3
# - Chained OR: f1 | f2 | f3
# - OR with TRUE/FALSE formulas
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Single clause | single clause ===
cat("=== 1-clause | 1-clause ===\n")
set.seed(125001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  syms = list(A = A, B = B, C = C)

  # Create two single-clause formulas
  make_clause = function() {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  }
  cl1 = make_clause()
  cl2 = make_clause()
  if (isTRUE(unclass(cl1)) || isTRUE(unclass(cl2))) next

  f1 = CnfFormula(list(cl1))
  f2 = CnfFormula(list(cl2))

  n_tests = n_tests + 1
  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [1|1-%d]: %s\n", trial, result$message)); next
  }

  # Evaluate: f1 | f2 should be semantically cl1 | cl2
  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  expected = t_f1 | t_f2
  truth = evaluate_formula(result, u)
  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [1|1-%d]: semantic mismatch\n", trial))
  }

  # Commutativity
  n_tests = n_tests + 1
  result2 = tryCatch(f2 | f1, error = function(e) e)
  if (inherits(result2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [comm-%d]: %s\n", trial, result2$message)); next
  }
  truth2 = evaluate_formula(result2, u)
  if (!all(truth == truth2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [comm-%d]: f1|f2 != f2|f1\n", trial))
  }
}
cat(sprintf("  1|1: %d tests, %d failures\n", n_tests, n_failures))

# === Multi-clause | multi-clause ===
cat("\n=== Multi-clause | multi-clause ===\n")
set.seed(125002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  syms = list(A = A, B = B)

  make_formula = function(n_cl) {
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    CnfFormula(clauses)
  }

  n1 = sample(2:4, 1)
  n2 = sample(2:4, 1)
  f1 = make_formula(n1)
  f2 = make_formula(n2)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [multi-%d]: %s\n", trial, result$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  expected = t_f1 | t_f2
  truth = evaluate_formula(result, u)
  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [multi-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Multi|Multi: %d tests, %d failures\n", n_tests, n_failures))

# === Chained OR: f1 | f2 | f3 ===
cat("\n=== Chained OR ===\n")
set.seed(125003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  syms = list(A = A, B = B, C = C)

  make_small_formula = function() {
    n_cl = sample(1:2, 1)
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    CnfFormula(clauses)
  }

  f1 = make_small_formula()
  f2 = make_small_formula()
  f3 = make_small_formula()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  n_tests = n_tests + 1
  # (f1 | f2) | f3
  result_lr = tryCatch((f1 | f2) | f3, error = function(e) e)
  if (inherits(result_lr, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-lr-%d]: %s\n", trial, result_lr$message)); next
  }

  # f1 | (f2 | f3)
  n_tests = n_tests + 1
  result_rl = tryCatch(f1 | (f2 | f3), error = function(e) e)
  if (inherits(result_rl, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-rl-%d]: %s\n", trial, result_rl$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  t_f3 = evaluate_formula(f3, u)
  expected = t_f1 | t_f2 | t_f3

  truth_lr = evaluate_formula(result_lr, u)
  truth_rl = evaluate_formula(result_rl, u)
  if (!all(truth_lr == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-lr-%d]: (f1|f2)|f3 semantic error\n", trial))
  }
  if (!all(truth_rl == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-rl-%d]: f1|(f2|f3) semantic error\n", trial))
  }
  if (!all(truth_lr == truth_rl)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [assoc-%d]: associativity violated\n", trial))
  }
}
cat(sprintf("  Chained OR: %d tests, %d failures\n", n_tests, n_failures))

# === OR then AND: (f1 | f2) & f3 ===
cat("\n=== OR then AND ===\n")
set.seed(125004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  syms = list(A = A, B = B, C = C)

  make_small_formula = function() {
    n_cl = sample(1:2, 1)
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    CnfFormula(clauses)
  }

  f1 = make_small_formula()
  f2 = make_small_formula()
  f3 = make_small_formula()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  n_tests = n_tests + 1
  result = tryCatch((f1 | f2) & f3, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [or-and-%d]: %s\n", trial, result$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  t_f3 = evaluate_formula(f3, u)
  expected = (t_f1 | t_f2) & t_f3
  truth = evaluate_formula(result, u)
  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [or-and-%d]: semantic error\n", trial))
  }

  # Also test AND then OR: f1 & (f2 | f3)
  n_tests = n_tests + 1
  result2 = tryCatch(f1 & (f2 | f3), error = function(e) e)
  if (inherits(result2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [and-or-%d]: %s\n", trial, result2$message)); next
  }
  expected2 = t_f1 & (t_f2 | t_f3)
  truth2 = evaluate_formula(result2, u)
  if (!all(truth2 == expected2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [and-or-%d]: semantic error\n", trial))
  }
}
cat(sprintf("  OR then AND: %d tests, %d failures\n", n_tests, n_failures))

# === OR with TRUE/FALSE ===
cat("\n=== OR with TRUE/FALSE ===\n")
set.seed(125005)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  f = CnfFormula(list(as.CnfClause(A %among% sample(dom, 1) | B %among% sample(dom, 1))))
  t_f = evaluate_formula(f, u)

  # f | TRUE = TRUE
  n_tests = n_tests + 1
  r1 = tryCatch(f | as.CnfFormula(TRUE), error = function(e) e)
  if (inherits(r1, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [or-true-%d]: %s\n", trial, r1$message)); next
  }
  if (!all(evaluate_formula(r1, u))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [or-true-%d]: f | TRUE != TRUE\n", trial))
  }

  # f | FALSE = f
  n_tests = n_tests + 1
  r2 = tryCatch(f | as.CnfFormula(FALSE), error = function(e) e)
  if (inherits(r2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [or-false-%d]: %s\n", trial, r2$message)); next
  }
  truth2 = evaluate_formula(r2, u)
  if (!all(truth2 == t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [or-false-%d]: f | FALSE != f\n", trial))
  }

  # TRUE | f = TRUE
  n_tests = n_tests + 1
  r3 = tryCatch(as.CnfFormula(TRUE) | f, error = function(e) e)
  if (inherits(r3, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [true-or-%d]: %s\n", trial, r3$message)); next
  }
  if (!all(evaluate_formula(r3, u))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [true-or-%d]: TRUE | f != TRUE\n", trial))
  }

  # FALSE | f = f
  n_tests = n_tests + 1
  r4 = tryCatch(as.CnfFormula(FALSE) | f, error = function(e) e)
  if (inherits(r4, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [false-or-%d]: %s\n", trial, r4$message)); next
  }
  truth4 = evaluate_formula(r4, u)
  if (!all(truth4 == t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [false-or-%d]: FALSE | f != f\n", trial))
  }
}
cat(sprintf("  OR TRUE/FALSE: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
