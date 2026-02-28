source("experiments/test_harness.R")

# Test |.CnfFormula distribution code specifically
# The distribution (A & B) | (C & D) = (A|C) & (A|D) & (B|C) & (B|D)
# can produce exponentially many clauses, and the code has optimizations.

n_failures = 0
n_tests = 0

check_equiv = function(f1, f2, universe, label) {
  n_tests <<- n_tests + 1
  truth1 = evaluate_formula(f1, universe)
  truth2 = evaluate_formula(f2, universe)
  mismatches = which(truth1 != truth2)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    varnames = ls(universe)
    domains = lapply(varnames, function(v) get(v, universe))
    names(domains) = varnames
    assignments = expand.grid(domains, stringsAsFactors = FALSE)
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d (lhs=%s, rhs=%s)\n", label, idx, truth1[idx], truth2[idx]))
    cat(sprintf("  Assignment: %s\n", paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
    return(FALSE)
  }
  TRUE
}

cat("=== OR distribution: manual truth table comparison ===\n")
u = CnfUniverse()
A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
B = CnfSymbol(u, "B", c("b1", "b2", "b3"))
C = CnfSymbol(u, "C", c("c1", "c2", "c3"))
syms = list(A = A, B = B, C = C)

set.seed(77)
for (trial in 1:500) {
  # Build two formulas and compare f1 | f2 against truth table
  f1_clauses = lapply(1:sample(1:3, 1), function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f2_clauses = lapply(1:sample(1:3, 1), function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f1 = tryCatch(CnfFormula(f1_clauses), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(f2_clauses), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  f_or = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(f_or, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [or trial %d]: %s\n", trial, f_or$message)); next
  }

  # Compare against truth table
  n_tests <<- n_tests + 1
  truth1 = evaluate_formula(f1, u)
  truth2 = evaluate_formula(f2, u)
  truth_or = evaluate_formula(f_or, u)
  expected = truth1 | truth2
  mismatches = which(truth_or != expected)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    varnames = ls(u)
    domains = lapply(varnames, function(v) get(v, u))
    names(domains) = varnames
    assignments = expand.grid(domains, stringsAsFactors = FALSE)
    idx = mismatches[1]
    cat(sprintf("FAIL [or trial %d]: row %d (got=%s, expected=%s)\n", trial, idx, truth_or[idx], expected[idx]))
    cat(sprintf("  Assignment: %s\n", paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
    cat(sprintf("  f1 clauses: %d, f2 clauses: %d\n", length(unclass(f1)), length(unclass(f2))))
  }
}
cat(sprintf("  OR distribution done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== OR with TRUE/FALSE formulas ===\n")
for (trial in 1:50) {
  f = tryCatch({
    n_clauses = sample(1:3, 1)
    clauses = lapply(1:n_clauses, function(j) {
      n_atoms = sample(1:2, 1)
      chosen = sample(names(syms), n_atoms)
      atoms = lapply(chosen, function(s) {
        dom = u[[s]]
        syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    CnfFormula(clauses)
  }, error = function(e) NULL)
  if (is.null(f)) next

  # f | TRUE == TRUE
  f_or_T = tryCatch(f | as.CnfFormula(TRUE), error = function(e) e)
  if (!inherits(f_or_T, "error")) {
    n_tests <<- n_tests + 1
    truth = evaluate_formula(f_or_T, u)
    if (!all(truth)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [or_TRUE trial %d]\n", trial))
    }
  }

  # f | FALSE == f
  f_or_F = tryCatch(f | as.CnfFormula(FALSE), error = function(e) e)
  if (!inherits(f_or_F, "error")) {
    check_equiv(f, f_or_F, u, sprintf("or_FALSE-%d", trial))
  }

  # TRUE | f == TRUE
  T_or_f = tryCatch(as.CnfFormula(TRUE) | f, error = function(e) e)
  if (!inherits(T_or_f, "error")) {
    n_tests <<- n_tests + 1
    truth = evaluate_formula(T_or_f, u)
    if (!all(truth)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [TRUE_or trial %d]\n", trial))
    }
  }

  # FALSE | f == f
  F_or_f = tryCatch(as.CnfFormula(FALSE) | f, error = function(e) e)
  if (!inherits(F_or_f, "error")) {
    check_equiv(f, F_or_f, u, sprintf("FALSE_or-%d", trial))
  }
}
cat(sprintf("  OR constants done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== OR: asymmetric formulas (one large, one small) ===\n")
for (trial in 1:200) {
  # One formula with many clauses, one with just 1
  f_big = tryCatch({
    clauses = lapply(1:sample(3:6, 1), function(j) {
      chosen = sample(names(syms), sample(1:3, 1))
      atoms = lapply(chosen, function(s) {
        dom = u[[s]]
        syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    CnfFormula(clauses)
  }, error = function(e) NULL)

  f_small = tryCatch({
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    CnfFormula(list(as.CnfClause(Reduce(`|`, atoms))))
  }, error = function(e) NULL)

  if (is.null(f_big) || is.null(f_small)) next

  f_or = tryCatch(f_big | f_small, error = function(e) e)
  if (inherits(f_or, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [asym trial %d]: %s\n", trial, f_or$message)); next
  }

  n_tests <<- n_tests + 1
  truth_big = evaluate_formula(f_big, u)
  truth_small = evaluate_formula(f_small, u)
  truth_or = evaluate_formula(f_or, u)
  expected = truth_big | truth_small
  mismatches = which(truth_or != expected)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    cat(sprintf("FAIL [asym trial %d]\n", trial))
  }
}
cat(sprintf("  OR asymmetric done: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
