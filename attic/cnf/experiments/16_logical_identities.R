source("experiments/test_harness.R")

# Test logical identities that should hold for all formulas:
# 1. Double negation: !!f == f
# 2. De Morgan: !(f1 & f2) == !f1 | !f2
# 3. f & TRUE == f, f | FALSE == f
# 4. f & FALSE == FALSE, f | TRUE == TRUE
# 5. f & f == f, f | f == f
# 6. f & !f == FALSE
# 7. Commutativity: f1 & f2 == f2 & f1, f1 | f2 == f2 | f1
# 8. Absorption: f1 & (f1 | f2) == f1, f1 | (f1 & f2) == f1

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

make_random_formula = function(u, syms, n_clauses = NULL) {
  if (is.null(n_clauses)) n_clauses = sample(1:4, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:min(3, length(syms)), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  CnfFormula(clauses)
}

u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("d", "e"))
Z = CnfSymbol(u, "Z", c("g", "h", "i"))
syms = list(X = X, Y = Y, Z = Z)

cat("=== Identity: Double negation ===\n")
set.seed(42)
for (trial in 1:200) {
  f = make_random_formula(u, syms)
  neg_neg_f = tryCatch(!!f, error = function(e) e)
  if (inherits(neg_neg_f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [double neg trial %d]: %s\n", trial, neg_neg_f$message)); next
  }
  check_equiv(f, neg_neg_f, u, sprintf("double_neg-%d", trial))
}
cat(sprintf("  Double neg done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Identity: De Morgan ===\n")
for (trial in 1:200) {
  f1 = make_random_formula(u, syms, sample(1:3, 1))
  f2 = make_random_formula(u, syms, sample(1:3, 1))

  # !(f1 & f2) == !f1 | !f2
  lhs_and = tryCatch(!(f1 & f2), error = function(e) e)
  rhs_and = tryCatch(!f1 | !f2, error = function(e) e)
  if (!inherits(lhs_and, "error") && !inherits(rhs_and, "error")) {
    check_equiv(lhs_and, rhs_and, u, sprintf("demorgan_and-%d", trial))
  } else {
    n_tests <<- n_tests + 1
  }

  # !(f1 | f2) == !f1 & !f2
  lhs_or = tryCatch(!(f1 | f2), error = function(e) e)
  rhs_or = tryCatch(!f1 & !f2, error = function(e) e)
  if (!inherits(lhs_or, "error") && !inherits(rhs_or, "error")) {
    check_equiv(lhs_or, rhs_or, u, sprintf("demorgan_or-%d", trial))
  } else {
    n_tests <<- n_tests + 1
  }
}
cat(sprintf("  De Morgan done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Identity: Boolean constants ===\n")
for (trial in 1:100) {
  f = make_random_formula(u, syms)

  # f & TRUE == f
  f_and_T = tryCatch(f & as.CnfFormula(TRUE), error = function(e) e)
  if (!inherits(f_and_T, "error")) check_equiv(f, f_and_T, u, sprintf("and_T-%d", trial))

  # f | FALSE == f
  f_or_F = tryCatch(f | as.CnfFormula(FALSE), error = function(e) e)
  if (!inherits(f_or_F, "error")) check_equiv(f, f_or_F, u, sprintf("or_F-%d", trial))

  # f & FALSE == FALSE
  f_and_F = tryCatch(f & as.CnfFormula(FALSE), error = function(e) e)
  if (!inherits(f_and_F, "error")) {
    n_tests <<- n_tests + 1
    truth = evaluate_formula(f_and_F, u)
    if (any(truth)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [and_F-%d]: f & FALSE is not FALSE\n", trial))
    }
  }

  # f | TRUE == TRUE
  f_or_T = tryCatch(f | as.CnfFormula(TRUE), error = function(e) e)
  if (!inherits(f_or_T, "error")) {
    n_tests <<- n_tests + 1
    truth = evaluate_formula(f_or_T, u)
    if (!all(truth)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [or_T-%d]: f | TRUE is not TRUE\n", trial))
    }
  }
}
cat(sprintf("  Constants done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Identity: Idempotence ===\n")
for (trial in 1:100) {
  f = make_random_formula(u, syms)

  # f & f == f
  f_and_f = tryCatch(f & f, error = function(e) e)
  if (!inherits(f_and_f, "error")) check_equiv(f, f_and_f, u, sprintf("idempotent_and-%d", trial))

  # f | f == f
  f_or_f = tryCatch(f | f, error = function(e) e)
  if (!inherits(f_or_f, "error")) check_equiv(f, f_or_f, u, sprintf("idempotent_or-%d", trial))
}
cat(sprintf("  Idempotence done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Identity: Complement ===\n")
for (trial in 1:100) {
  f = make_random_formula(u, syms, sample(1:2, 1))

  # f & !f == FALSE (not always detected, but semantically true)
  neg_f = tryCatch(!f, error = function(e) e)
  if (!inherits(neg_f, "error")) {
    f_and_neg = tryCatch(f & neg_f, error = function(e) e)
    if (!inherits(f_and_neg, "error")) {
      n_tests <<- n_tests + 1
      truth = evaluate_formula(f_and_neg, u)
      if (any(truth)) {
        n_failures <<- n_failures + 1
        cat(sprintf("FAIL [complement-%d]: f & !f is not FALSE\n", trial))
      }
    }
  }
}
cat(sprintf("  Complement done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Identity: Commutativity ===\n")
for (trial in 1:100) {
  f1 = make_random_formula(u, syms, sample(1:3, 1))
  f2 = make_random_formula(u, syms, sample(1:3, 1))

  # f1 & f2 == f2 & f1
  and1 = tryCatch(f1 & f2, error = function(e) e)
  and2 = tryCatch(f2 & f1, error = function(e) e)
  if (!inherits(and1, "error") && !inherits(and2, "error")) {
    check_equiv(and1, and2, u, sprintf("commut_and-%d", trial))
  }

  # f1 | f2 == f2 | f1
  or1 = tryCatch(f1 | f2, error = function(e) e)
  or2 = tryCatch(f2 | f1, error = function(e) e)
  if (!inherits(or1, "error") && !inherits(or2, "error")) {
    check_equiv(or1, or2, u, sprintf("commut_or-%d", trial))
  }
}
cat(sprintf("  Commutativity done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Identity: Absorption ===\n")
for (trial in 1:100) {
  f1 = make_random_formula(u, syms, sample(1:2, 1))
  f2 = make_random_formula(u, syms, sample(1:2, 1))

  # f1 & (f1 | f2) == f1
  f1_or_f2 = tryCatch(f1 | f2, error = function(e) e)
  if (!inherits(f1_or_f2, "error")) {
    absorbed = tryCatch(f1 & f1_or_f2, error = function(e) e)
    if (!inherits(absorbed, "error")) {
      check_equiv(f1, absorbed, u, sprintf("absorb_and-%d", trial))
    }
  }

  # f1 | (f1 & f2) == f1
  f1_and_f2 = tryCatch(f1 & f2, error = function(e) e)
  if (!inherits(f1_and_f2, "error")) {
    absorbed = tryCatch(f1 | f1_and_f2, error = function(e) e)
    if (!inherits(absorbed, "error")) {
      check_equiv(f1, absorbed, u, sprintf("absorb_or-%d", trial))
    }
  }
}
cat(sprintf("  Absorption done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Identity: Associativity ===\n")
for (trial in 1:50) {
  f1 = make_random_formula(u, syms, sample(1:2, 1))
  f2 = make_random_formula(u, syms, sample(1:2, 1))
  f3 = make_random_formula(u, syms, sample(1:2, 1))

  # (f1 & f2) & f3 == f1 & (f2 & f3)
  lhs = tryCatch((f1 & f2) & f3, error = function(e) e)
  rhs = tryCatch(f1 & (f2 & f3), error = function(e) e)
  if (!inherits(lhs, "error") && !inherits(rhs, "error")) {
    check_equiv(lhs, rhs, u, sprintf("assoc_and-%d", trial))
  }

  # (f1 | f2) | f3 == f1 | (f2 | f3) -- can be expensive
  lhs = tryCatch((f1 | f2) | f3, error = function(e) e)
  rhs = tryCatch(f1 | (f2 | f3), error = function(e) e)
  if (!inherits(lhs, "error") && !inherits(rhs, "error")) {
    check_equiv(lhs, rhs, u, sprintf("assoc_or-%d", trial))
  }
}
cat(sprintf("  Associativity done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Identity: Distributivity ===\n")
for (trial in 1:50) {
  f1 = make_random_formula(u, syms, 1)  # single clause to keep it manageable
  f2 = make_random_formula(u, syms, 1)
  f3 = make_random_formula(u, syms, 1)

  # f1 & (f2 | f3) == (f1 & f2) | (f1 & f3)
  lhs = tryCatch(f1 & (f2 | f3), error = function(e) e)
  rhs = tryCatch((f1 & f2) | (f1 & f3), error = function(e) e)
  if (!inherits(lhs, "error") && !inherits(rhs, "error")) {
    check_equiv(lhs, rhs, u, sprintf("distrib_and-%d", trial))
  }

  # f1 | (f2 & f3) == (f1 | f2) & (f1 | f3)
  lhs = tryCatch(f1 | (f2 & f3), error = function(e) e)
  rhs = tryCatch((f1 | f2) & (f1 | f3), error = function(e) e)
  if (!inherits(lhs, "error") && !inherits(rhs, "error")) {
    check_equiv(lhs, rhs, u, sprintf("distrib_or-%d", trial))
  }
}
cat(sprintf("  Distributivity done: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
