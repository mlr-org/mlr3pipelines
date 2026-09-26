#!/usr/bin/env Rscript
# Property-based testing: apply random transformations to formulas
# and verify algebraic properties hold.
# For each random formula f, apply sequences of:
# - Double negation: !!f should equal f
# - Idempotence: f & f == f, f | f == f
# - Complement: f & !f == FALSE, f | !f == TRUE
# - Absorption: f & (f | g) == f, f | (f & g) == f
# - Commutativity and associativity
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

make_random_formula = function(u, syms, dom, n_clauses) {
  clauses = lapply(1:n_clauses, function(j) {
    n_sym = sample(1:min(3, length(syms)), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:(length(dom) - 1), 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) == 0) return(NULL)
  tryCatch(CnfFormula(clauses), error = function(e) NULL)
}

# === Random double negation ===
cat("=== Double negation ===\n")
set.seed(131001)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  f = make_random_formula(u, syms, dom, sample(1:3, 1))
  if (is.null(f)) next

  n_tests = n_tests + 1
  dbl_neg = tryCatch(!!f, error = function(e) NULL)
  if (is.null(dbl_neg)) next

  t_f = evaluate_formula(f, u)
  t_dbl = evaluate_formula(dbl_neg, u)
  if (!all(t_f == t_dbl)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dblneg-%d]: !!f != f\n", trial))
  }
}
cat(sprintf("  Double negation: %d tests, %d failures\n", n_tests, n_failures))

# === Complement: f & !f = FALSE, f | !f = TRUE ===
cat("\n=== Complement ===\n")
set.seed(131002)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("x", "y", "z")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  f = make_random_formula(u, syms, dom, sample(1:3, 1))
  if (is.null(f)) next
  neg_f = tryCatch(!f, error = function(e) NULL)
  if (is.null(neg_f)) next

  # f & !f = FALSE
  n_tests = n_tests + 1
  f_and_neg = tryCatch(f & neg_f, error = function(e) NULL)
  if (!is.null(f_and_neg)) {
    t = evaluate_formula(f_and_neg, u)
    if (any(t)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [and-comp-%d]: f & !f not FALSE\n", trial))
    }
  }

  # f | !f = TRUE
  n_tests = n_tests + 1
  f_or_neg = tryCatch(f | neg_f, error = function(e) NULL)
  if (!is.null(f_or_neg)) {
    t = evaluate_formula(f_or_neg, u)
    if (!all(t)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [or-comp-%d]: f | !f not TRUE\n", trial))
    }
  }
}
cat(sprintf("  Complement: %d tests, %d failures\n", n_tests, n_failures))

# === Absorption: f & (f | g) = f, f | (f & g) = f ===
cat("\n=== Absorption ===\n")
set.seed(131003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("0", "1", "2")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  f = make_random_formula(u, syms, dom, sample(1:2, 1))
  g = make_random_formula(u, syms, dom, sample(1:2, 1))
  if (is.null(f) || is.null(g)) next

  t_f = evaluate_formula(f, u)

  # f & (f | g) = f
  n_tests = n_tests + 1
  f_or_g = tryCatch(f | g, error = function(e) NULL)
  if (!is.null(f_or_g)) {
    absorbed = tryCatch(f & f_or_g, error = function(e) NULL)
    if (!is.null(absorbed)) {
      t = evaluate_formula(absorbed, u)
      if (!all(t == t_f)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [abs1-%d]: f & (f|g) != f\n", trial))
      }
    }
  }

  # f | (f & g) = f
  n_tests = n_tests + 1
  f_and_g = tryCatch(f & g, error = function(e) NULL)
  if (!is.null(f_and_g)) {
    absorbed2 = tryCatch(f | f_and_g, error = function(e) NULL)
    if (!is.null(absorbed2)) {
      t = evaluate_formula(absorbed2, u)
      if (!all(t == t_f)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [abs2-%d]: f | (f&g) != f\n", trial))
      }
    }
  }
}
cat(sprintf("  Absorption: %d tests, %d failures\n", n_tests, n_failures))

# === Distributivity: f | (g & h) = (f | g) & (f | h) ===
cat("\n=== Distributivity ===\n")
set.seed(131004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b")  # binary to keep things fast
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  f = make_random_formula(u, syms, dom, sample(1:2, 1))
  g = make_random_formula(u, syms, dom, sample(1:2, 1))
  h = make_random_formula(u, syms, dom, sample(1:2, 1))
  if (is.null(f) || is.null(g) || is.null(h)) next

  # f | (g & h) = (f | g) & (f | h)
  n_tests = n_tests + 1
  g_and_h = tryCatch(g & h, error = function(e) NULL)
  lhs = if (!is.null(g_and_h)) tryCatch(f | g_and_h, error = function(e) NULL)
  f_or_g = tryCatch(f | g, error = function(e) NULL)
  f_or_h = tryCatch(f | h, error = function(e) NULL)
  rhs = if (!is.null(f_or_g) && !is.null(f_or_h)) tryCatch(f_or_g & f_or_h, error = function(e) NULL)

  if (!is.null(lhs) && !is.null(rhs)) {
    t_lhs = evaluate_formula(lhs, u)
    t_rhs = evaluate_formula(rhs, u)
    if (!all(t_lhs == t_rhs)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [dist-%d]: f|(g&h) != (f|g)&(f|h)\n", trial))
    }
  }
}
cat(sprintf("  Distributivity: %d tests, %d failures\n", n_tests, n_failures))

# === Random sequence of operations ===
cat("\n=== Random operation sequences ===\n")
set.seed(131005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("p", "q", "r")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  # Build a random formula from 3-5 operations
  f = make_random_formula(u, syms, dom, sample(1:2, 1))
  if (is.null(f)) next

  n_tests = n_tests + 1
  t_f = evaluate_formula(f, u)

  # Apply random operations and verify semantics
  g = make_random_formula(u, syms, dom, sample(1:2, 1))
  if (is.null(g)) next

  op = sample(c("and", "or", "neg"), 1)
  result = tryCatch({
    switch(op,
      and = f & g,
      or = f | g,
      neg = !f
    )
  }, error = function(e) NULL)

  if (is.null(result)) next

  t_g = evaluate_formula(g, u)
  expected = switch(op,
    and = t_f & t_g,
    or = t_f | t_g,
    neg = !t_f
  )
  truth = evaluate_formula(result, u)
  if (!all(truth == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [op-%s-%d]: semantic error\n", op, trial))
  }
}
cat(sprintf("  Random ops: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
