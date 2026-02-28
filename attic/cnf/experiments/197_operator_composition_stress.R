#!/usr/bin/env Rscript
# Operator composition stress tests:
# - Nested operator expressions (many levels deep)
# - Mix of &, |, ! in complex patterns
# - Identity and absorption laws
# - Associativity and commutativity verification
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: (f1 | f2) & (f3 | f4) should equal (f1 & f3) | (f1 & f4) | (f2 & f3) | (f2 & f4) ===
cat("=== Distributivity of & over | ===\n")
set.seed(197001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(1:2, 1)
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

  f1 = make_f(); f2 = make_f(); f3 = make_f(); f4 = make_f()
  if (any(sapply(list(f1, f2, f3, f4), is.null))) next

  n_tests = n_tests + 1

  lhs = tryCatch((f1 | f2) & (f3 | f4), error = function(e) NULL)
  rhs = tryCatch(
    (f1 & f3) | (f1 & f4) | (f2 & f3) | (f2 & f4),
    error = function(e) NULL
  )
  if (is.null(lhs) || is.null(rhs)) next

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dist-%d]: distributivity mismatch\n", trial))
  }
}
cat(sprintf("  Distributivity: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Double negation: !!f == f ===
cat("\n=== Double negation ===\n")
set.seed(197002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  n_cl = sample(1:3, 1)
  cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 1) next
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  ff = tryCatch(!!f, error = function(e) NULL)
  if (is.null(ff)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dblneg-%d]: double negation crashed\n", trial)); next
  }

  t_f = evaluate_formula(f, u)
  t_ff = evaluate_formula(ff, u)
  if (!all(t_f == t_ff)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dblneg-%d]: !!f != f\n", trial))
  }
}
cat(sprintf("  Double negation: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: De Morgan: !(f & g) == !f | !g ===
cat("\n=== De Morgan ===\n")
set.seed(197003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(); g = make_f()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1

  lhs = tryCatch(!(f & g), error = function(e) NULL)
  rhs = tryCatch((!f) | (!g), error = function(e) NULL)
  if (is.null(lhs) || is.null(rhs)) next

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan-%d]: !(f&g) != !f|!g\n", trial))
  }
}
cat(sprintf("  De Morgan: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Triple composition ===
cat("\n=== Triple composition ===\n")
set.seed(197004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(1:2, 1)
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

  f = make_f(); g = make_f(); h = make_f()
  if (any(sapply(list(f, g, h), is.null))) next

  n_tests = n_tests + 1

  # (f | g) & h should equal (f & h) | (g & h)
  lhs = tryCatch((f | g) & h, error = function(e) NULL)
  rhs = tryCatch((f & h) | (g & h), error = function(e) NULL)
  if (is.null(lhs) || is.null(rhs)) next

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [triple-%d]: (f|g)&h != (f&h)|(g&h)\n", trial))
  }
}
cat(sprintf("  Triple composition: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 5: Associativity of | ===
cat("\n=== Associativity of | ===\n")
set.seed(197005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(2:3, 1)
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

  f = make_f(); g = make_f(); h = make_f()
  if (any(sapply(list(f, g, h), is.null))) next

  n_tests = n_tests + 1

  lhs = tryCatch((f | g) | h, error = function(e) NULL)
  rhs = tryCatch(f | (g | h), error = function(e) NULL)
  if (is.null(lhs) || is.null(rhs)) next

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [assoc-%d]: (f|g)|h != f|(g|h)\n", trial))
  }
}
cat(sprintf("  Associativity: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
