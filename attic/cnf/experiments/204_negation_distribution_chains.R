#!/usr/bin/env Rscript
# Complex negation-distribution chains:
# !(f1 | f2) = !f1 & !f2 (De Morgan)
# !(f1 & f2) = !f1 | !f2 (De Morgan)
# Test deep chains of alternating negation and distribution.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: !(f1 | f2) == !f1 & !f2 ===
cat("=== De Morgan OR ===\n")
set.seed(204001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(1:3, 1)
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

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  lhs = tryCatch(!(f1 | f2), error = function(e) e)
  rhs = tryCatch(!f1 & !f2, error = function(e) e)

  if (inherits(lhs, "error") || inherits(rhs, "error")) {
    if (inherits(lhs, "error")) cat(sprintf("ERROR [dmor_lhs-%d]: %s\n", trial, lhs$message))
    if (inherits(rhs, "error")) cat(sprintf("ERROR [dmor_rhs-%d]: %s\n", trial, rhs$message))
    n_failures = n_failures + 1
    next
  }

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dmor-%d]: !(f1|f2) != !f1 & !f2\n", trial))
  }
}
cat(sprintf("  De Morgan OR: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: !(f1 & f2) == !f1 | !f2 ===
cat("\n=== De Morgan AND ===\n")
set.seed(204002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(1:3, 1)
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

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  lhs = tryCatch(!(f1 & f2), error = function(e) e)
  rhs = tryCatch(!f1 | !f2, error = function(e) e)

  if (inherits(lhs, "error") || inherits(rhs, "error")) {
    if (inherits(lhs, "error")) cat(sprintf("ERROR [dmand_lhs-%d]: %s\n", trial, lhs$message))
    if (inherits(rhs, "error")) cat(sprintf("ERROR [dmand_rhs-%d]: %s\n", trial, rhs$message))
    n_failures = n_failures + 1
    next
  }

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dmand-%d]: !(f1&f2) != !f1 | !f2\n", trial))
  }
}
cat(sprintf("  De Morgan AND: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Triple negation chain ===
cat("\n=== Triple negation chain ===\n")
set.seed(204003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  n_tests = n_tests + 1

  # !(!(f1 | f2) & f3) should equal (f1 | f2) | !f3
  lhs = tryCatch(!(!(f1 | f2) & f3), error = function(e) e)
  rhs = tryCatch((f1 | f2) | !f3, error = function(e) e)

  if (inherits(lhs, "error") || inherits(rhs, "error")) {
    if (inherits(lhs, "error")) cat(sprintf("ERROR [triple_lhs-%d]: %s\n", trial, lhs$message))
    if (inherits(rhs, "error")) cat(sprintf("ERROR [triple_rhs-%d]: %s\n", trial, rhs$message))
    n_failures = n_failures + 1
    next
  }

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [triple-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Triple negation: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Alternating neg-or-and chain ===
cat("\n=== Alternating neg-or-and ===\n")
set.seed(204004)

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

  f1 = make_f(); f2 = make_f(); f3 = make_f(); f4 = make_f()
  if (is.null(f1) || is.null(f2) || is.null(f3) || is.null(f4)) next

  n_tests = n_tests + 1

  # Build complex expression via operations
  result = tryCatch({
    step1 = f1 | f2
    step2 = !step1
    step3 = step2 & f3
    step4 = step3 | f4
    !step4
  }, error = function(e) e)

  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [alt-%d]: %s\n", trial, result$message))
    next
  }

  # Verify against truth table
  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  t_f3 = evaluate_formula(f3, u)
  t_f4 = evaluate_formula(f4, u)
  expected = !((!(t_f1 | t_f2) & t_f3) | t_f4)

  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [alt-%d]: chain mismatch\n", trial))
  }
}
cat(sprintf("  Alternating chain: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 5: Deep OR distribution with negated sub-formulas ===
cat("\n=== Negated sub-formula distribution ===\n")
set.seed(204005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  # (!f1) | (!f2) via operator
  result = tryCatch(!f1 | !f2, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [negdist-%d]: %s\n", trial, result$message)); next
  }

  t_f1 = evaluate_formula(f1, u)
  t_f2 = evaluate_formula(f2, u)
  expected = !t_f1 | !t_f2

  t_result = evaluate_formula(result, u)
  if (!all(t_result == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [negdist-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Negated distribution: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
