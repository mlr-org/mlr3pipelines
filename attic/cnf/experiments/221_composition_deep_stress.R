#!/usr/bin/env Rscript
# Deep composition stress testing:
# Test complex compositions of formulas using AND, OR, NOT
# with various numbers of variables and domain sizes.
# Focus on compositions that produce large intermediate formulas
# which then simplify down.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: (f1 | f2) & (f3 | f4) ===
cat("=== Complex OR-AND compositions ===\n")
set.seed(221001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(2:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
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
  result = tryCatch((f1 | f2) & (f3 | f4), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [orand-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u); t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u); t4 = evaluate_formula(f4, u)
  expected = (t1 | t2) & (t3 | t4)
  actual = evaluate_formula(result, u)

  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [orand-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  OR-AND compositions: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: !(!f1 | !f2) == f1 & f2 (De Morgan) ===
cat("\n=== De Morgan stress ===\n")
set.seed(221002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
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

  # De Morgan: !(!f1 | !f2) should equal f1 & f2
  lhs = tryCatch(!(!f1 | !f2), error = function(e) e)
  rhs = tryCatch(f1 & f2, error = function(e) e)

  if (inherits(lhs, "error") || inherits(rhs, "error")) {
    n_failures = n_failures + 1
    msg = if (inherits(lhs, "error")) lhs$message else rhs$message
    cat(sprintf("ERROR [demorgan-%d]: %s\n", trial, msg)); next
  }

  t_lhs = evaluate_formula(lhs, u)
  t_rhs = evaluate_formula(rhs, u)
  if (!all(t_lhs == t_rhs)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [demorgan-%d]: De Morgan mismatch\n", trial))
  }
}
cat(sprintf("  De Morgan: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Repeated application of operations ===
cat("\n=== Repeated operations ===\n")
set.seed(221003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  formulas = list()
  for (i in 1:sample(3:5, 1)) {
    f = make_f()
    if (!is.null(f)) formulas[[length(formulas) + 1]] = f
  }
  if (length(formulas) < 3) next

  n_tests = n_tests + 1

  # Build up a complex expression using random operations
  result = formulas[[1]]
  expected = evaluate_formula(formulas[[1]], u)
  for (i in 2:length(formulas)) {
    op = sample(1:3, 1)
    fi = formulas[[i]]
    ti = evaluate_formula(fi, u)
    result = tryCatch({
      if (op == 1) result & fi
      else if (op == 2) result | fi
      else !result  # then & fi
    }, error = function(e) e)
    if (inherits(result, "error")) break
    expected = if (op == 1) expected & ti
    else if (op == 2) expected | ti
    else !expected

    if (op == 3) {
      result = tryCatch(result & fi, error = function(e) e)
      if (inherits(result, "error")) break
      expected = expected & ti
    }
  }

  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [repeat-%d]: %s\n", trial, result$message)); next
  }

  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [repeat-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Repeated operations: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: f & !f should be FALSE, f | !f should be TRUE ===
cat("\n=== Complement verification ===\n")
set.seed(221004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(1:3, 1)
  cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 1) next

  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  nf = tryCatch(!f, error = function(e) e)
  if (inherits(nf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [comp-%d]: %s\n", trial, nf$message)); next
  }

  # f & !f should give FALSE
  f_and_nf = tryCatch(f & nf, error = function(e) e)
  if (inherits(f_and_nf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [comp-and-%d]: %s\n", trial, f_and_nf$message)); next
  }
  t_and = evaluate_formula(f_and_nf, u)
  if (any(t_and)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [comp-and-%d]: f & !f not FALSE\n", trial)); next
  }

  # f | !f should give TRUE
  f_or_nf = tryCatch(f | nf, error = function(e) e)
  if (inherits(f_or_nf, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [comp-or-%d]: %s\n", trial, f_or_nf$message)); next
  }
  t_or = evaluate_formula(f_or_nf, u)
  if (!all(t_or)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [comp-or-%d]: f | !f not TRUE\n", trial))
  }
}
cat(sprintf("  Complement verification: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
