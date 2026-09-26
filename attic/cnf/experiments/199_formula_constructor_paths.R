#!/usr/bin/env Rscript
# CnfFormula constructor paths:
# - CnfFormula(list(CnfFormula, CnfFormula)) -- combining formula objects
# - CnfFormula(list(CnfClause, CnfFormula, CnfClause)) -- mixed
# - CnfFormula(list(FALSE_clause, ...)) -- formula with FALSE clause
# - CnfFormula(list(TRUE_clause, ...)) -- formula with TRUE clause
# - as.CnfFormula on various inputs
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: CnfFormula(list(f1, f2)) ===
cat("=== CnfFormula(list(f1, f2)) ===\n")
set.seed(199001)

for (trial in 1:1000) {
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
  # Combine via CnfFormula constructor
  combined = tryCatch(CnfFormula(list(f1, f2)), error = function(e) e)
  if (inherits(combined, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [comb-%d]: %s\n", trial, combined$message)); next
  }
  # Compare with &
  anded = tryCatch(f1 & f2, error = function(e) e)
  if (inherits(anded, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [and-%d]: %s\n", trial, anded$message)); next
  }

  t_combined = evaluate_formula(combined, u)
  t_anded = evaluate_formula(anded, u)
  if (!all(t_combined == t_anded)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [comb-%d]: CnfFormula(list(f1,f2)) != f1&f2\n", trial))
  }
}
cat(sprintf("  CnfFormula(list(f1,f2)): %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Mixed CnfClause and CnfFormula ===
cat("\n=== Mixed clause and formula ===\n")
set.seed(199002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Make a clause
  n_sym = sample(1:3, 1)
  chosen = sample(names(syms), n_sym)
  atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
  cl = as.CnfClause(Reduce(`|`, atoms))

  # Make a formula
  cls = lapply(1:sample(1:3, 1), function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 1) next
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next
  if (isTRUE(unclass(cl))) next

  n_tests = n_tests + 1
  # Mix them
  mixed = tryCatch(CnfFormula(list(cl, f, cl)), error = function(e) e)
  if (inherits(mixed, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mix-%d]: %s\n", trial, mixed$message)); next
  }

  # Compare: should be equivalent to as.CnfFormula(cl) & f
  cl_f = as.CnfFormula(cl)
  manual = tryCatch(cl_f & f, error = function(e) NULL)
  if (is.null(manual)) next

  t_mixed = evaluate_formula(mixed, u)
  t_manual = evaluate_formula(manual, u)
  if (!all(t_mixed == t_manual)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mix-%d]: CnfFormula(list(cl,f,cl)) != cl&f\n", trial))
  }
}
cat(sprintf("  Mixed clause/formula: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: CnfFormula with FALSE clause ===
cat("\n=== FALSE clause in CnfFormula ===\n")
set.seed(199003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  false_cl = as.CnfClause(FALSE)

  n_cl = sample(1:3, 1)
  other_cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  other_cls = other_cls[!sapply(other_cls, function(x) isTRUE(unclass(x)))]

  n_tests = n_tests + 1
  all_cls = c(list(false_cl), other_cls)
  result = tryCatch(CnfFormula(all_cls), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [false-%d]: %s\n", trial, result$message)); next
  }
  # Should be FALSE (AND with FALSE = FALSE)
  if (!isFALSE(unclass(result))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [false-%d]: should be FALSE but isn't\n", trial))
  }
}
cat(sprintf("  FALSE clause: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: CnfFormula with TRUE clause ===
cat("\n=== TRUE clause in CnfFormula ===\n")
set.seed(199004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  true_cl = as.CnfClause(TRUE)

  n_cl = sample(1:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  n_tests = n_tests + 1
  all_cls = c(list(true_cl), clauses)
  result_with_true = tryCatch(CnfFormula(all_cls), error = function(e) e)
  if (inherits(result_with_true, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [true-%d]: %s\n", trial, result_with_true$message)); next
  }

  result_without = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result_without, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [notrue-%d]: %s\n", trial, result_without$message)); next
  }

  t_with = evaluate_formula(result_with_true, u)
  t_without = evaluate_formula(result_without, u)
  if (!all(t_with == t_without)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [true-%d]: adding TRUE clause changed result\n", trial))
  }
}
cat(sprintf("  TRUE clause: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 5: CnfFormula with many CnfFormula objects ===
cat("\n=== Many formulas combined ===\n")
set.seed(199005)

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

  formulas = lapply(1:sample(3:5, 1), function(j) make_f())
  formulas = formulas[!sapply(formulas, is.null)]
  if (length(formulas) < 3) next

  n_tests = n_tests + 1
  combined = tryCatch(CnfFormula(formulas), error = function(e) e)
  if (inherits(combined, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [many-%d]: %s\n", trial, combined$message)); next
  }

  # Compare with Reduce(&)
  reduced = tryCatch(Reduce(function(x, y) x & y, formulas), error = function(e) NULL)
  if (is.null(reduced)) next

  t_combined = evaluate_formula(combined, u)
  t_reduced = evaluate_formula(reduced, u)
  if (!all(t_combined == t_reduced)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [many-%d]: CnfFormula(list(f1,...)) != f1&f2&...\n", trial))
  }
}
cat(sprintf("  Many formulas: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
