#!/usr/bin/env Rscript
# Special value name testing:
# Test with domain values that could be confused with R internal values,
# empty strings, or values that match R function names.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

evaluate_raw_clauses = function(clauses, universe) {
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }
  raw_truth
}

# === Pattern 1: Values that look like logicals ===
cat("=== Logical-like values ===\n")
set.seed(246001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("TRUE", "FALSE", "NA", "NULL")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [logval-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [logval-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Logical-like values: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Values with spaces and special characters ===
cat("\n=== Special character values ===\n")
set.seed(246002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a b", "c.d", "e_f", "g-h", "i+j")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [specchar-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [specchar-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Special chars: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Numeric string values ===
cat("\n=== Numeric string values ===\n")
set.seed(246003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("0", "1", "2", "10", "100")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [numstr-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [numstr-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Numeric strings: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Long symbol names ===
cat("\n=== Long symbol names ===\n")
set.seed(246004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("val_alpha_1", "val_beta_2", "val_gamma_3", "val_delta_4")
  long_names = c("very_long_symbol_name_alpha", "very_long_symbol_name_beta",
                 "very_long_symbol_name_gamma")
  syms = list()
  for (s in long_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(long_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [longname-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [longname-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Long names: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 5: Operations with special values ===
cat("\n=== Operations with special values ===\n")
set.seed(246005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("TRUE", "FALSE", "NA")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  make_f = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      atoms = list()
      if (runif(1) > 0.3) atoms[[length(atoms) + 1]] = A %among% sample(dom, sample(1:2, 1))
      if (runif(1) > 0.3) atoms[[length(atoms) + 1]] = B %among% sample(dom, sample(1:2, 1))
      if (length(atoms) == 0) atoms[[1]] = A %among% sample(dom, 1)
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  # AND
  r = tryCatch(f1 & f2, error = function(e) e)
  if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR [specop-%d-and]: %s\n", trial, r$message)); next }
  t1 = evaluate_formula(f1, u); t2 = evaluate_formula(f2, u)
  if (!all(evaluate_formula(r, u) == (t1 & t2))) { n_failures = n_failures + 1; cat(sprintf("FAIL [specop-%d-and]\n", trial)); next }

  # OR
  r = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR [specop-%d-or]: %s\n", trial, r$message)); next }
  if (!all(evaluate_formula(r, u) == (t1 | t2))) { n_failures = n_failures + 1; cat(sprintf("FAIL [specop-%d-or]\n", trial)); next }

  # NOT
  r = tryCatch(!f1, error = function(e) e)
  if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR [specop-%d-not]: %s\n", trial, r$message)); next }
  if (!all(evaluate_formula(r, u) == !t1)) { n_failures = n_failures + 1; cat(sprintf("FAIL [specop-%d-not]\n", trial)) }
}
cat(sprintf("  Special value ops: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
