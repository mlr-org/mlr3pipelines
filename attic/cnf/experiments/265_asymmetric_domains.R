#!/usr/bin/env Rscript
# Test with variables that have different domain sizes.
# Previous experiments mostly used uniform domains. Here we mix
# binary (2-val), ternary (3-val), and larger (5-val) domains
# within the same formula.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Mix of binary and ternary ===
cat("=== Binary + ternary ===\n")
set.seed(265001)

for (trial in 1:500) {
  u = CnfUniverse()
  # V1, V2 are binary; V3, V4 are ternary
  syms = list()
  syms[["V1"]] = CnfSymbol(u, "V1", c("0", "1"))
  syms[["V2"]] = CnfSymbol(u, "V2", c("0", "1"))
  syms[["V3"]] = CnfSymbol(u, "V3", c("a", "b", "c"))
  syms[["V4"]] = CnfSymbol(u, "V4", c("a", "b", "c"))
  sym_names = paste0("V", 1:4)

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) {
      dom = get(s, u)
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [bt-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [bt-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Binary + ternary: %d tests, %d failures\n", n_tests, n_failures))

# === Binary, ternary, and 5-val ===
cat("\n=== Mixed 2/3/5 ===\n")
set.seed(265002)

for (trial in 1:500) {
  u = CnfUniverse()
  syms = list()
  syms[["B1"]] = CnfSymbol(u, "B1", c("0", "1"))
  syms[["B2"]] = CnfSymbol(u, "B2", c("0", "1"))
  syms[["T1"]] = CnfSymbol(u, "T1", c("x", "y", "z"))
  syms[["L1"]] = CnfSymbol(u, "L1", c("a", "b", "c", "d", "e"))
  sym_names = c("B1", "B2", "T1", "L1")

  n_cl = sample(3:7, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:4, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) {
      dom = get(s, u)
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [m235-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [m235-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Mixed 2/3/5: %d tests, %d failures\n", n_tests, n_failures))

# === Asymmetric domain operations ===
cat("\n=== Asymmetric operations ===\n")
set.seed(265003)

for (trial in 1:400) {
  u = CnfUniverse()
  syms = list()
  syms[["A"]] = CnfSymbol(u, "A", c("0", "1"))
  syms[["B"]] = CnfSymbol(u, "B", c("x", "y", "z"))
  syms[["C"]] = CnfSymbol(u, "C", c("p", "q", "r", "s"))
  sym_names = c("A", "B", "C")

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) {
        dom = get(s, u)
        syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  # Test all operations
  r_and = tryCatch(f1 & f2, error = function(e) NULL)
  r_or = tryCatch(f1 | f2, error = function(e) NULL)
  r_not = tryCatch(!f1, error = function(e) NULL)

  fail = FALSE
  if (!is.null(r_and) && !all(evaluate_formula(r_and, u) == (t1 & t2))) {
    fail = TRUE; cat(sprintf("FAIL [asymop-%d]: AND mismatch\n", trial))
  }
  if (!is.null(r_or) && !all(evaluate_formula(r_or, u) == (t1 | t2))) {
    fail = TRUE; cat(sprintf("FAIL [asymop-%d]: OR mismatch\n", trial))
  }
  if (!is.null(r_not) && !all(evaluate_formula(r_not, u) == !t1)) {
    fail = TRUE; cat(sprintf("FAIL [asymop-%d]: NOT mismatch\n", trial))
  }
  if (fail) n_failures = n_failures + 1
}
cat(sprintf("  Asymmetric operations: %d tests, %d failures\n", n_tests, n_failures))

# === Domain size 1 and larger mixed ===
cat("\n=== Domain-1 + larger ===\n")
set.seed(265004)

for (trial in 1:300) {
  u = CnfUniverse()
  syms = list()
  syms[["V1"]] = CnfSymbol(u, "V1", "only")
  syms[["V2"]] = CnfSymbol(u, "V2", c("a", "b", "c"))
  syms[["V3"]] = CnfSymbol(u, "V3", c("x", "y"))
  sym_names = c("V1", "V2", "V3")

  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) {
      dom = get(s, u)
      syms[[s]] %among% sample(dom, sample(1:length(dom), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dom1-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dom1-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Domain-1 + larger: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
