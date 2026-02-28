#!/usr/bin/env Rscript
# Mixed domain sizes with wide clauses:
# Test formulas where variables have very different domain sizes
# (e.g., binary, ternary, and 5-valued mixed together).
# This stresses the subset checking logic when some symbols have
# tight ranges and others have wide ranges.
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

# === Pattern 1: Binary + quaternary + singleton ===
cat("=== Binary + quaternary + singleton ===\n")
set.seed(226001)

for (trial in 1:500) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("T", "F"))  # binary
  B = CnfSymbol(u, "B", c("T", "F"))  # binary
  C = CnfSymbol(u, "C", c("a", "b", "c", "d"))  # quaternary
  D = CnfSymbol(u, "D", c("x"))  # singleton - always "x"
  E = CnfSymbol(u, "E", c("a", "b", "c", "d", "e"))  # 5-valued
  syms = list(A = A, B = B, C = C, D = D, E = E)
  doms = list(A = c("T", "F"), B = c("T", "F"), C = c("a", "b", "c", "d"), D = c("x"), E = c("a", "b", "c", "d", "e"))

  n_cl = sample(4:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:max(1, length(d)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mixed1-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixed1-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Binary+quat+single: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Three different sizes, operations ===
cat("\n=== Three sizes, operations ===\n")
set.seed(226002)

for (trial in 1:400) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("0", "1"))
  B = CnfSymbol(u, "B", c("a", "b", "c"))
  C = CnfSymbol(u, "C", c("w", "x", "y", "z", "v", "u"))
  syms = list(A = A, B = B, C = C)
  doms = list(A = c("0", "1"), B = c("a", "b", "c"), C = c("w", "x", "y", "z", "v", "u"))

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) {
        d = doms[[s]]
        syms[[s]] %among% sample(d, sample(1:max(1, length(d)-1), 1))
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

  op = sample(1:4, 1)
  result = tryCatch({
    if (op == 1) f1 & f2
    else if (op == 2) f1 | f2
    else if (op == 3) !f1
    else !f1 & f2
  }, error = function(e) e)

  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [3size-%d]: %s\n", trial, result$message)); next
  }

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  expected = if (op == 1) t1 & t2
  else if (op == 2) t1 | t2
  else if (op == 3) !t1
  else !t1 & t2

  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3size-%d]: op=%d mismatch\n", trial, op))
  }
}
cat(sprintf("  Three sizes ops: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Large domain + binary mixed ===
cat("\n=== Large domain + binary ===\n")
set.seed(226003)

for (trial in 1:400) {
  u = CnfUniverse()
  large_dom = paste0("v", 1:8)
  A = CnfSymbol(u, "A", c("T", "F"))
  B = CnfSymbol(u, "B", c("T", "F"))
  C = CnfSymbol(u, "C", c("T", "F"))
  D = CnfSymbol(u, "D", large_dom)
  syms = list(A = A, B = B, C = C, D = D)
  doms = list(A = c("T", "F"), B = c("T", "F"), C = c("T", "F"), D = large_dom)

  n_cl = sample(4:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:max(1, length(d)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [lgbin-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [lgbin-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Large+binary: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: 6 variables with different domain sizes ===
cat("\n=== 6 variables mixed domains ===\n")
set.seed(226004)

for (trial in 1:300) {
  u = CnfUniverse()
  V1 = CnfSymbol(u, "V1", c("a"))           # singleton
  V2 = CnfSymbol(u, "V2", c("a", "b"))       # binary
  V3 = CnfSymbol(u, "V3", c("a", "b", "c"))  # ternary
  V4 = CnfSymbol(u, "V4", c("a", "b"))       # binary
  V5 = CnfSymbol(u, "V5", c("a", "b", "c", "d"))  # quaternary
  V6 = CnfSymbol(u, "V6", c("a", "b", "c"))  # ternary
  syms = list(V1 = V1, V2 = V2, V3 = V3, V4 = V4, V5 = V5, V6 = V6)
  doms = list(V1 = c("a"), V2 = c("a", "b"), V3 = c("a", "b", "c"),
              V4 = c("a", "b"), V5 = c("a", "b", "c", "d"), V6 = c("a", "b", "c"))

  n_cl = sample(5:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:5, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:max(1, length(d)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [6mix-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [6mix-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  6 vars mixed domains: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
