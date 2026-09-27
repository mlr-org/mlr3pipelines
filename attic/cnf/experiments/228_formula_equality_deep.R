#!/usr/bin/env Rscript
# Deep formula equality testing:
# Tests all.equal, ==, and semantic equivalence under various
# construction paths. Also tests that structurally different
# formulas (from non-confluence) are detected as equal semantically.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: all.equal for semantically equivalent formulas ===
cat("=== all.equal semantic equivalence ===\n")
set.seed(228001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(3:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  f1 = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f1)) next

  # Build same formula via different order
  f2 = tryCatch(CnfFormula(rev(clauses)), error = function(e) NULL)
  if (is.null(f2)) next

  n_tests = n_tests + 1

  # Semantic equivalence must hold
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [aleq-%d]: different ordering gives different semantics\n", trial)); next
  }

  # all.equal should ideally return TRUE (if simplification is confluent for this case)
  eq_result = tryCatch(all.equal(f1, f2), error = function(e) e)
  if (inherits(eq_result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [aleq-%d]: %s\n", trial, eq_result$message))
  }
  # We don't fail on all.equal returning non-TRUE since non-confluence is known
}
cat(sprintf("  all.equal equivalence: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: all.equal for TRUE/FALSE formulas ===
cat("\n=== all.equal TRUE/FALSE ===\n")
set.seed(228002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  n_tests = n_tests + 1

  # TRUE formula comparisons
  f_true = as.CnfFormula(TRUE)
  eq1 = tryCatch(all.equal(f_true, f_true), error = function(e) e)
  if (inherits(eq1, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [tf-%d-1]: %s\n", trial, eq1$message)); next
  }
  if (!isTRUE(eq1)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [tf-%d-1]: TRUE != TRUE\n", trial)); next
  }

  # FALSE formula comparisons
  f_false = as.CnfFormula(FALSE)
  eq2 = tryCatch(all.equal(f_false, f_false), error = function(e) e)
  if (inherits(eq2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [tf-%d-2]: %s\n", trial, eq2$message)); next
  }
  if (!isTRUE(eq2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [tf-%d-2]: FALSE != FALSE\n", trial)); next
  }

  # TRUE vs FALSE
  eq3 = tryCatch(all.equal(f_true, f_false), error = function(e) e)
  if (inherits(eq3, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [tf-%d-3]: %s\n", trial, eq3$message)); next
  }
  if (isTRUE(eq3)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [tf-%d-3]: TRUE == FALSE\n", trial)); next
  }

  # Build formula that simplifies to TRUE
  taut_clauses = list(as.CnfClause(A %among% dom | B %among% dom))
  f_taut = tryCatch(CnfFormula(taut_clauses), error = function(e) NULL)
  # Should have simplified to TRUE
  if (!is.null(f_taut) && isTRUE(unclass(f_taut))) {
    eq4 = tryCatch(all.equal(f_taut, f_true), error = function(e) e)
    if (inherits(eq4, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [tf-%d-4]: %s\n", trial, eq4$message))
    } else if (!isTRUE(eq4)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [tf-%d-4]: tautology != TRUE\n", trial))
    }
  }
}
cat(sprintf("  all.equal TRUE/FALSE: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: as.list roundtrip ===
cat("\n=== as.list roundtrip ===\n")
set.seed(228003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  n_vars = sample(2:4, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:min(3, n_vars), 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1

  # Roundtrip through as.list
  cl_list = as.list(f)
  f2 = tryCatch({
    if (length(cl_list) == 0) as.CnfFormula(TRUE)
    else CnfFormula(cl_list)
  }, error = function(e) e)

  if (inherits(f2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [round-%d]: %s\n", trial, f2$message)); next
  }

  t1 = evaluate_formula(f, u)
  t2 = evaluate_formula(f2, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [round-%d]: roundtrip mismatch\n", trial))
  }
}
cat(sprintf("  as.list roundtrip: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Format/print safety on complex formulas ===
cat("\n=== Format/print safety ===\n")
set.seed(228004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  n_vars = sample(2:5, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(1:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:min(4, n_vars), 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 1) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1

  # format and print should not error
  fmt = tryCatch(format(f), error = function(e) e)
  if (inherits(fmt, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [fmt-%d]: %s\n", trial, fmt$message)); next
  }

  pr = tryCatch(capture.output(print(f)), error = function(e) e)
  if (inherits(pr, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [print-%d]: %s\n", trial, pr$message))
  }
}
cat(sprintf("  Format/print: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
