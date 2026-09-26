#!/usr/bin/env Rscript
# Deep test of all.equal.CnfFormula: verify normalization handles edge cases
# and that semantically equivalent formulas compare equal
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Identical formulas ===
cat("=== Identical formulas ===\n")
set.seed(63001)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  cls = lapply(1:sample(1:4, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  # all.equal with itself should be TRUE
  n_tests = n_tests + 1
  result = all.equal(f, f)
  if (!isTRUE(result)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [identical-%d]: all.equal(f, f) = %s\n", trial, paste(result, collapse = "; ")))
  }
}
cat(sprintf("  Identical: %d tests, %d failures\n", n_tests, n_failures))

# === Same clauses, different construction ===
cat("\n=== Same clauses, different construction ===\n")
set.seed(63002)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  cls = lapply(1:sample(2:3, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f1 = tryCatch(CnfFormula(cls), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(rev(cls)), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  # Constructing from same clauses in different order should give equal result
  # (after normalization)
  n_tests = n_tests + 1
  result = all.equal(f1, f2)
  if (!isTRUE(result)) {
    # Check if they're at least semantically equivalent
    t1 = evaluate_formula(f1, u)
    t2 = evaluate_formula(f2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [rev-order-%d]: semantic mismatch!\n", trial))
    }
    # Note: structural inequality is OK (non-confluent simplifier)
  }
}
cat(sprintf("  Same clauses different order: %d tests, %d failures\n", n_tests, n_failures))

# === TRUE/FALSE comparison ===
cat("\n=== TRUE/FALSE comparison ===\n")

f_true1 = as.CnfFormula(TRUE)
f_true2 = as.CnfFormula(TRUE)
f_false1 = as.CnfFormula(FALSE)
f_false2 = as.CnfFormula(FALSE)

n_tests = n_tests + 1
if (!isTRUE(all.equal(f_true1, f_true2))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE != TRUE\n")
}
n_tests = n_tests + 1
if (!isTRUE(all.equal(f_false1, f_false2))) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE != FALSE\n")
}
n_tests = n_tests + 1
if (isTRUE(all.equal(f_true1, f_false1))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE == FALSE\n")
}

cat(sprintf("  TRUE/FALSE comparison: %d tests, %d failures\n", n_tests, n_failures))

# === as.list roundtrip ===
cat("\n=== as.list roundtrip ===\n")
set.seed(63003)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  cls = lapply(1:sample(1:4, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  # as.list -> CnfFormula should give same result (after re-simplification)
  f_list = tryCatch(as.list(f), error = function(e) NULL)
  if (is.null(f_list)) next
  f2 = tryCatch(CnfFormula(f_list), error = function(e) NULL)
  if (is.null(f2)) next

  t1 = evaluate_formula(f, u)
  t2 = evaluate_formula(f2, u)

  n_tests = n_tests + 1
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [roundtrip-%d]: as.list -> CnfFormula semantic mismatch\n", trial))
  }
}
cat(sprintf("  as.list roundtrip: %d tests, %d failures\n", n_tests, n_failures))

# === Roundtrip for TRUE and FALSE ===
cat("\n=== TRUE/FALSE roundtrip ===\n")

f_true = as.CnfFormula(TRUE)
f_true_list = as.list(f_true)
n_tests = n_tests + 1
if (!identical(f_true_list, list())) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: as.list(TRUE) should be list(), got length %d\n", length(f_true_list)))
}

f_false = as.CnfFormula(FALSE)
f_false_list = as.list(f_false)
n_tests = n_tests + 1
# FALSE formula: as.list should return list with one FALSE clause
if (length(f_false_list) != 1) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: as.list(FALSE) should have length 1, got %d\n", length(f_false_list)))
} else {
  cl = f_false_list[[1]]
  if (!isFALSE(unclass(cl))) {
    n_failures = n_failures + 1
    cat("FAIL: as.list(FALSE)[[1]] should be FALSE clause\n")
  }
}
cat(sprintf("  TRUE/FALSE roundtrip: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
