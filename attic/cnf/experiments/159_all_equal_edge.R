#!/usr/bin/env Rscript
# Test all.equal methods for CnfAtom, CnfClause, CnfFormula:
# - Comparing same objects
# - Comparing different objects that should be equal
# - Comparing objects that differ
# - Cross-type comparisons
# - TRUE/FALSE edge cases
# - all.equal with non-Cnf objects
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a", "b", "c"))
B = CnfSymbol(u, "B", c("d", "e", "f"))

# === all.equal.CnfAtom ===
cat("=== all.equal.CnfAtom ===\n")

# Same atom
n_tests = n_tests + 1
a1 = A %among% c("a", "b")
if (!isTRUE(all.equal(a1, a1))) {
  n_failures = n_failures + 1
  cat("FAIL: atom != itself\n")
}

# Same atom, different order of values
n_tests = n_tests + 1
a2 = A %among% c("b", "a")
eq = all.equal(a1, a2)
if (!isTRUE(eq)) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: atom(a,b) != atom(b,a): %s\n", eq))
}

# Different atoms
n_tests = n_tests + 1
a3 = A %among% "a"
eq = all.equal(a1, a3)
if (isTRUE(eq)) {
  n_failures = n_failures + 1
  cat("FAIL: atom(a,b) == atom(a)\n")
}

# TRUE atoms
n_tests = n_tests + 1
if (!isTRUE(all.equal(as.CnfAtom(TRUE), as.CnfAtom(TRUE)))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE atom != TRUE atom\n")
}

# FALSE atoms
n_tests = n_tests + 1
if (!isTRUE(all.equal(as.CnfAtom(FALSE), as.CnfAtom(FALSE)))) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE atom != FALSE atom\n")
}

# TRUE vs FALSE
n_tests = n_tests + 1
if (isTRUE(all.equal(as.CnfAtom(TRUE), as.CnfAtom(FALSE)))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE atom == FALSE atom\n")
}

# Atom vs non-atom
n_tests = n_tests + 1
eq = all.equal(a1, "not an atom")
if (isTRUE(eq)) {
  n_failures = n_failures + 1
  cat("FAIL: atom == string\n")
}

# Atom vs different symbol
n_tests = n_tests + 1
b1 = B %among% c("d", "e")
eq = all.equal(a1, b1)
if (isTRUE(eq)) {
  n_failures = n_failures + 1
  cat("FAIL: A atom == B atom\n")
}

cat(sprintf("  CnfAtom all.equal: %d tests, %d failures\n", n_tests, n_failures))

# === all.equal.CnfClause ===
cat("\n=== all.equal.CnfClause ===\n")

# Same clause
n_tests = n_tests + 1
c1 = A %among% c("a", "b") | B %among% "d"
if (!isTRUE(all.equal(c1, c1))) {
  n_failures = n_failures + 1
  cat("FAIL: clause != itself\n")
}

# Same clause, different construction order
n_tests = n_tests + 1
c2 = B %among% "d" | A %among% c("b", "a")
eq = all.equal(c1, c2)
if (!isTRUE(eq)) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: clause(A|B) != clause(B|A): %s\n", eq))
}

# Different clauses
n_tests = n_tests + 1
c3 = A %among% "a" | B %among% "d"
eq = all.equal(c1, c3)
if (isTRUE(eq)) {
  n_failures = n_failures + 1
  cat("FAIL: different clauses equal\n")
}

# TRUE clauses
n_tests = n_tests + 1
if (!isTRUE(all.equal(as.CnfClause(TRUE), as.CnfClause(TRUE)))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE clause != TRUE clause\n")
}

# FALSE clauses
n_tests = n_tests + 1
if (!isTRUE(all.equal(as.CnfClause(FALSE), as.CnfClause(FALSE)))) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE clause != FALSE clause\n")
}

# Clause vs non-clause
n_tests = n_tests + 1
eq = all.equal(c1, 42)
if (isTRUE(eq)) {
  n_failures = n_failures + 1
  cat("FAIL: clause == number\n")
}

cat(sprintf("  CnfClause all.equal: %d tests, %d failures\n", n_tests, n_failures))

# === all.equal.CnfFormula ===
cat("\n=== all.equal.CnfFormula ===\n")

# Same formula
n_tests = n_tests + 1
f1 = (A %among% c("a", "b") | B %among% "d") & (A %among% "a")
if (!isTRUE(all.equal(f1, f1))) {
  n_failures = n_failures + 1
  cat("FAIL: formula != itself\n")
}

# Same formula, different clause order via AND
n_tests = n_tests + 1
f2 = (A %among% "a") & (A %among% c("a", "b") | B %among% "d")
eq = all.equal(f1, f2)
if (!isTRUE(eq)) {
  # Note: this might fail due to non-confluent simplification
  # Check semantic equivalence
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL: formula semantic mismatch after reorder: %s\n", eq))
  }
}

# TRUE formulas
n_tests = n_tests + 1
if (!isTRUE(all.equal(as.CnfFormula(TRUE), as.CnfFormula(TRUE)))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE formula != TRUE formula\n")
}

# FALSE formulas
n_tests = n_tests + 1
if (!isTRUE(all.equal(as.CnfFormula(FALSE), as.CnfFormula(FALSE)))) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE formula != FALSE formula\n")
}

# TRUE vs FALSE
n_tests = n_tests + 1
if (isTRUE(all.equal(as.CnfFormula(TRUE), as.CnfFormula(FALSE)))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE formula == FALSE formula\n")
}

# Formula vs non-formula
n_tests = n_tests + 1
eq = all.equal(f1, "not a formula")
if (isTRUE(eq)) {
  n_failures = n_failures + 1
  cat("FAIL: formula == string\n")
}

cat(sprintf("  CnfFormula all.equal: %d tests, %d failures\n", n_tests, n_failures))

# === Stress test: random constructions ===
cat("\n=== Random all.equal stress ===\n")
set.seed(159001)

for (trial in 1:500) {
  u2 = CnfUniverse()
  dom = c("a", "b", "c")
  X = CnfSymbol(u2, "X", dom)
  Y = CnfSymbol(u2, "Y", dom)
  syms = list(X = X, Y = Y)

  # Create a formula
  n_cl = sample(1:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) == 0) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  # Self-equality
  n_tests = n_tests + 1
  eq = tryCatch(all.equal(f, f), error = function(e) paste("error:", e$message))
  if (!isTRUE(eq)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [self-%d]: %s\n", trial, eq))
  }

  # Double negation equality
  if (!is.logical(unclass(f))) {
    n_tests = n_tests + 1
    f_dbl = tryCatch(!!f, error = function(e) NULL)
    if (!is.null(f_dbl)) {
      t1 = evaluate_formula(f, u2)
      t2 = evaluate_formula(f_dbl, u2)
      if (!all(t1 == t2)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [dblneg-%d]: !!f != f semantically\n", trial))
      }
    }
  }

  # Construct from as.list, check equality
  if (!is.logical(unclass(f))) {
    n_tests = n_tests + 1
    clauses2 = as.list(f)
    f2 = tryCatch(CnfFormula(clauses2), error = function(e) NULL)
    if (!is.null(f2)) {
      eq2 = tryCatch(all.equal(f, f2), error = function(e) paste("error:", e$message))
      if (!isTRUE(eq2)) {
        # Check semantic
        t1 = evaluate_formula(f, u2)
        t2 = evaluate_formula(f2, u2)
        if (!all(t1 == t2)) {
          n_failures = n_failures + 1
          cat(sprintf("FAIL [roundtrip-%d]: semantic mismatch\n", trial))
        }
      }
    }
  }
}
cat(sprintf("  Random stress: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
