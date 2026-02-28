#!/usr/bin/env Rscript
# Test operations across different universes and cross-type operator interactions
# Also test format/print methods and as.character
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Cross-universe operations should error ===
cat("=== Cross-universe operations ===\n")

u1 = CnfUniverse()
X1 = CnfSymbol(u1, "X", c("a", "b"))
Y1 = CnfSymbol(u1, "Y", c("c", "d"))

u2 = CnfUniverse()
X2 = CnfSymbol(u2, "X", c("a", "b"))
Z2 = CnfSymbol(u2, "Z", c("e", "f"))

# Clause from u1
cl1 = as.CnfClause(X1 %among% "a")
# Clause from u2
cl2 = as.CnfClause(X2 %among% "a")

# Formula from u1
f1 = CnfFormula(list(cl1))
# Formula from u2
f2 = CnfFormula(list(cl2))

# AND of formulas from different universes
n_tests = n_tests + 1
r = tryCatch(f1 & f2, error = function(e) e)
if (!inherits(r, "error")) {
  # If it succeeds, check semantics carefully
  # Actually this might be undefined behavior - the formulas refer to different universes
  # Let's just note whether it errors or succeeds
  cat(sprintf("  f1 & f2 from different universes: succeeded (returned class: %s)\n", paste(class(r), collapse = ",")))
} else {
  cat(sprintf("  f1 & f2 from different universes: errored (%s)\n", r$message))
}

# OR of formulas from different universes
n_tests = n_tests + 1
r = tryCatch(f1 | f2, error = function(e) e)
if (!inherits(r, "error")) {
  cat(sprintf("  f1 | f2 from different universes: succeeded\n"))
} else {
  cat(sprintf("  f1 | f2 from different universes: errored (%s)\n", r$message))
}

# Clause OR from different universes
n_tests = n_tests + 1
r = tryCatch(cl1 | cl2, error = function(e) e)
if (!inherits(r, "error")) {
  cat(sprintf("  cl1 | cl2 from different universes: succeeded (class: %s)\n", paste(class(r), collapse = ",")))
} else {
  cat(sprintf("  cl1 | cl2 from different universes: errored (%s)\n", r$message))
}

cat(sprintf("  Cross-universe: %d tests, %d failures\n", n_tests, n_failures))

# === Cross-type operator dispatch ===
cat("\n=== Cross-type operator dispatch ===\n")

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
B = CnfSymbol(u, "B", c("b1", "b2"))

atom_a = A %among% "a1"
atom_b = B %among% "b1"
clause_a = as.CnfClause(A %among% c("a1", "a2"))
clause_b = as.CnfClause(B %among% "b1")
formula_a = CnfFormula(list(clause_a))
formula_b = CnfFormula(list(clause_b))

# Atom & Clause
n_tests = n_tests + 1
r = tryCatch(atom_a & clause_b, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR atom & clause: %s\n", r$message))
} else {
  # Should produce a CnfFormula
  if (!inherits(r, "CnfFormula")) { n_failures = n_failures + 1; cat("  FAIL: atom & clause should be CnfFormula\n") }
}

# Atom | Clause -> should make a CnfClause
n_tests = n_tests + 1
r = tryCatch(atom_a | clause_b, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR atom | clause: %s\n", r$message))
} else {
  if (!inherits(r, "CnfClause")) { n_failures = n_failures + 1; cat(sprintf("  FAIL: atom | clause should be CnfClause, got %s\n", class(r)[1])) }
}

# Clause & Formula
n_tests = n_tests + 1
r = tryCatch(clause_a & formula_b, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR clause & formula: %s\n", r$message))
} else {
  if (!inherits(r, "CnfFormula")) { n_failures = n_failures + 1; cat("  FAIL: clause & formula should be CnfFormula\n") }
}

# Clause | Formula -> should distribute and return CnfFormula
n_tests = n_tests + 1
r = tryCatch(clause_a | formula_b, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR clause | formula: %s\n", r$message))
} else {
  if (!inherits(r, "CnfFormula")) { n_failures = n_failures + 1; cat("  FAIL: clause | formula should be CnfFormula\n") }
}

# Atom & Formula
n_tests = n_tests + 1
r = tryCatch(atom_a & formula_b, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR atom & formula: %s\n", r$message))
} else {
  if (!inherits(r, "CnfFormula")) { n_failures = n_failures + 1; cat("  FAIL: atom & formula should be CnfFormula\n") }
}

# Atom | Formula -> distribution
n_tests = n_tests + 1
r = tryCatch(atom_a | formula_b, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR atom | formula: %s\n", r$message))
} else {
  if (!inherits(r, "CnfFormula")) { n_failures = n_failures + 1; cat("  FAIL: atom | formula should be CnfFormula\n") }
}

# Formula & Atom
n_tests = n_tests + 1
r = tryCatch(formula_a & atom_b, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR formula & atom: %s\n", r$message))
}

# Formula | Atom
n_tests = n_tests + 1
r = tryCatch(formula_a | atom_b, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR formula | atom: %s\n", r$message))
}

# Formula | Clause
n_tests = n_tests + 1
r = tryCatch(formula_a | clause_b, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR formula | clause: %s\n", r$message))
}

# Formula & Clause
n_tests = n_tests + 1
r = tryCatch(formula_a & clause_b, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR formula & clause: %s\n", r$message))
}

cat(sprintf("  Cross-type dispatch: %d tests, %d failures\n", n_tests, n_failures))

# === Verify semantic correctness of cross-type operations ===
cat("\n=== Cross-type semantic verification ===\n")
set.seed(38001)

for (trial in 1:200) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("x1", "x2", "x3"))
  Y = CnfSymbol(u, "Y", c("y1", "y2"))
  Z = CnfSymbol(u, "Z", c("z1", "z2"))

  atom = X %among% sample(c("x1", "x2", "x3"), sample(1:2, 1))
  clause = as.CnfClause(Y %among% sample(c("y1", "y2"), 1) | Z %among% sample(c("z1", "z2"), 1))
  formula_f = CnfFormula(list(
    as.CnfClause(X %among% sample(c("x1", "x2", "x3"), sample(1:2, 1))),
    as.CnfClause(Y %among% sample(c("y1", "y2"), 1))
  ))

  # Test: atom & clause = as.CnfFormula(atom) & as.CnfFormula(clause)
  r1 = tryCatch(atom & clause, error = function(e) NULL)
  r2 = tryCatch(as.CnfFormula(as.CnfClause(atom)) & as.CnfFormula(clause), error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    n_tests = n_tests + 1
    t1 = evaluate_formula(r1, u)
    t2 = evaluate_formula(r2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [cross-type trial %d]: atom & clause != explicit conversion\n", trial))
    }
  }

  # Test: atom | formula = as.CnfFormula(atom) | formula
  r1 = tryCatch(atom | formula_f, error = function(e) NULL)
  r2 = tryCatch(as.CnfFormula(as.CnfClause(atom)) | formula_f, error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    n_tests = n_tests + 1
    t1 = evaluate_formula(r1, u)
    t2 = evaluate_formula(r2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [cross-type trial %d]: atom | formula != explicit conversion\n", trial))
    }
  }

  # Test: clause | formula = as.CnfFormula(clause) | formula
  r1 = tryCatch(clause | formula_f, error = function(e) NULL)
  r2 = tryCatch(as.CnfFormula(clause) | formula_f, error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    n_tests = n_tests + 1
    t1 = evaluate_formula(r1, u)
    t2 = evaluate_formula(r2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [cross-type trial %d]: clause | formula != explicit conversion\n", trial))
    }
  }
}
cat(sprintf("  Cross-type semantic: %d tests, %d failures\n", n_tests, n_failures))

# === format/print methods ===
cat("\n=== Format and print methods ===\n")

u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("d", "e"))

# Atom format
n_tests = n_tests + 1
r = tryCatch(format(X %among% c("a", "b")), error = function(e) e)
if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR format atom: %s\n", r$message)) }

# Clause format
n_tests = n_tests + 1
r = tryCatch(format(as.CnfClause(X %among% "a" | Y %among% "d")), error = function(e) e)
if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR format clause: %s\n", r$message)) }

# TRUE clause format
n_tests = n_tests + 1
r = tryCatch(format(as.CnfClause(TRUE)), error = function(e) e)
if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR format TRUE clause: %s\n", r$message)) }

# FALSE clause format
n_tests = n_tests + 1
r = tryCatch(format(as.CnfClause(FALSE)), error = function(e) e)
if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR format FALSE clause: %s\n", r$message)) }

# Formula format
n_tests = n_tests + 1
f = CnfFormula(list(as.CnfClause(X %among% "a"), as.CnfClause(Y %among% "d")))
r = tryCatch(format(f), error = function(e) e)
if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR format formula: %s\n", r$message)) }

# TRUE formula format
n_tests = n_tests + 1
r = tryCatch(format(as.CnfFormula(TRUE)), error = function(e) e)
if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR format TRUE formula: %s\n", r$message)) }

# FALSE formula format
n_tests = n_tests + 1
r = tryCatch(format(as.CnfFormula(FALSE)), error = function(e) e)
if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR format FALSE formula: %s\n", r$message)) }

# print methods (just check they don't error)
n_tests = n_tests + 1
r = tryCatch(capture.output(print(X %among% "a")), error = function(e) e)
if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR print atom: %s\n", r$message)) }

n_tests = n_tests + 1
r = tryCatch(capture.output(print(as.CnfClause(X %among% "a" | Y %among% "d"))), error = function(e) e)
if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR print clause: %s\n", r$message)) }

n_tests = n_tests + 1
r = tryCatch(capture.output(print(f)), error = function(e) e)
if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR print formula: %s\n", r$message)) }

cat(sprintf("  Format/print: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
