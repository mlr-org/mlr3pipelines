#!/usr/bin/env Rscript
# Deep dive into constructor edge cases for all Cnf types,
# especially focusing on universe handling.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== CnfUniverse edge cases ===\n")

# Duplicate symbol registration
n_tests = n_tests + 1
u = CnfUniverse()
CnfSymbol(u, "A", c("a", "b"))
r = tryCatch(CnfSymbol(u, "A", c("a", "b")), error = function(e) "expected")
if (!identical(r, "expected")) {
  n_failures = n_failures + 1
  cat("FAIL: duplicate symbol should error\n")
}

# length and names
n_tests = n_tests + 1
if (length(u) != 1) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: length(u) should be 1, got %d\n", length(u)))
}

n_tests = n_tests + 1
CnfSymbol(u, "B", c("x", "y"))
if (!setequal(ls(u), c("A", "B"))) {
  n_failures = n_failures + 1
  cat("FAIL: ls(u) should show A and B\n")
}

# $NonExistent should error
n_tests = n_tests + 1
r = tryCatch(u$NonExistent, error = function(e) "expected")
if (!identical(r, "expected")) {
  n_failures = n_failures + 1
  cat("FAIL: u$NonExistent should error\n")
}

# print and format
n_tests = n_tests + 1
r = tryCatch({ capture.output(print(u)); TRUE }, error = function(e) FALSE)
if (!r) {
  n_failures = n_failures + 1
  cat("FAIL: print(u) should not error\n")
}

n_tests = n_tests + 1
r = tryCatch(format(u), error = function(e) NULL)
if (is.null(r)) {
  n_failures = n_failures + 1
  cat("FAIL: format(u) should not error\n")
}

# Empty universe
n_tests = n_tests + 1
u_empty = CnfUniverse()
r = tryCatch({ capture.output(print(u_empty)); TRUE }, error = function(e) FALSE)
if (!r) {
  n_failures = n_failures + 1
  cat("FAIL: print(empty universe) should not error\n")
}

cat(sprintf("  CnfUniverse: %d tests, %d failures\n", n_tests, n_failures))

# === CnfSymbol edge cases ===
cat("\n=== CnfSymbol edge cases ===\n")

# Single-value domain
u2 = CnfUniverse()
n_tests = n_tests + 1
r = tryCatch(CnfSymbol(u2, "X", "only_val"), error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: single-value domain should work: %s\n", r$message))
}

# Large domain
n_tests = n_tests + 1
r = tryCatch(CnfSymbol(u2, "Y", paste0("v", 1:100)), error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: large domain should work: %s\n", r$message))
}

# Empty domain should fail
n_tests = n_tests + 1
r = tryCatch(CnfSymbol(u2, "Z", character(0)), error = function(e) "expected")
if (!identical(r, "expected")) {
  n_failures = n_failures + 1
  cat("FAIL: empty domain should error\n")
}

# print and format
n_tests = n_tests + 1
X = u2$X
r = tryCatch({ capture.output(print(X)); TRUE }, error = function(e) FALSE)
if (!r) {
  n_failures = n_failures + 1
  cat("FAIL: print(symbol) should not error\n")
}

n_tests = n_tests + 1
r = tryCatch(format(X), error = function(e) NULL)
if (is.null(r)) {
  n_failures = n_failures + 1
  cat("FAIL: format(symbol) should not error\n")
}

cat(sprintf("  CnfSymbol: %d tests, %d failures\n", n_tests, n_failures))

# === CnfAtom edge cases ===
cat("\n=== CnfAtom edge cases ===\n")

u3 = CnfUniverse()
A = CnfSymbol(u3, "A", c("a", "b", "c"))

# as.CnfAtom on non-atom
n_tests = n_tests + 1
r = tryCatch(as.CnfAtom("not_an_atom"), error = function(e) "expected")
if (!identical(r, "expected")) {
  n_failures = n_failures + 1
  cat("FAIL: as.CnfAtom(string) should error\n")
}

# Values not in domain
n_tests = n_tests + 1
r = tryCatch(A %among% c("a", "z"), error = function(e) "expected")
if (!identical(r, "expected")) {
  n_failures = n_failures + 1
  cat("FAIL: out-of-domain values should error\n")
}

# Atom print/format
n_tests = n_tests + 1
atom_normal = A %among% c("a", "b")
r = tryCatch({ capture.output(print(atom_normal)); TRUE }, error = function(e) FALSE)
if (!r) {
  n_failures = n_failures + 1
  cat("FAIL: print(normal atom) should not error\n")
}

n_tests = n_tests + 1
atom_true = A %among% c("a", "b", "c")
r = tryCatch({ capture.output(print(atom_true)); TRUE }, error = function(e) FALSE)
if (!r) {
  n_failures = n_failures + 1
  cat("FAIL: print(TRUE atom) should not error\n")
}

n_tests = n_tests + 1
atom_false = A %among% character(0)
r = tryCatch({ capture.output(print(atom_false)); TRUE }, error = function(e) FALSE)
if (!r) {
  n_failures = n_failures + 1
  cat("FAIL: print(FALSE atom) should not error\n")
}

# as.logical
n_tests = n_tests + 1
if (!isTRUE(as.logical(atom_true))) {
  n_failures = n_failures + 1
  cat("FAIL: as.logical(TRUE atom) should be TRUE\n")
}

n_tests = n_tests + 1
if (!isFALSE(as.logical(atom_false))) {
  n_failures = n_failures + 1
  cat("FAIL: as.logical(FALSE atom) should be FALSE\n")
}

n_tests = n_tests + 1
if (!is.na(as.logical(atom_normal))) {
  n_failures = n_failures + 1
  cat("FAIL: as.logical(normal atom) should be NA\n")
}

# all.equal
n_tests = n_tests + 1
atom2 = A %among% c("b", "a")  # same values, different order
if (!isTRUE(all.equal(atom_normal, atom2))) {
  n_failures = n_failures + 1
  cat("FAIL: all.equal should handle reordered values\n")
}

n_tests = n_tests + 1
atom3 = A %among% "a"  # different values
if (isTRUE(all.equal(atom_normal, atom3))) {
  n_failures = n_failures + 1
  cat("FAIL: different atoms should not be equal\n")
}

# all.equal for TRUE/FALSE atoms
n_tests = n_tests + 1
if (!isTRUE(all.equal(as.CnfAtom(TRUE), as.CnfAtom(TRUE)))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE atoms should be equal\n")
}

n_tests = n_tests + 1
if (isTRUE(all.equal(as.CnfAtom(TRUE), as.CnfAtom(FALSE)))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE != FALSE atoms\n")
}

cat(sprintf("  CnfAtom: %d tests, %d failures\n", n_tests, n_failures))

# === CnfFormula as.logical edge cases ===
cat("\n=== CnfFormula as.logical ===\n")

u4 = CnfUniverse()
X = CnfSymbol(u4, "X", c("a", "b", "c"))

# TRUE formula
n_tests = n_tests + 1
f_true = as.CnfFormula(TRUE)
if (!isTRUE(as.logical(f_true))) {
  n_failures = n_failures + 1
  cat("FAIL: as.logical(TRUE formula) should be TRUE\n")
}

# FALSE formula
n_tests = n_tests + 1
f_false = as.CnfFormula(FALSE)
if (!isFALSE(as.logical(f_false))) {
  n_failures = n_failures + 1
  cat("FAIL: as.logical(FALSE formula) should be FALSE\n")
}

# Normal formula
n_tests = n_tests + 1
f_norm = CnfFormula(list(as.CnfClause(X %among% c("a", "b"))))
r = as.logical(f_norm)
if (!is.na(r)) {
  n_failures = n_failures + 1
  cat("FAIL: as.logical(normal formula) should be NA\n")
}

# Tautological formula (all values)
n_tests = n_tests + 1
f_taut = CnfFormula(list(as.CnfClause(X %among% c("a", "b", "c"))))
if (!isTRUE(as.logical(f_taut))) {
  n_failures = n_failures + 1
  cat("FAIL: as.logical(tautological formula) should be TRUE\n")
}

# Contradictory formula
n_tests = n_tests + 1
f_contr = CnfFormula(list(
  as.CnfClause(X %among% "a"),
  as.CnfClause(X %among% "b")
))
if (!isFALSE(as.logical(f_contr))) {
  n_failures = n_failures + 1
  cat("FAIL: as.logical(contradictory formula) should be FALSE\n")
}

cat(sprintf("  CnfFormula as.logical: %d tests, %d failures\n", n_tests, n_failures))

# === Cross-universe operations ===
cat("\n=== Cross-universe operations ===\n")

u5 = CnfUniverse()
u6 = CnfUniverse()
P = CnfSymbol(u5, "P", c("a", "b"))
Q = CnfSymbol(u6, "Q", c("x", "y"))

# & across universes
n_tests = n_tests + 1
r = tryCatch(
  as.CnfFormula(as.CnfClause(P %among% "a")) & as.CnfFormula(as.CnfClause(Q %among% "x")),
  error = function(e) "expected"
)
if (!identical(r, "expected")) {
  n_failures = n_failures + 1
  cat("FAIL: cross-universe & should error\n")
}

# | across universes
n_tests = n_tests + 1
r = tryCatch(
  as.CnfFormula(as.CnfClause(P %among% "a")) | as.CnfFormula(as.CnfClause(Q %among% "x")),
  error = function(e) "expected"
)
if (!identical(r, "expected")) {
  n_failures = n_failures + 1
  cat("FAIL: cross-universe | should error\n")
}

# CnfClause across universes
n_tests = n_tests + 1
r = tryCatch(
  CnfClause(list(P %among% "a", Q %among% "x")),
  error = function(e) "expected"
)
if (!identical(r, "expected")) {
  n_failures = n_failures + 1
  cat("FAIL: cross-universe clause should error\n")
}

cat(sprintf("  Cross-universe: %d tests, %d failures\n", n_tests, n_failures))

# === chooseOpsMethod ===
cat("\n=== chooseOpsMethod dispatch ===\n")

u7 = CnfUniverse()
W = CnfSymbol(u7, "W", c("a", "b", "c"))

# atom & clause (dispatches to CnfFormula via CnfAtom)
n_tests = n_tests + 1
r = tryCatch(
  (W %among% "a") & as.CnfClause(W %among% c("a", "b")),
  error = function(e) e
)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: atom & clause error: %s\n", r$message))
}

# clause | atom
n_tests = n_tests + 1
r = tryCatch(
  as.CnfClause(W %among% "a") | (W %among% "b"),
  error = function(e) e
)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: clause | atom error: %s\n", r$message))
}

# !atom
n_tests = n_tests + 1
r = tryCatch(!(W %among% "a"), error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: !atom error: %s\n", r$message))
} else {
  if (!identical(sort(r$values), c("b", "c"))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL: !(W %%among%% 'a') should give {b,c}, got %s\n", paste(r$values, collapse=",")))
  }
}

# !clause
n_tests = n_tests + 1
r = tryCatch(!as.CnfClause(W %among% c("a", "b")), error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: !clause error: %s\n", r$message))
} else if (!inherits(r, "CnfFormula")) {
  n_failures = n_failures + 1
  cat("FAIL: !clause should return CnfFormula\n")
}

cat(sprintf("  chooseOpsMethod: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
