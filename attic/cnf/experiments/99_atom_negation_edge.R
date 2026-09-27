#!/usr/bin/env Rscript
# Test CnfAtom negation edge cases:
# - Negation of atom with single value domain
# - Negation produces atom with empty values (domain = 1)
# - Double negation at atom level
# - Negation + OR (atom | !atom should be tautological clause)
# Also test CnfClause [ subsetting edge cases.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Atom negation basics ===
cat("=== Atom negation basics ===\n")

u = CnfUniverse()
dom = c("a", "b", "c")
A = CnfSymbol(u, "A", dom)

# !( A %among% "a") should produce A %among% c("b", "c")
n_tests = n_tests + 1
atom = A %among% "a"
neg_atom = !atom
if (!identical(sort(neg_atom$values), c("b", "c"))) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: !(A %%among%% 'a') values = %s, expected b,c\n", paste(neg_atom$values, collapse = ",")))
}

# !( A %among% c("a","b")) should produce A %among% "c"
n_tests = n_tests + 1
atom2 = A %among% c("a", "b")
neg_atom2 = !atom2
if (!identical(neg_atom2$values, "c")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: !(A %%among%% 'a','b') values = %s, expected c\n", paste(neg_atom2$values, collapse = ",")))
}

# !!A should equal A
n_tests = n_tests + 1
dbl_neg = !!atom
if (!identical(sort(dbl_neg$values), sort(atom$values)) || dbl_neg$symbol != atom$symbol) {
  n_failures = n_failures + 1
  cat("FAIL: !!atom != atom\n")
}

cat(sprintf("  Atom negation basics: %d tests, %d failures\n", n_tests, n_failures))

# === Singleton domain negation ===
cat("\n=== Singleton domain negation ===\n")

u2 = CnfUniverse()
S = CnfSymbol(u2, "S", c("only"))

# S %among% "only" negated should produce empty values
# S %among% "only" with domain c("only") is TAUTOLOGICAL (covers full domain)
# So the atom is TRUE, and its negation is FALSE
n_tests = n_tests + 1
atom_s = S %among% "only"
if (!isTRUE(unclass(atom_s))) {
  n_failures = n_failures + 1
  cat("FAIL: S %among% 'only' with singleton domain should be TRUE atom\n")
}

n_tests = n_tests + 1
neg_s = tryCatch(!atom_s, error = function(e) e)
if (inherits(neg_s, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: negating singleton atom: %s\n", neg_s$message))
} else {
  # Should be FALSE atom
  if (!isFALSE(unclass(neg_s))) {
    n_failures = n_failures + 1
    cat("FAIL: !(TRUE atom) should be FALSE\n")
  }
  # Can we use the FALSE atom?
  n_tests = n_tests + 1
  clause = tryCatch(as.CnfClause(neg_s), error = function(e) e)
  if (inherits(clause, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR: as.CnfClause of FALSE atom: %s\n", clause$message))
  } else {
    if (!isFALSE(unclass(clause))) {
      n_failures = n_failures + 1
      cat("FAIL: CnfClause from FALSE atom should be FALSE\n")
    }
  }
}

# Now test with binary domain: S2 %among% "a" where domain is c("a","b")
u3 = CnfUniverse()
S2 = CnfSymbol(u3, "S2", c("a", "b"))
n_tests = n_tests + 1
atom_s2 = S2 %among% "a"
neg_s2 = !atom_s2
if (!identical(neg_s2$values, "b")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: !(S2 %%among%% 'a') should have values 'b', got %s\n", paste(neg_s2$values, collapse = ",")))
}
cat(sprintf("  Singleton domain: %d tests, %d failures\n", n_tests, n_failures))

# === Atom | !Atom ===
cat("\n=== Atom | !Atom (tautology check) ===\n")
set.seed(99001)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = paste0("v", 1:sample(2:5, 1))
  A = CnfSymbol(u, "A", dom)

  vals = sample(dom, sample(1:(length(dom) - 1), 1))
  atom = A %among% vals
  neg_atom = !atom

  n_tests = n_tests + 1
  clause = tryCatch(atom | neg_atom, error = function(e) e)
  if (inherits(clause, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [taut-%d]: %s\n", trial, clause$message)); next
  }

  # Should be TRUE (tautology)
  if (!isTRUE(unclass(clause))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [taut-%d]: atom | !atom should be TRUE\n", trial))
  }
}
cat(sprintf("  Atom tautology: %d tests, %d failures\n", n_tests, n_failures))

# === CnfClause [ subsetting ===
cat("\n=== CnfClause [ subsetting ===\n")
set.seed(99002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  clause = as.CnfClause(A %among% sample(dom, sample(1:2, 1)) |
                         B %among% sample(dom, sample(1:2, 1)) |
                         C %among% sample(dom, sample(1:2, 1)))
  if (isTRUE(unclass(clause))) next  # skip tautological clauses

  n_tests = n_tests + 1

  # Subsetting by name
  syms = names(clause)
  for (s in syms) {
    sub = tryCatch(clause[s], error = function(e) e)
    if (inherits(sub, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [sub-%d-%s]: %s\n", trial, s, sub$message)); next
    }
    # Result should be a valid CnfClause
    if (!inherits(sub, "CnfClause")) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [sub-%d-%s]: result not CnfClause\n", trial, s))
    }
  }

  # Subsetting by index
  for (i in seq_along(syms)) {
    sub = tryCatch(clause[i], error = function(e) e)
    if (inherits(sub, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [sub-%d-idx%d]: %s\n", trial, i, sub$message)); next
    }
  }

  # Subsetting with multiple indices
  if (length(syms) >= 2) {
    sub = tryCatch(clause[1:2], error = function(e) e)
    if (inherits(sub, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [sub-%d-multi]: %s\n", trial, sub$message)); next
    }
  }

  # Empty subsetting
  sub = tryCatch(clause[integer(0)], error = function(e) e)
  if (inherits(sub, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sub-%d-empty]: %s\n", trial, sub$message))
  }
}
cat(sprintf("  Clause subsetting: %d tests, %d failures\n", n_tests, n_failures))

# === Atom & !Atom (should be FALSE formula) ===
cat("\n=== Atom & !Atom (contradiction) ===\n")
set.seed(99003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = paste0("v", 1:sample(2:5, 1))
  A = CnfSymbol(u, "A", dom)

  vals = sample(dom, sample(1:(length(dom) - 1), 1))
  atom = A %among% vals
  neg_atom = !atom

  n_tests = n_tests + 1
  result = tryCatch(atom & neg_atom, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [contr-%d]: %s\n", trial, result$message)); next
  }

  # Should be FALSE
  if (!isFALSE(as.logical(result))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [contr-%d]: atom & !atom should be FALSE\n", trial))
  }
}
cat(sprintf("  Atom contradiction: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
