#!/usr/bin/env Rscript
# Test all.equal.CnfFormula and related comparison operations
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

set.seed(11235)

cat("=== all.equal: identical formulas ===\n")
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("d", "e", "f"))
Z = CnfSymbol(u, "Z", c("g", "h", "i"))

# Test 1: Same formula should be equal
f1 = (X %among% c("a", "b") | Y %among% "d") & Z %among% c("g", "h")
n_tests = n_tests + 1
if (!isTRUE(all.equal(f1, f1))) {
  n_failures = n_failures + 1
  cat("  FAIL: Formula not equal to itself\n")
}

# Test 2: Reconstructed formula should be equal
f2 = (X %among% c("a", "b") | Y %among% "d") & Z %among% c("g", "h")
n_tests = n_tests + 1
if (!isTRUE(all.equal(f1, f2))) {
  n_failures = n_failures + 1
  cat("  FAIL: Reconstructed formula not equal\n")
}

# Test 3: Different clause order should be equal (simplifier normalizes)
f3 = Z %among% c("g", "h") & (X %among% c("a", "b") | Y %among% "d")
n_tests = n_tests + 1
if (!isTRUE(all.equal(f1, f3))) {
  n_failures = n_failures + 1
  cat("  FAIL: Different clause order not recognized as equal\n")
}

# Test 4: Different atom order within clause should be equal
f4 = (Y %among% "d" | X %among% c("a", "b")) & Z %among% c("g", "h")
n_tests = n_tests + 1
if (!isTRUE(all.equal(f1, f4))) {
  n_failures = n_failures + 1
  cat("  FAIL: Different atom order not recognized as equal\n")
}

# Test 5: TRUE formulas
n_tests = n_tests + 1
if (!isTRUE(all.equal(as.CnfFormula(TRUE), as.CnfFormula(TRUE)))) {
  n_failures = n_failures + 1
  cat("  FAIL: TRUE formulas not equal\n")
}

# Test 6: FALSE formulas
n_tests = n_tests + 1
f_false1 = X %among% "a" & X %among% "b"
f_false2 = Y %among% "d" & Y %among% "e"
if (!isTRUE(all.equal(f_false1, f_false2))) {
  n_failures = n_failures + 1
  cat("  FAIL: FALSE formulas not equal\n")
}

# Test 7: TRUE vs FALSE should not be equal
n_tests = n_tests + 1
result = all.equal(as.CnfFormula(TRUE), as.CnfFormula(FALSE))
if (isTRUE(result)) {
  n_failures = n_failures + 1
  cat("  FAIL: TRUE and FALSE formulas incorrectly equal\n")
}

# Test 8: Formula vs TRUE should not be equal
n_tests = n_tests + 1
result = all.equal(f1, as.CnfFormula(TRUE))
if (isTRUE(result)) {
  n_failures = n_failures + 1
  cat("  FAIL: Non-trivial formula incorrectly equal to TRUE\n")
}

# Test 9: Different formulas should not be equal
n_tests = n_tests + 1
f_diff = X %among% "a" & Y %among% "d"
result = all.equal(f1, f_diff)
if (isTRUE(result)) {
  n_failures = n_failures + 1
  cat("  FAIL: Different formulas incorrectly equal\n")
}

cat(sprintf("  all.equal basic: %d tests, %d failures\n", n_tests, n_failures))

# Test 10: Random formula equality (build same formula two different ways)
cat("\n=== all.equal: random reconstructions ===\n")
for (trial in 1:200) {
  u2 = CnfUniverse()
  A = CnfSymbol(u2, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u2, "B", c("b1", "b2"))
  C = CnfSymbol(u2, "C", c("c1", "c2", "c3"))
  syms = list(A = A, B = B, C = C)

  # Build random formula
  n_clauses = sample(2:5, 1)
  clause_list = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u2[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f_a = tryCatch(CnfFormula(clause_list), error = function(e) NULL)
  if (is.null(f_a)) next

  # Rebuild with shuffled clause order
  f_b = tryCatch(CnfFormula(sample(clause_list)), error = function(e) NULL)
  if (is.null(f_b)) next

  n_tests = n_tests + 1
  if (!isTRUE(all.equal(f_a, f_b))) {
    n_failures = n_failures + 1
    cat(sprintf("  FAIL [trial %d]: Same clauses in different order not equal\n", trial))
  }
}
cat(sprintf("  Random reconstructions: %d tests, %d failures\n", n_tests, n_failures))

# Test 11: Formulas built with & vs CnfFormula()
cat("\n=== all.equal: & vs CnfFormula() ===\n")
for (trial in 1:200) {
  u3 = CnfUniverse()
  X3 = CnfSymbol(u3, "X", c("x1", "x2", "x3"))
  Y3 = CnfSymbol(u3, "Y", c("y1", "y2"))
  syms = list(X = X3, Y = Y3)

  cl1 = as.CnfClause(X3 %among% sample(c("x1", "x2", "x3"), sample(1:2, 1)))
  cl2 = as.CnfClause(Y3 %among% sample(c("y1", "y2"), 1) | X3 %among% sample(c("x1", "x2", "x3"), sample(1:2, 1)))

  f_and = tryCatch(as.CnfFormula(cl1) & as.CnfFormula(cl2), error = function(e) NULL)
  f_ctor = tryCatch(CnfFormula(list(cl1, cl2)), error = function(e) NULL)
  if (is.null(f_and) || is.null(f_ctor)) next

  n_tests = n_tests + 1
  if (!isTRUE(all.equal(f_and, f_ctor))) {
    n_failures = n_failures + 1
    cat(sprintf("  FAIL [trial %d]: & vs CnfFormula() disagree\n", trial))
    # Verify semantically
    t_and = evaluate_formula(f_and, u3)
    t_ctor = evaluate_formula(f_ctor, u3)
    if (all(t_and == t_ctor)) {
      cat("    (Semantically equivalent but structurally different)\n")
    } else {
      cat("    SEMANTIC MISMATCH!\n")
    }
  }
}
cat(sprintf("  & vs CnfFormula: %d tests, %d failures\n", n_tests, n_failures))

# Test 12: as.list -> CnfFormula roundtrip
cat("\n=== as.list -> CnfFormula roundtrip ===\n")
for (trial in 1:200) {
  u4 = CnfUniverse()
  X4 = CnfSymbol(u4, "X", c("x1", "x2", "x3"))
  Y4 = CnfSymbol(u4, "Y", c("y1", "y2", "y3"))
  Z4 = CnfSymbol(u4, "Z", c("z1", "z2"))
  syms = list(X = X4, Y = Y4, Z = Z4)

  n_clauses = sample(2:5, 1)
  clause_list = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u4[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f_orig = tryCatch(CnfFormula(clause_list), error = function(e) NULL)
  if (is.null(f_orig)) next

  # Roundtrip: as.list -> CnfFormula
  clauses_back = tryCatch(as.list(f_orig), error = function(e) NULL)
  if (is.null(clauses_back)) {
    # This would be a bug if f_orig is not TRUE (as.list(TRUE) = list())
    if (!is.logical(unclass(f_orig))) {
      n_tests = n_tests + 1; n_failures = n_failures + 1
      cat(sprintf("  ERROR [trial %d]: as.list failed on non-logical formula\n", trial))
    }
    next
  }

  f_round = tryCatch(CnfFormula(clauses_back), error = function(e) NULL)
  if (is.null(f_round)) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("  ERROR [trial %d]: CnfFormula(as.list(f)) failed\n", trial))
    next
  }

  n_tests = n_tests + 1

  # Check semantic equivalence
  t_orig = evaluate_formula(f_orig, u4)
  t_round = evaluate_formula(f_round, u4)
  if (!all(t_orig == t_round)) {
    n_failures = n_failures + 1
    cat(sprintf("  FAIL [trial %d]: roundtrip changes semantics\n", trial))
  }
}
cat(sprintf("  Roundtrip: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
