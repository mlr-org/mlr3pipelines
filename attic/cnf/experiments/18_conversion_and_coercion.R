source("experiments/test_harness.R")

# Test conversion and coercion methods:
# as.CnfFormula, as.CnfClause, as.CnfAtom
# Also test interactions between different types in operators

n_failures = 0
n_tests = 0
errors = character(0)

report = function(test_name, ok, msg = "") {
  n_tests <<- n_tests + 1
  if (ok) {
    # cat(sprintf("  PASS: %s\n", test_name))
  } else {
    n_failures <<- n_failures + 1
    cat(sprintf("  FAIL: %s -- %s\n", test_name, msg))
    errors <<- c(errors, paste(test_name, msg, sep = ": "))
  }
}

u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("d", "e", "f"))

cat("=== as.CnfFormula conversions ===\n")

# From logical
report("as.CnfFormula(TRUE)", inherits(as.CnfFormula(TRUE), "CnfFormula"))
report("as.CnfFormula(FALSE)", inherits(as.CnfFormula(FALSE), "CnfFormula"))
report("as.CnfFormula(TRUE) is TRUE", isTRUE(as.logical(as.CnfFormula(TRUE))))
report("as.CnfFormula(FALSE) is FALSE", isFALSE(as.logical(as.CnfFormula(FALSE))))

# From CnfAtom
atom = X %among% c("a", "b")
f_from_atom = as.CnfFormula(atom)
report("as.CnfFormula(atom)", inherits(f_from_atom, "CnfFormula"))

# TRUE atom -> formula
atom_T = X %among% c("a", "b", "c")
f_from_T_atom = as.CnfFormula(atom_T)
report("TRUE atom -> formula is TRUE", isTRUE(as.logical(f_from_T_atom)))

# FALSE atom -> formula
atom_F = X %among% character(0)
f_from_F_atom = as.CnfFormula(atom_F)
report("FALSE atom -> formula is FALSE", isFALSE(as.logical(f_from_F_atom)))

# From CnfClause
clause = X %among% "a" | Y %among% "d"
f_from_clause = as.CnfFormula(clause)
report("as.CnfFormula(clause)", inherits(f_from_clause, "CnfFormula"))

# TRUE clause -> formula
clause_T = as.CnfClause(TRUE)
f_from_T_clause = as.CnfFormula(clause_T)
report("TRUE clause -> formula is TRUE", isTRUE(as.logical(f_from_T_clause)))

# FALSE clause -> formula
clause_F = as.CnfClause(FALSE)
f_from_F_clause = as.CnfFormula(clause_F)
report("FALSE clause -> formula is FALSE", isFALSE(as.logical(f_from_F_clause)))

# From CnfFormula
formula = (X %among% "a" | Y %among% "d") & X %among% c("a", "b")
f_from_f = as.CnfFormula(formula)
report("as.CnfFormula(formula) identity", identical(formula, f_from_f))

cat("\n=== as.CnfClause conversions ===\n")

# From logical
report("as.CnfClause(TRUE)", isTRUE(as.logical(as.CnfClause(TRUE))))
report("as.CnfClause(FALSE)", isFALSE(as.logical(as.CnfClause(FALSE))))

# From CnfAtom
clause_from_atom = as.CnfClause(X %among% "a")
report("as.CnfClause(atom)", inherits(clause_from_atom, "CnfClause"))
report("as.CnfClause(atom) preserves semantics",
  identical(unclass(clause_from_atom)[["X"]], "a"))

# TRUE atom -> clause
clause_from_T_atom = as.CnfClause(X %among% c("a", "b", "c"))
report("TRUE atom -> clause is TRUE", isTRUE(as.logical(clause_from_T_atom)))

# FALSE atom -> clause
clause_from_F_atom = as.CnfClause(X %among% character(0))
report("FALSE atom -> clause is FALSE", isFALSE(as.logical(clause_from_F_atom)))

cat("\n=== Cross-type operations ===\n")

# Atom & Atom -> Formula
f1 = X %among% "a" & Y %among% "d"
report("atom & atom -> CnfFormula", inherits(f1, "CnfFormula"))

# Atom | Atom -> Clause (if same universe)
c1 = X %among% "a" | Y %among% "d"
report("atom | atom -> CnfClause", inherits(c1, "CnfClause"))

# Clause & Clause -> Formula
f2 = (X %among% "a" | Y %among% "d") & (X %among% "b" | Y %among% "e")
report("clause & clause -> CnfFormula", inherits(f2, "CnfFormula"))

# Formula | Formula -> Formula
f3 = tryCatch(f1 | f2, error = function(e) e)
report("formula | formula -> CnfFormula", inherits(f3, "CnfFormula"))

# Atom & Formula
f4 = X %among% "a" & f2
report("atom & formula -> CnfFormula", inherits(f4, "CnfFormula"))

# Formula & Atom
f5 = f2 & X %among% "a"
report("formula & atom -> CnfFormula", inherits(f5, "CnfFormula"))

# Atom | Formula
f6 = tryCatch(X %among% "a" | f2, error = function(e) e)
report("atom | formula works", inherits(f6, "CnfFormula"))

# Clause | Formula
f7 = tryCatch(c1 | f2, error = function(e) e)
report("clause | formula works", inherits(f7, "CnfFormula"))

# Formula | Clause
f8 = tryCatch(f2 | c1, error = function(e) e)
report("formula | clause works", inherits(f8, "CnfFormula"))

# Check semantic equivalence of atom | formula vs formula | atom
if (inherits(f6, "CnfFormula") && inherits(f8, "CnfFormula")) {
  # These are different computations but let's check the general pattern
  f6b = tryCatch(f2 | X %among% "a", error = function(e) e)
  if (inherits(f6b, "CnfFormula")) {
    check_equiv = function(fa, fb, label) {
      n_tests <<- n_tests + 1
      ta = evaluate_formula(fa, u)
      tb = evaluate_formula(fb, u)
      if (!all(ta == tb)) {
        n_failures <<- n_failures + 1
        cat(sprintf("FAIL: %s\n", label))
      }
    }
    check_equiv(f6, f6b, "atom|formula == formula|atom")
  }
}

cat("\n=== CnfFormula constructor edge cases ===\n")

# CnfFormula with mixed CnfClause and CnfFormula in list
inner_f = (X %among% "a" | Y %among% "d") & X %among% c("a", "b")
outer_clause = as.CnfClause(Y %among% c("d", "e"))
combined = tryCatch(CnfFormula(list(inner_f, outer_clause)), error = function(e) e)
report("CnfFormula with CnfFormula + CnfClause in list", inherits(combined, "CnfFormula"))

# Check that CnfFormula(list(formula)) flattens the inner formula's clauses
# The result should be equivalent to formula & clause
if (inherits(combined, "CnfFormula")) {
  expected = inner_f & as.CnfFormula(outer_clause)
  n_tests <<- n_tests + 1
  tc = evaluate_formula(combined, u)
  te = evaluate_formula(expected, u)
  if (!all(tc == te)) {
    n_failures <<- n_failures + 1
    cat("FAIL: CnfFormula(list(formula, clause)) semantics\n")
  }
}

# CnfFormula with only TRUE clauses
f_all_T = tryCatch(CnfFormula(list(as.CnfClause(TRUE), as.CnfClause(TRUE))), error = function(e) e)
report("CnfFormula(list(TRUE, TRUE)) is TRUE", !inherits(f_all_T, "error") && isTRUE(as.logical(f_all_T)))

# CnfFormula with one FALSE clause
f_has_F = tryCatch(CnfFormula(list(as.CnfClause(X %among% "a"), as.CnfClause(FALSE))), error = function(e) e)
report("CnfFormula with FALSE clause is FALSE", !inherits(f_has_F, "error") && isFALSE(as.logical(f_has_F)))

# CnfFormula with single clause
f_single = CnfFormula(list(as.CnfClause(X %among% c("a", "b"))))
report("CnfFormula single clause", inherits(f_single, "CnfFormula"))
f_single_list = as.list(f_single)
report("CnfFormula single clause has 1 element", length(f_single_list) == 1)

cat("\n=== Negation edge cases ===\n")

# ! on TRUE formula
neg_T = !as.CnfFormula(TRUE)
report("!TRUE is FALSE", isFALSE(as.logical(neg_T)))

# ! on FALSE formula
neg_F = !as.CnfFormula(FALSE)
report("!FALSE is TRUE", isTRUE(as.logical(neg_F)))

# ! on single-clause formula
f_sc = as.CnfFormula(as.CnfClause(X %among% "a" | Y %among% "d"))
neg_sc = !f_sc
# !(X in a | Y in d) = (X not in a) & (Y not in d) = (X in {b,c}) & (Y in {e,f})
n_tests <<- n_tests + 1
truth_neg = evaluate_formula(neg_sc, u)
truth_orig = evaluate_formula(f_sc, u)
if (!all(truth_neg == !truth_orig)) {
  n_failures <<- n_failures + 1
  cat("FAIL: negation of single clause\n")
}

# ! on multi-clause formula (De Morgan)
f_mc = (X %among% "a" | Y %among% "d") & (X %among% c("b", "c"))
neg_mc = tryCatch(!f_mc, error = function(e) e)
if (!inherits(neg_mc, "error")) {
  n_tests <<- n_tests + 1
  truth_neg = evaluate_formula(neg_mc, u)
  truth_orig = evaluate_formula(f_mc, u)
  if (!all(truth_neg == !truth_orig)) {
    n_failures <<- n_failures + 1
    cat("FAIL: negation of multi-clause formula\n")
  }
}

cat(sprintf("\n=== Summary ===\n"))
if (length(errors)) {
  cat(sprintf("FAILURES (%d):\n", length(errors)))
  for (e in errors) cat(sprintf("  - %s\n", e))
}
cat(sprintf("Total: %d tests, %d failures\n", n_tests, n_failures))
