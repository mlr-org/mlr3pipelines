source("experiments/test_harness.R")

# Test negation correctness and OR-distribution (|.CnfFormula)
# These operations have exponential blowup potential, so we focus on small cases

errors = character(0)
n_tests = 0
report = function(test_name, ok, msg = "") {
  n_tests <<- n_tests + 1
  if (ok) {
    cat(sprintf("  PASS: %s\n", test_name))
  } else {
    cat(sprintf("  FAIL: %s -- %s\n", test_name, msg))
    errors <<- c(errors, paste(test_name, msg, sep = ": "))
  }
}

u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("d", "e", "f"))
Z = CnfSymbol(u, "Z", c("g", "h", "i"))

cat("=== Negation of single clauses ===\n")

# !(X in {a} | Y in {d}) should be (X in {b,c} & Y in {e,f})
f = !(X %among% "a" | Y %among% "d")
check_neg = function(original, negated, label) {
  truth_orig = evaluate_formula(as.CnfFormula(original), u)
  truth_neg = evaluate_formula(negated, u)
  # negation should flip every assignment
  report(label, all(truth_orig != truth_neg))
}

clause1 = as.CnfFormula(X %among% "a" | Y %among% "d")
check_neg(clause1, !clause1, "Negation of 2-atom clause")

clause2 = as.CnfFormula(X %among% c("a", "b") | Y %among% c("d", "e") | Z %among% "g")
check_neg(clause2, !clause2, "Negation of 3-atom clause")

cat("\n=== Negation of formulas ===\n")

# !(A & B) should be (!A | !B) by De Morgan
f1 = (X %among% "a" | Y %among% "d") & (X %among% "b" | Z %among% "g")
check_neg(f1, !f1, "Negation of 2-clause formula")

f2 = (X %among% "a") & (Y %among% "d") & (Z %among% "g")
check_neg(f2, !f2, "Negation of 3-unit formula")

# Negation of a formula that simplifies
f3 = (X %among% c("a", "b")) & (X %among% c("a", "c") | Y %among% "d")
check_neg(f3, !f3, "Negation of formula with simplification")

cat("\n=== Double negation ===\n")

formulas_to_test = list(
  (X %among% "a" | Y %among% "d") & Z %among% "g",
  (X %among% c("a", "b") | Y %among% "d") & (X %among% "a" | Y %among% "e"),
  X %among% "a" & Y %among% "d",
  (X %among% c("a", "b") | Y %among% c("d", "e") | Z %among% c("g", "h")) &
    (X %among% "a" | Y %among% "d"),
  (X %among% "a" | Y %among% "d") & (X %among% "b" | Y %among% "e") & (Y %among% c("d", "e"))
)

for (i in seq_along(formulas_to_test)) {
  f = formulas_to_test[[i]]
  truth_orig = evaluate_formula(f, u)
  dn = tryCatch(!(!f), error = function(e) e)
  if (inherits(dn, "error")) {
    report(sprintf("Double negation %d no error", i), FALSE, dn$message)
  } else {
    truth_dn = evaluate_formula(dn, u)
    report(sprintf("Double negation %d preserves semantics", i), all(truth_orig == truth_dn))
  }
}

cat("\n=== OR distribution (formula | formula) ===\n")

# (A & B) | (C & D) should distribute correctly
f_left = (X %among% "a") & (Y %among% "d")
f_right = (X %among% "b") & (Y %among% "e")
f_or = f_left | f_right

truth_left = evaluate_formula(f_left, u)
truth_right = evaluate_formula(f_right, u)
truth_or = evaluate_formula(f_or, u)
report("formula | formula semantics", all(truth_or == (truth_left | truth_right)))

# Larger distribution
f_left2 = (X %among% "a" | Y %among% "d") & (Z %among% c("g", "h"))
f_right2 = (X %among% "b") & (Y %among% "e" | Z %among% "i")
f_or2 = f_left2 | f_right2
truth_left2 = evaluate_formula(f_left2, u)
truth_right2 = evaluate_formula(f_right2, u)
truth_or2 = evaluate_formula(f_or2, u)
report("larger formula | formula semantics", all(truth_or2 == (truth_left2 | truth_right2)))

# Distribution that leads to tautology
f_left3 = X %among% "a" & Y %among% "d"
f_right3 = X %among% c("b", "c") & Y %among% c("e", "f")
f_or3 = f_left3 | f_right3
truth_or3 = evaluate_formula(f_or3, u)
# This might not be a tautology, but let's check correctness
report("distribution potentially tautological",
  all(truth_or3 == (evaluate_formula(f_left3, u) | evaluate_formula(f_right3, u))))

cat("\n=== OR of formula with atom/clause ===\n")
# formula | atom
f_fa = formula_proper_test = (X %among% "a" & Y %among% "d")
result = f_fa | (Z %among% "g")
truth_fa = evaluate_formula(f_fa, u)
truth_z = evaluate_clause(as.CnfClause(Z %among% "g"), u)
truth_result = evaluate_formula(result, u)
report("formula | atom semantics", all(truth_result == (truth_fa | truth_z)))

# formula | clause
clause_yz = X %among% c("b", "c") | Y %among% "e"
result2 = f_fa | clause_yz
truth_clause = evaluate_clause(clause_yz, u)
truth_result2 = evaluate_formula(result2, u)
report("formula | clause semantics", all(truth_result2 == (truth_fa | truth_clause)))

cat("\n=== Fuzzy negation + distribution ===\n")
set.seed(123)
sym_list = list(X = X, Y = Y, Z = Z)

for (i in 1:200) {
  # Generate two small formulas
  n_clauses_1 = sample(1:3, 1)
  n_clauses_2 = sample(1:3, 1)

  make_random_clause = function() {
    n_atoms = sample(1:3, 1)
    syms = sample(names(sym_list), n_atoms)
    atoms = lapply(syms, function(s) {
      domain = u[[s]]
      vals = sample(domain, sample(1:length(domain), 1))
      sym_list[[s]] %among% vals
    })
    Reduce(`|`, atoms)
  }

  clauses1 = replicate(n_clauses_1, make_random_clause(), simplify = FALSE)
  clauses2 = replicate(n_clauses_2, make_random_clause(), simplify = FALSE)

  f1 = tryCatch(CnfFormula(clauses1), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(clauses2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  # Test OR distribution
  result = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(result, "error")) {
    report(sprintf("Fuzz OR %d no error", i), FALSE, result$message)
    next
  }
  truth1 = evaluate_formula(f1, u)
  truth2 = evaluate_formula(f2, u)
  truth_r = evaluate_formula(result, u)
  n_tests <<- n_tests + 1
  if (!all(truth_r == (truth1 | truth2))) {
    idx = which(truth_r != (truth1 | truth2))[1]
    cat(sprintf("  FAIL: Fuzz OR %d\n", i))
    cat(sprintf("    f1: %s\n    f2: %s\n", format(f1), format(f2)))
    errors <<- c(errors, sprintf("Fuzz OR %d", i))
  }

  # Test negation
  neg1 = tryCatch(!f1, error = function(e) e)
  if (!inherits(neg1, "error")) {
    truth_neg = evaluate_formula(neg1, u)
    n_tests <<- n_tests + 1
    if (!all(truth_neg == !truth1)) {
      cat(sprintf("  FAIL: Fuzz NEG %d\n", i))
      errors <<- c(errors, sprintf("Fuzz NEG %d", i))
    }
  }
}
cat(sprintf("  Completed fuzzy neg+dist: %d tests\n", n_tests))

cat(sprintf("\n=== Summary: %d tests, %d failures ===\n", n_tests, length(errors)))
if (length(errors)) {
  for (e in errors) cat(sprintf("  - %s\n", e))
}
