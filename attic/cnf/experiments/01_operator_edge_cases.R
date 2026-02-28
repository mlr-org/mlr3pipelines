source("experiments/test_harness.R")

# Test operator edge cases with TRUE/FALSE atoms, clauses, formulas
errors = character(0)
report = function(test_name, ok, msg = "") {
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

atom_T = X %among% c("a", "b", "c")  # TRUE atom
atom_F = X %among% character(0)        # FALSE atom
atom_X = X %among% c("a", "b")         # proper atom
atom_Y = Y %among% "d"                 # proper atom

cat("=== CnfAtom edge cases ===\n")

# Negation of TRUE/FALSE atoms
report("!TRUE_atom is FALSE", isFALSE(as.logical(!atom_T)))
report("!FALSE_atom is TRUE", isTRUE(as.logical(!atom_F)))
report("!!proper_atom", isTRUE(all.equal(!(!atom_X), atom_X)))

# OR with TRUE/FALSE
report("TRUE | proper = TRUE clause", isTRUE(as.logical(atom_T | atom_X)))
report("FALSE | proper = proper clause", !is.logical(unclass(atom_F | atom_X)))
report("proper | TRUE = TRUE clause", isTRUE(as.logical(atom_X | atom_T)))
report("proper | FALSE = proper clause", !is.logical(unclass(atom_X | atom_F)))
report("TRUE | TRUE = TRUE", isTRUE(as.logical(atom_T | atom_T)))
report("FALSE | FALSE = FALSE", isFALSE(as.logical(atom_F | atom_F)))
report("TRUE | FALSE = TRUE", isTRUE(as.logical(atom_T | atom_F)))
report("FALSE | TRUE = TRUE", isTRUE(as.logical(atom_F | atom_T)))

# AND with TRUE/FALSE
report("TRUE & proper = proper formula", !is.logical(unclass(atom_T & atom_X)))
report("FALSE & proper = FALSE formula", isFALSE(as.logical(atom_F & atom_X)))
report("proper & TRUE = proper formula", !is.logical(unclass(atom_X & atom_T)))
report("proper & FALSE = FALSE formula", isFALSE(as.logical(atom_X & atom_F)))
report("TRUE & TRUE = TRUE", isTRUE(as.logical(atom_T & atom_T)))
report("FALSE & FALSE = FALSE", isFALSE(as.logical(atom_F & atom_F)))

cat("\n=== CnfClause edge cases ===\n")

clause_T = as.CnfClause(atom_T)
clause_F = as.CnfClause(atom_F)
clause_XY = atom_X | atom_Y  # proper clause

# OR of clauses
report("TRUE_cl | proper_cl = TRUE", isTRUE(as.logical(clause_T | clause_XY)))
report("FALSE_cl | proper_cl = proper", !is.logical(unclass(clause_F | clause_XY)))
report("proper_cl | TRUE_cl = TRUE", isTRUE(as.logical(clause_XY | clause_T)))
report("proper_cl | FALSE_cl = proper", !is.logical(unclass(clause_XY | clause_F)))

# AND of clauses (yields formula)
report("TRUE_cl & proper_cl = proper formula", !is.logical(unclass(clause_T & clause_XY)))
report("FALSE_cl & proper_cl = FALSE formula", isFALSE(as.logical(clause_F & clause_XY)))

# Negation of clause
result = tryCatch(!clause_T, error = function(e) e)
report("!TRUE_clause works", !inherits(result, "error"), if (inherits(result, "error")) result$message else "")
if (!inherits(result, "error")) {
  report("!TRUE_clause is FALSE formula", isFALSE(as.logical(result)))
}

result = tryCatch(!clause_F, error = function(e) e)
report("!FALSE_clause works", !inherits(result, "error"), if (inherits(result, "error")) result$message else "")
if (!inherits(result, "error")) {
  report("!FALSE_clause is TRUE formula", isTRUE(as.logical(result)))
}

cat("\n=== CnfFormula edge cases ===\n")

formula_T = as.CnfFormula(TRUE)
formula_F = as.CnfFormula(FALSE)
formula_proper = atom_X & atom_Y

# OR of formulas
report("TRUE_frm | proper_frm = TRUE", isTRUE(as.logical(formula_T | formula_proper)))
report("FALSE_frm | proper_frm = proper", !is.logical(unclass(formula_F | formula_proper)))
report("proper_frm | TRUE_frm = TRUE", isTRUE(as.logical(formula_proper | formula_T)))
report("proper_frm | FALSE_frm = proper", !is.logical(unclass(formula_proper | formula_F)))

# Negation of formula
report("!TRUE_formula is FALSE", isFALSE(as.logical(!formula_T)))
report("!FALSE_formula is TRUE", isTRUE(as.logical(!formula_F)))

# Double negation
result = tryCatch(!(!formula_proper), error = function(e) e)
report("!!formula doesn't error", !inherits(result, "error"), if (inherits(result, "error")) result$message else "")
if (!inherits(result, "error")) {
  report("!!formula equivalent", formulas_equivalent(result, formula_proper, u))
}

cat("\n=== Mixed type operations ===\n")
# atom | clause
report("atom | clause works", inherits(atom_X | clause_XY, "CnfClause"))
# clause | atom
report("clause | atom works", inherits(clause_XY | atom_X, "CnfClause"))
# atom | formula
report("atom | formula works", inherits(atom_X | formula_proper, "CnfFormula"))
# formula | atom
report("formula | atom works", inherits(formula_proper | atom_X, "CnfFormula"))
# clause | formula
report("clause | formula works", inherits(clause_XY | formula_proper, "CnfFormula"))
# formula | clause
report("formula | clause works", inherits(formula_proper | clause_XY, "CnfFormula"))

cat("\n=== Semantic correctness of mixed operations ===\n")
# Check that atom | clause gives correct truth table
result = atom_X | clause_XY
# atom_X = X in {a,b}, clause_XY = X in {a,b} | Y in {d}
# result should be: X in {a,b} | Y in {d}  (same as clause_XY since atom_X is subsumed)
report("atom | clause semantics", formulas_equivalent(as.CnfFormula(result), as.CnfFormula(clause_XY), u))

# Check atom & clause
result = atom_X & clause_XY
# atom_X = X in {a,b}, clause_XY = X in {a,b} | Y in {d}
# conjunction should be: X in {a,b} AND (X in {a,b} | Y in {d})
report("atom & clause semantics",
  formulas_equivalent(result,
    CnfFormula(list(as.CnfClause(atom_X), clause_XY)), u))

cat("\n=== Summary ===\n")
if (length(errors)) {
  cat(sprintf("FAILURES (%d):\n", length(errors)))
  for (e in errors) cat(sprintf("  - %s\n", e))
} else {
  cat("All tests passed!\n")
}
