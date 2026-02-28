source("experiments/test_harness.R")

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

cat("=== as.list.CnfClause ===\n")

# FALSE clause
result = tryCatch(as.list(as.CnfClause(FALSE)), error = function(e) e)
report("as.list(FALSE clause) works", !inherits(result, "error"),
  if (inherits(result, "error")) result$message else "")
if (!inherits(result, "error")) {
  report("as.list(FALSE clause) is empty list", identical(result, list()))
}

# TRUE clause
result = tryCatch(as.list(as.CnfClause(TRUE)), error = function(e) e)
report("as.list(TRUE clause) works", !inherits(result, "error"),
  if (inherits(result, "error")) result$message else "")
if (!inherits(result, "error")) {
  cat(sprintf("    Result class: %s, length: %d\n", class(result), length(result)))
}

# TRUE clause from universe
clause_T = X %among% c("a", "b", "c")  # becomes TRUE
clause_T_as_clause = as.CnfClause(clause_T)
result = tryCatch(as.list(clause_T_as_clause), error = function(e) e)
report("as.list(TRUE clause from atom) works", !inherits(result, "error"),
  if (inherits(result, "error")) result$message else "")

# proper clause
clause = X %among% c("a", "b") | Y %among% "d"
result = tryCatch(as.list(clause), error = function(e) e)
report("as.list(proper clause) works", !inherits(result, "error"),
  if (inherits(result, "error")) result$message else "")
if (!inherits(result, "error")) {
  report("as.list(proper clause) has correct length", length(result) == 2)
  report("as.list(proper clause) contains CnfAtoms",
    all(sapply(result, inherits, "CnfAtom")))
}

cat("\n=== as.list.CnfFormula ===\n")

# TRUE formula
result = tryCatch(as.list(as.CnfFormula(TRUE)), error = function(e) e)
report("as.list(TRUE formula) works", !inherits(result, "error"),
  if (inherits(result, "error")) result$message else "")
if (!inherits(result, "error")) {
  report("as.list(TRUE formula) is empty list", identical(result, list()))
}

# FALSE formula
result = tryCatch(as.list(as.CnfFormula(FALSE)), error = function(e) e)
report("as.list(FALSE formula) works", !inherits(result, "error"),
  if (inherits(result, "error")) result$message else "")
if (!inherits(result, "error")) {
  report("as.list(FALSE formula) has one clause", length(result) == 1)
  if (length(result) == 1) {
    report("as.list(FALSE formula)[[1]] is FALSE clause",
      inherits(result[[1]], "CnfClause") && isFALSE(as.logical(result[[1]])))
  }
}

# proper formula
formula = (X %among% c("a", "b") | Y %among% "d") & (X %among% "a")
result = tryCatch(as.list(formula), error = function(e) e)
report("as.list(proper formula) works", !inherits(result, "error"),
  if (inherits(result, "error")) result$message else "")
if (!inherits(result, "error")) {
  report("as.list(proper formula) contains CnfClauses",
    all(sapply(result, inherits, "CnfClause")))
}

cat("\n=== Roundtrip: formula -> as.list -> CnfFormula ===\n")
# This tests whether as.list returns clauses that can be fed back to CnfFormula
formula = (X %among% c("a", "b") | Y %among% "d") & (X %among% "a")
clauses = tryCatch(as.list(formula), error = function(e) e)
if (!inherits(clauses, "error")) {
  roundtrip = tryCatch(CnfFormula(clauses), error = function(e) e)
  report("CnfFormula(as.list(formula)) works", !inherits(roundtrip, "error"),
    if (inherits(roundtrip, "error")) roundtrip$message else "")
  if (!inherits(roundtrip, "error")) {
    report("Roundtrip preserves semantics", formulas_equivalent(formula, roundtrip, u))
  }
}

cat("\n=== lapply over CnfClause (used in formula_to_expression pattern) ===\n")
# The attic code does lapply(clause, function(atom) { atom$symbol ... })
# This calls as.list.CnfClause internally
clause = X %among% c("a", "b") | Y %among% "d"
atoms = tryCatch(lapply(clause, function(atom) {
  list(sym = atom$symbol, vals = atom$values)
}), error = function(e) e)
report("lapply over proper CnfClause works", !inherits(atoms, "error"),
  if (inherits(atoms, "error")) atoms$message else "")
if (!inherits(atoms, "error")) {
  report("lapply gives correct atoms", length(atoms) == 2)
}

# Now try with TRUE clause
clause_true = as.CnfClause(TRUE)
atoms = tryCatch(lapply(clause_true, function(atom) {
  list(sym = atom$symbol, vals = atom$values)
}), error = function(e) e)
report("lapply over TRUE CnfClause works", !inherits(atoms, "error"),
  if (inherits(atoms, "error")) atoms$message else "")

cat("\n=== Summary ===\n")
if (length(errors)) {
  cat(sprintf("FAILURES (%d):\n", length(errors)))
  for (e in errors) cat(sprintf("  - %s\n", e))
} else {
  cat("All tests passed!\n")
}
