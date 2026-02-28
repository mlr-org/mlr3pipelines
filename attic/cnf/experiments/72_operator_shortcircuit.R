#!/usr/bin/env Rscript
# Test the shortcut/shortcircuit paths in &.CnfFormula and |.CnfFormula
# These have subtle interactions between isTRUE/isFALSE and the internal
# representations of TRUE/FALSE formulas.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Operator shortcircuit tests ===\n")

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a", "b", "c"))
B = CnfSymbol(u, "B", c("x", "y"))

f_true = as.CnfFormula(TRUE)
f_false = as.CnfFormula(FALSE)
f_norm = CnfFormula(list(as.CnfClause(A %among% c("a", "b")), as.CnfClause(B %among% "x")))

# --- AND operations ---
cat("\n--- AND operations ---\n")

# TRUE & normal
n_tests = n_tests + 1
r = tryCatch(f_true & f_norm, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: TRUE & normal error: %s\n", r$message))
} else {
  t1 = evaluate_formula(r, u)
  t2 = evaluate_formula(f_norm, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat("FAIL: TRUE & normal != normal\n")
  }
}

# normal & TRUE
n_tests = n_tests + 1
r = tryCatch(f_norm & f_true, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: normal & TRUE error: %s\n", r$message))
} else {
  t1 = evaluate_formula(r, u)
  t2 = evaluate_formula(f_norm, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat("FAIL: normal & TRUE != normal\n")
  }
}

# FALSE & normal
n_tests = n_tests + 1
r = tryCatch(f_false & f_norm, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: FALSE & normal error: %s\n", r$message))
} else {
  t1 = evaluate_formula(r, u)
  if (any(t1)) {
    n_failures = n_failures + 1
    cat("FAIL: FALSE & normal should be all FALSE\n")
  }
}

# normal & FALSE
n_tests = n_tests + 1
r = tryCatch(f_norm & f_false, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: normal & FALSE error: %s\n", r$message))
} else {
  t1 = evaluate_formula(r, u)
  if (any(t1)) {
    n_failures = n_failures + 1
    cat("FAIL: normal & FALSE should be all FALSE\n")
  }
}

# TRUE & TRUE
n_tests = n_tests + 1
r = tryCatch(f_true & f_true, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: TRUE & TRUE error: %s\n", r$message))
} else if (!isTRUE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE & TRUE should be TRUE\n")
}

# FALSE & FALSE
n_tests = n_tests + 1
r = tryCatch(f_false & f_false, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: FALSE & FALSE error: %s\n", r$message))
} else if (!isFALSE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE & FALSE should be FALSE\n")
}

# TRUE & FALSE
n_tests = n_tests + 1
r = tryCatch(f_true & f_false, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: TRUE & FALSE error: %s\n", r$message))
} else if (!isFALSE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE & FALSE should be FALSE\n")
}

# FALSE & TRUE
n_tests = n_tests + 1
r = tryCatch(f_false & f_true, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: FALSE & TRUE error: %s\n", r$message))
} else if (!isFALSE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE & TRUE should be FALSE\n")
}

cat(sprintf("  AND operations: %d tests, %d failures\n", n_tests, n_failures))

# --- OR operations ---
cat("\n--- OR operations ---\n")

# TRUE | normal
n_tests = n_tests + 1
r = tryCatch(f_true | f_norm, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: TRUE | normal error: %s\n", r$message))
} else if (!isTRUE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE | normal should be TRUE\n")
}

# normal | TRUE
n_tests = n_tests + 1
r = tryCatch(f_norm | f_true, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: normal | TRUE error: %s\n", r$message))
} else if (!isTRUE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: normal | TRUE should be TRUE\n")
}

# FALSE | normal
n_tests = n_tests + 1
r = tryCatch(f_false | f_norm, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: FALSE | normal error: %s\n", r$message))
} else {
  t1 = evaluate_formula(r, u)
  t2 = evaluate_formula(f_norm, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat("FAIL: FALSE | normal != normal\n")
  }
}

# normal | FALSE
n_tests = n_tests + 1
r = tryCatch(f_norm | f_false, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: normal | FALSE error: %s\n", r$message))
} else {
  t1 = evaluate_formula(r, u)
  t2 = evaluate_formula(f_norm, u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat("FAIL: normal | FALSE != normal\n")
  }
}

# TRUE | TRUE
n_tests = n_tests + 1
r = tryCatch(f_true | f_true, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: TRUE | TRUE error: %s\n", r$message))
} else if (!isTRUE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE | TRUE should be TRUE\n")
}

# FALSE | FALSE
n_tests = n_tests + 1
r = tryCatch(f_false | f_false, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: FALSE | FALSE error: %s\n", r$message))
} else if (!isFALSE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE | FALSE should be FALSE\n")
}

# TRUE | FALSE
n_tests = n_tests + 1
r = tryCatch(f_true | f_false, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: TRUE | FALSE error: %s\n", r$message))
} else if (!isTRUE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE | FALSE should be TRUE\n")
}

# FALSE | TRUE
n_tests = n_tests + 1
r = tryCatch(f_false | f_true, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: FALSE | TRUE error: %s\n", r$message))
} else if (!isTRUE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE | TRUE should be TRUE\n")
}

cat(sprintf("  OR operations: %d tests, %d failures\n", n_tests, n_failures))

# --- NOT operations ---
cat("\n--- NOT operations ---\n")

n_tests = n_tests + 1
r = tryCatch(!f_true, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: !TRUE error: %s\n", r$message))
} else if (!isFALSE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: !TRUE should be FALSE\n")
}

n_tests = n_tests + 1
r = tryCatch(!f_false, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: !FALSE error: %s\n", r$message))
} else if (!isTRUE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: !FALSE should be TRUE\n")
}

cat(sprintf("  NOT operations: %d tests, %d failures\n", n_tests, n_failures))

# --- Mixed types in operators ---
cat("\n--- Mixed types (atom & formula, clause | formula, etc.) ---\n")

atom_a = A %among% c("a", "b")
clause_b = as.CnfClause(B %among% "x")

# atom & formula
n_tests = n_tests + 1
r = tryCatch(atom_a & f_norm, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: atom & formula error: %s\n", r$message))
}

# formula & atom
n_tests = n_tests + 1
r = tryCatch(f_norm & atom_a, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: formula & atom error: %s\n", r$message))
}

# clause | formula
n_tests = n_tests + 1
r = tryCatch(clause_b | f_norm, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: clause | formula error: %s\n", r$message))
}

# formula | clause
n_tests = n_tests + 1
r = tryCatch(f_norm | clause_b, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: formula | clause error: %s\n", r$message))
}

# atom | formula
n_tests = n_tests + 1
r = tryCatch(atom_a | f_norm, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: atom | formula error: %s\n", r$message))
}

# formula | atom
n_tests = n_tests + 1
r = tryCatch(f_norm | atom_a, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: formula | atom error: %s\n", r$message))
}

# atom & TRUE formula
n_tests = n_tests + 1
r = tryCatch(atom_a & f_true, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: atom & TRUE error: %s\n", r$message))
} else {
  t1 = evaluate_formula(r, u)
  t2 = evaluate_formula(as.CnfFormula(as.CnfClause(atom_a)), u)
  if (!all(t1 == t2)) {
    n_failures = n_failures + 1
    cat("FAIL: atom & TRUE != atom-as-formula\n")
  }
}

# FALSE formula & atom
n_tests = n_tests + 1
r = tryCatch(f_false & atom_a, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: FALSE & atom error: %s\n", r$message))
} else if (!isFALSE(as.logical(r))) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE & atom should be FALSE\n")
}

cat(sprintf("  Mixed types: %d tests, %d failures\n", n_tests, n_failures))

# --- Semantic verification of mixed-type operations ---
cat("\n--- Semantic verification of mixed-type ops ---\n")
set.seed(72001)

for (trial in 1:300) {
  u = CnfUniverse()
  n_vars = sample(2:3, 1)
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  # Create an atom, a clause, and a formula
  s1 = sample(names(syms), 1)
  atom = syms[[s1]] %among% sample(dom, sample(1:2, 1))

  s2 = sample(names(syms), sample(1:min(2, n_vars), 1))
  clause_atoms = lapply(s2, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
  clause = as.CnfClause(Reduce(`|`, clause_atoms))

  n_cl = sample(1:3, 1)
  f_clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:min(2, n_vars), 1))
    atoms_inner = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms_inner))
  })
  formula = tryCatch(CnfFormula(f_clauses), error = function(e) NULL)
  if (is.null(formula)) next

  # Test: atom & formula == (as.CnfFormula(atom) & formula)
  r1 = tryCatch(atom & formula, error = function(e) NULL)
  r2 = tryCatch(as.CnfFormula(as.CnfClause(atom)) & formula, error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    n_tests = n_tests + 1
    t1 = evaluate_formula(r1, u)
    t2 = evaluate_formula(r2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [mixed-and-%d]: atom & formula != as.CnfFormula(atom) & formula\n", trial))
    }
  }

  # Test: clause | formula == (as.CnfFormula(clause) | formula)
  r1 = tryCatch(clause | formula, error = function(e) NULL)
  r2 = tryCatch(as.CnfFormula(clause) | formula, error = function(e) NULL)
  if (!is.null(r1) && !is.null(r2)) {
    n_tests = n_tests + 1
    t1 = evaluate_formula(r1, u)
    t2 = evaluate_formula(r2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [mixed-or-%d]: clause | formula != as.CnfFormula(clause) | formula\n", trial))
    }
  }
}
cat(sprintf("  Semantic mixed-type: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
