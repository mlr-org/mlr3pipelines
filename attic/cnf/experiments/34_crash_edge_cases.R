#!/usr/bin/env Rscript
# Test edge cases that might cause crashes, errors, or unexpected behavior
# Focus on boundary conditions the simplifier must handle
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

check_fc = function(formula, raw_clauses, universe, label) {
  n_tests <<- n_tests + 1
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in raw_clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }

  simplified_truth = evaluate_formula(formula, universe)
  mismatches = which(raw_truth != simplified_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d (raw=%s, simp=%s)\n",
      label, idx, raw_truth[idx], simplified_truth[idx]))
    return(FALSE)
  }
  TRUE
}

cat("=== Single clause formula ===\n")
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("d", "e"))

# Single non-unit clause
n_tests = n_tests + 1
f1 = tryCatch(CnfFormula(list(as.CnfClause(X %among% c("a", "b") | Y %among% "d"))), error = function(e) e)
if (inherits(f1, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR: Single clause: %s\n", f1$message))
} else {
  check_fc(f1, list(as.CnfClause(X %among% c("a", "b") | Y %among% "d")), u, "single-clause")
}

# Single unit clause
n_tests = n_tests + 1
f2 = tryCatch(CnfFormula(list(as.CnfClause(X %among% "a"))), error = function(e) e)
if (inherits(f2, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR: Single unit: %s\n", f2$message))
}

# Single TRUE clause
n_tests = n_tests + 1
f3 = tryCatch(CnfFormula(list(as.CnfClause(TRUE))), error = function(e) e)
if (inherits(f3, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR: Single TRUE clause: %s\n", f3$message))
} else if (!isTRUE(as.logical(f3))) {
  n_failures = n_failures + 1
  cat("  FAIL: Single TRUE clause should give TRUE formula\n")
}

# Single FALSE clause
n_tests = n_tests + 1
f4 = tryCatch(CnfFormula(list(as.CnfClause(FALSE))), error = function(e) e)
if (inherits(f4, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR: Single FALSE clause: %s\n", f4$message))
} else if (!isFALSE(as.logical(f4))) {
  n_failures = n_failures + 1
  cat("  FAIL: Single FALSE clause should give FALSE formula\n")
}

# Empty list
n_tests = n_tests + 1
f5 = tryCatch(CnfFormula(list()), error = function(e) e)
if (inherits(f5, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR: Empty list: %s\n", f5$message))
} else if (!isTRUE(as.logical(f5))) {
  n_failures = n_failures + 1
  cat("  FAIL: Empty formula should be TRUE\n")
}

cat(sprintf("  Single clause: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== All units formula ===\n")
u2 = CnfUniverse()
A = CnfSymbol(u2, "A", c("a1", "a2", "a3"))
B = CnfSymbol(u2, "B", c("b1", "b2"))
C = CnfSymbol(u2, "C", c("c1", "c2"))

# Multiple compatible units
clauses = list(
  as.CnfClause(A %among% c("a1", "a2")),
  as.CnfClause(B %among% "b1"),
  as.CnfClause(C %among% "c2")
)
f = tryCatch(CnfFormula(clauses), error = function(e) e)
if (inherits(f, "error")) {
  n_tests = n_tests + 1; n_failures = n_failures + 1
  cat(sprintf("  ERROR: Multiple compatible units: %s\n", f$message))
} else {
  check_fc(f, clauses, u2, "all-units-compatible")
}

# Multiple conflicting units for same symbol
clauses2 = list(
  as.CnfClause(A %among% "a1"),
  as.CnfClause(A %among% "a2")
)
f_conflict = tryCatch(CnfFormula(clauses2), error = function(e) e)
n_tests = n_tests + 1
if (inherits(f_conflict, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR: Conflicting units: %s\n", f_conflict$message))
} else if (!isFALSE(as.logical(f_conflict))) {
  n_failures = n_failures + 1
  cat("  FAIL: Conflicting units should give FALSE\n")
}

# Duplicate units
clauses3 = list(
  as.CnfClause(A %among% c("a1", "a2")),
  as.CnfClause(A %among% c("a1", "a2")),
  as.CnfClause(A %among% c("a1", "a2"))
)
f_dup = tryCatch(CnfFormula(clauses3), error = function(e) e)
if (inherits(f_dup, "error")) {
  n_tests = n_tests + 1; n_failures = n_failures + 1
  cat(sprintf("  ERROR: Duplicate units: %s\n", f_dup$message))
} else {
  check_fc(f_dup, clauses3, u2, "dup-units")
}

cat(sprintf("  All units: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== All tautology clauses ===\n")
u3 = CnfUniverse()
X3 = CnfSymbol(u3, "X", c("a", "b"))
Y3 = CnfSymbol(u3, "Y", c("c", "d"))

clauses_taut = list(
  as.CnfClause(X3 %among% c("a", "b")),
  as.CnfClause(Y3 %among% c("c", "d")),
  as.CnfClause(X3 %among% c("a", "b") | Y3 %among% c("c", "d"))
)
f_taut = tryCatch(CnfFormula(clauses_taut), error = function(e) e)
n_tests = n_tests + 1
if (inherits(f_taut, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR: All tautologies: %s\n", f_taut$message))
} else if (!isTRUE(as.logical(f_taut))) {
  n_failures = n_failures + 1
  cat("  FAIL: All tautologies should give TRUE\n")
}
cat(sprintf("  All tautologies: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Formulas with identical clauses that differ only in symbol order ===\n")
u4 = CnfUniverse()
P = CnfSymbol(u4, "P", c("p1", "p2"))
Q = CnfSymbol(u4, "Q", c("q1", "q2"))
R = CnfSymbol(u4, "R", c("r1", "r2"))

# Two clauses that are the same but atoms in different order
cl_a = as.CnfClause(P %among% "p1" | Q %among% "q1")
cl_b = as.CnfClause(Q %among% "q1" | P %among% "p1")
f_ab = tryCatch(CnfFormula(list(cl_a, cl_b)), error = function(e) e)
n_tests = n_tests + 1
if (inherits(f_ab, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("  ERROR: Same clause diff order: %s\n", f_ab$message))
} else {
  check_fc(f_ab, list(cl_a, cl_b), u4, "same-clause-diff-order")
}

cat(sprintf("  Symbol order: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Extreme: many units for many variables ===\n")
for (trial in 1:100) {
  u5 = CnfUniverse()
  n_vars = sample(3:8, 1)
  syms = list()
  clauses = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    dom_size = sample(2:4, 1)
    syms[[vname]] = CnfSymbol(u5, vname, paste0(vname, "_", 1:dom_size))
    # Each variable gets a unit
    dom = u5[[vname]]
    clauses[[v]] = as.CnfClause(syms[[vname]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1)))
  }
  # Add some multi-symbol clauses
  for (i in 1:sample(2:4, 1)) {
    n_atoms = sample(2:min(4, n_vars), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u5[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [extreme trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u5, sprintf("extreme-%d", trial))
}
cat(sprintf("  Extreme units: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Operations on TRUE/FALSE formulas ===\n")
f_true = as.CnfFormula(TRUE)
f_false = as.CnfFormula(FALSE)

# TRUE & TRUE
n_tests = n_tests + 1
r = tryCatch(f_true & f_true, error = function(e) e)
r = tryCatch(f_true & f_true, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1; cat(sprintf("ERROR: T&T: %s\n", r$message))
} else if (!isTRUE(as.logical(r))) {
  n_failures = n_failures + 1; cat("FAIL: T&T != T\n")
}

# FALSE & FALSE
n_tests = n_tests + 1
r = tryCatch(f_false & f_false, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1; cat(sprintf("ERROR: F&F: %s\n", r$message))
} else if (!isFALSE(as.logical(r))) {
  n_failures = n_failures + 1; cat("FAIL: F&F != F\n")
}

# TRUE | FALSE
n_tests = n_tests + 1
r = tryCatch(f_true | f_false, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1; cat(sprintf("ERROR: T|F: %s\n", r$message))
} else if (!isTRUE(as.logical(r))) {
  n_failures = n_failures + 1; cat("FAIL: T|F != T\n")
}

# FALSE | FALSE
n_tests = n_tests + 1
r = tryCatch(f_false | f_false, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1; cat(sprintf("ERROR: F|F: %s\n", r$message))
} else if (!isFALSE(as.logical(r))) {
  n_failures = n_failures + 1; cat("FAIL: F|F != F\n")
}

# !TRUE = FALSE
n_tests = n_tests + 1
r = tryCatch(!f_true, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1; cat(sprintf("ERROR: !T: %s\n", r$message))
} else if (!isFALSE(as.logical(r))) {
  n_failures = n_failures + 1; cat("FAIL: !T != F\n")
}

# !FALSE = TRUE
n_tests = n_tests + 1
r = tryCatch(!f_false, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1; cat(sprintf("ERROR: !F: %s\n", r$message))
} else if (!isTRUE(as.logical(r))) {
  n_failures = n_failures + 1; cat("FAIL: !F != T\n")
}

# Formula & TRUE
n_tests = n_tests + 1
u6 = CnfUniverse()
X6 = CnfSymbol(u6, "X", c("a", "b"))
f_x = as.CnfFormula(as.CnfClause(X6 %among% "a"))
r = tryCatch(f_x & f_true, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1; cat(sprintf("ERROR: f&T: %s\n", r$message))
}

# Formula | FALSE
n_tests = n_tests + 1
r = tryCatch(f_x | f_false, error = function(e) e)
if (inherits(r, "error")) {
  n_failures = n_failures + 1; cat(sprintf("ERROR: f|F: %s\n", r$message))
}

cat(sprintf("  TRUE/FALSE ops: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== as.list on all formula types ===\n")
# Normal formula
n_tests = n_tests + 1
r = tryCatch(as.list(f_x), error = function(e) e)
if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR as.list normal: %s\n", r$message)) }

# TRUE formula
n_tests = n_tests + 1
r = tryCatch(as.list(f_true), error = function(e) e)
if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR as.list TRUE: %s\n", r$message)) } else if (length(r) != 0) { n_failures = n_failures + 1; cat("FAIL: as.list(TRUE formula) should be empty list\n") }

# FALSE formula
n_tests = n_tests + 1
r = tryCatch(as.list(f_false), error = function(e) e)
if (inherits(r, "error")) { n_failures = n_failures + 1; cat(sprintf("ERROR as.list FALSE: %s\n", r$message)) }

cat(sprintf("  as.list: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
