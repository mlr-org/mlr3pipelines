#!/usr/bin/env Rscript
# Investigate the failures from experiment 74
source("experiments/test_harness.R")

# === Issue 1: "wrong arguments for subsetting an environment" ===
cat("=== Issue 1: CnfFormula from list of CnfFormulas ===\n")
set.seed(74001)

# Reproduce issue from trial 62
trial = 0
for (i in 1:200) {
  trial = trial + 1
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  sub_formulas = list()
  all_clauses = list()
  for (sf in 1:sample(2:3, 1)) {
    n_cl = sample(1:3, 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) {
        syms[[s]] %among% sample(dom, sample(1:2, 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    f_sub = tryCatch(CnfFormula(clauses), error = function(e) NULL)
    if (is.null(f_sub)) next
    sub_formulas[[length(sub_formulas) + 1]] = f_sub
    all_clauses = c(all_clauses, clauses)
  }
  if (length(sub_formulas) < 2) next

  f_combined = tryCatch(CnfFormula(sub_formulas), error = function(e) e)
  if (inherits(f_combined, "error")) {
    if (trial == 62) {
      cat(sprintf("REPRODUCED at trial %d: %s\n", trial, f_combined$message))
      cat("Sub-formulas:\n")
      for (j in seq_along(sub_formulas)) {
        cat(sprintf("  Sub-formula %d:\n", j))
        print(sub_formulas[[j]])
        cat(sprintf("  Universe: %s\n", paste(ls(attr(sub_formulas[[j]], "universe")), collapse = ", ")))
      }
      # Check if universes are identical
      cat(sprintf("Same universe? %s\n", identical(attr(sub_formulas[[1]], "universe"), attr(sub_formulas[[2]], "universe"))))
      cat(sprintf("Traceback:\n"))
      f_combined2 = tryCatch(CnfFormula(sub_formulas), error = function(e) {
        cat(sprintf("  Error class: %s\n", class(e)))
        cat(sprintf("  Error call: %s\n", deparse(e$call)))
        e
      })
      break
    }
  }
}

# === Issue 2: "All clauses must be in the same universe" with TRUE/FALSE ===
cat("\n=== Issue 2: TRUE/FALSE clauses universe mismatch ===\n")

# The issue: as.CnfClause(TRUE) and as.CnfClause(FALSE) have universe = NULL
# while regular clauses have a universe set.
u = CnfUniverse()
A = CnfSymbol(u, "A", c("a", "b", "c"))

cl_normal = as.CnfClause(A %among% c("a", "b"))
cl_true = as.CnfClause(TRUE)
cl_false = as.CnfClause(FALSE)

cat(sprintf("Normal clause universe: %s\n",
  if (is.null(attr(cl_normal, "universe"))) "NULL" else "set"))
cat(sprintf("TRUE clause universe: %s\n",
  if (is.null(attr(cl_true, "universe"))) "NULL" else "set"))
cat(sprintf("FALSE clause universe: %s\n",
  if (is.null(attr(cl_false, "universe"))) "NULL" else "set"))

# The problem: when TRUE/FALSE clause comes FIRST in the list,
# the universe is extracted from it (NULL), and then compared against
# the real clause's universe -> mismatch!
cat("\nTest: CnfFormula(list(TRUE_clause, normal_clause)):\n")
result = tryCatch(
  CnfFormula(list(cl_true, cl_normal)),
  error = function(e) e
)
if (inherits(result, "error")) {
  cat(sprintf("  ERROR: %s\n", result$message))
} else {
  cat("  OK\n")
}

cat("\nTest: CnfFormula(list(normal_clause, TRUE_clause)):\n")
result = tryCatch(
  CnfFormula(list(cl_normal, cl_true)),
  error = function(e) e
)
if (inherits(result, "error")) {
  cat(sprintf("  ERROR: %s\n", result$message))
} else {
  cat("  OK\n")
}

cat("\nTest: CnfFormula(list(FALSE_clause, normal_clause)):\n")
result = tryCatch(
  CnfFormula(list(cl_false, cl_normal)),
  error = function(e) e
)
if (inherits(result, "error")) {
  cat(sprintf("  ERROR: %s\n", result$message))
} else {
  cat("  OK\n")
}

cat("\nTest: CnfFormula(list(normal_clause, FALSE_clause)):\n")
result = tryCatch(
  CnfFormula(list(cl_normal, cl_false)),
  error = function(e) e
)
if (inherits(result, "error")) {
  cat(sprintf("  ERROR: %s\n", result$message))
} else {
  cat("  OK\n")
}

# Check if TRUE formula has universe
cat("\n\nTRUE formula universe test:\n")
f_true = as.CnfFormula(TRUE)
cat(sprintf("as.CnfFormula(TRUE) universe: %s\n",
  if (is.null(attr(f_true, "universe"))) "NULL" else "set"))

# What about TRUE constructed from a tautological clause?
taut_clause = A %among% c("a", "b", "c")  # tautology
cat(sprintf("Tautological atom internal: %s\n", unclass(taut_clause)))
f_taut = CnfFormula(list(as.CnfClause(taut_clause)))
cat(sprintf("CnfFormula(taut_clause) universe: %s\n",
  if (is.null(attr(f_taut, "universe"))) "NULL" else "set"))
cat(sprintf("CnfFormula(taut_clause) as.logical: %s\n", as.logical(f_taut)))

# Now test: what about CnfFormula where a sub-formula is TRUE/FALSE?
cat("\n\nTest: CnfFormula from TRUE formula + normal clause:\n")
f_true_formula = as.CnfFormula(TRUE)
result = tryCatch(
  CnfFormula(list(f_true_formula, cl_normal)),
  error = function(e) e
)
if (inherits(result, "error")) {
  cat(sprintf("  ERROR: %s\n", result$message))
} else {
  cat("  OK\n")
}

cat("\n\nTest: CnfFormula from normal clause + TRUE formula:\n")
result = tryCatch(
  CnfFormula(list(cl_normal, f_true_formula)),
  error = function(e) e
)
if (inherits(result, "error")) {
  cat(sprintf("  ERROR: %s\n", result$message))
} else {
  cat("  OK\n")
}
