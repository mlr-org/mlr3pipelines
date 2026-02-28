source("experiments/test_harness.R")

# Use the documentation's own HLA example, then test with duplicate domain
# Doc example:
# (X %among% c("a", "b") | Y %among% c("d", "e")) &
#   (X %among% "a" | Z %among% c("g", "h")) &
#   (X %among% "b" | Z %among% c("h", "i")) &
#   (Y %among% c("d", "e") | Z %among% c("g", "i"))
# The first clause is a hidden tautology.

cat("=== Reference: Doc HLA example with clean domains ===\n")
u1 = CnfUniverse()
X1 = CnfSymbol(u1, "X", c("a", "b", "c"))
Y1 = CnfSymbol(u1, "Y", c("d", "e", "f"))
Z1 = CnfSymbol(u1, "Z", c("g", "h", "i"))

f1 = (X1 %among% c("a", "b") | Y1 %among% c("d", "e")) &
  (X1 %among% "a" | Z1 %among% c("g", "h")) &
  (X1 %among% "b" | Z1 %among% c("h", "i")) &
  (Y1 %among% c("d", "e") | Z1 %among% c("g", "i"))

n_clauses_1 = if (is.logical(unclass(f1))) paste0("logical:", as.logical(f1)) else length(unclass(f1))
cat(sprintf("  Clean: %s clauses\n", n_clauses_1))

# Now test with duplicate in Z's domain
cat("\n=== Duplicate in Z domain ===\n")
u2 = CnfUniverse()
X2 = CnfSymbol(u2, "X", c("a", "b", "c"))
Y2 = CnfSymbol(u2, "Y", c("d", "e", "f"))
Z2 = CnfSymbol(u2, "Z", c("g", "g", "h", "i"))  # DUPLICATE "g"!

f2 = (X2 %among% c("a", "b") | Y2 %among% c("d", "e")) &
  (X2 %among% "a" | Z2 %among% c("g", "h")) &
  (X2 %among% "b" | Z2 %among% c("h", "i")) &
  (Y2 %among% c("d", "e") | Z2 %among% c("g", "i"))

n_clauses_2 = if (is.logical(unclass(f2))) paste0("logical:", as.logical(f2)) else length(unclass(f2))
cat(sprintf("  Dup Z: %s clauses\n", n_clauses_2))

# Verify semantics on clean domain
cat("\n=== Semantic check ===\n")
truth_clean = evaluate_formula(f1, u1)
# Manually compute expected truth
varnames = ls(u1)
domains = lapply(varnames, function(v) get(v, u1))
names(domains) = varnames
assignments = expand.grid(domains, stringsAsFactors = FALSE)

raw_clauses = list(
  as.CnfClause(X1 %among% c("a", "b") | Y1 %among% c("d", "e")),
  as.CnfClause(X1 %among% "a" | Z1 %among% c("g", "h")),
  as.CnfClause(X1 %among% "b" | Z1 %among% c("h", "i")),
  as.CnfClause(Y1 %among% c("d", "e") | Z1 %among% c("g", "i"))
)

raw_truth = rep(TRUE, nrow(assignments))
for (cl in raw_clauses) {
  cl_bare = unclass(cl)
  cl_truth = rep(FALSE, nrow(assignments))
  for (sym in names(cl_bare)) {
    cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
  }
  raw_truth = raw_truth & cl_truth
}

mismatches = which(truth_clean != raw_truth)
if (length(mismatches)) {
  cat(sprintf("  BUG: Clean domain formula has semantic error at row %d!\n", mismatches[1]))
} else {
  cat("  Clean domain formula is semantically correct.\n")
}

# For dup domain, evaluate on the dup grid
truth_dup = evaluate_formula(f2, u2)
varnames2 = ls(u2)
domains2 = lapply(varnames2, function(v) get(v, u2))
names(domains2) = varnames2
assignments2 = expand.grid(domains2, stringsAsFactors = FALSE)

raw_truth2 = rep(TRUE, nrow(assignments2))
for (cl_idx in 1:4) {
  # Reconstruct clauses for dup domain
  cl = switch(cl_idx,
    as.CnfClause(X2 %among% c("a", "b") | Y2 %among% c("d", "e")),
    as.CnfClause(X2 %among% "a" | Z2 %among% c("g", "h")),
    as.CnfClause(X2 %among% "b" | Z2 %among% c("h", "i")),
    as.CnfClause(Y2 %among% c("d", "e") | Z2 %among% c("g", "i"))
  )
  cl_bare = unclass(cl)
  if (isTRUE(cl_bare)) next
  cl_truth = rep(FALSE, nrow(assignments2))
  for (sym in names(cl_bare)) {
    cl_truth = cl_truth | (assignments2[[sym]] %in% cl_bare[[sym]])
  }
  raw_truth2 = raw_truth2 & cl_truth
}

mismatches2 = which(truth_dup != raw_truth2)
if (length(mismatches2)) {
  cat(sprintf("  BUG: Dup domain formula has SEMANTIC ERROR at row %d!\n", mismatches2[1]))
  idx = mismatches2[1]
  cat(sprintf("  Assignment: %s\n", paste(colnames(assignments2), "=", assignments2[idx, ], collapse = ", ")))
  cat(sprintf("  Raw truth: %s, Simplified: %s\n", raw_truth2[idx], truth_dup[idx]))
} else {
  cat("  Dup domain formula is semantically correct.\n")
}

if (n_clauses_1 != n_clauses_2) {
  cat(sprintf("\n  NOTE: Different clause counts! Clean=%s, Dup=%s\n", n_clauses_1, n_clauses_2))
  cat("  The duplicate domain causes missed HLA tautology elimination.\n")
  cat("  This is NOT a semantic bug, but a missed simplification opportunity.\n")
  cat("  Root cause: length(range) == length(universe[[symbol]]) check fails with duplicates.\n")
}

cat("\n=== Systematic: Test many HLA-triggering patterns with dup domains ===\n")
# Generate random formulas that are likely to trigger HLA
n_failures = 0
n_tests = 0

for (trial in 1:200) {
  u = CnfUniverse()
  # Randomly decide whether to add duplicates
  has_dup = sample(c(TRUE, FALSE), 1)

  dom_x = c("x1", "x2", "x3")
  dom_y = c("y1", "y2", "y3")
  dom_z = c("z1", "z2", "z3")

  if (has_dup) {
    # Add a random duplicate
    which_dup = sample(1:3, 1)
    if (which_dup == 1) dom_x = c(dom_x, sample(dom_x, 1))
    if (which_dup == 2) dom_y = c(dom_y, sample(dom_y, 1))
    if (which_dup == 3) dom_z = c(dom_z, sample(dom_z, 1))
  }

  X = CnfSymbol(u, "X", dom_x)
  Y = CnfSymbol(u, "Y", dom_y)
  Z = CnfSymbol(u, "Z", dom_z)
  syms = list(X = X, Y = Y, Z = Z)

  # Create HLA-triggering pattern: one large clause + several helpers
  n_clauses = sample(4:8, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = unique(u[[s]])  # use unique values for atom creation
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [trial %d]: %s\n", trial, f$message)); next
  }

  # Check semantics
  n_tests <<- n_tests + 1
  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }

  simplified_truth = evaluate_formula(f, u)
  mismatches = which(raw_truth != simplified_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [trial %d, dup=%s]: row %d (raw=%s, simp=%s)\n",
      trial, has_dup, idx, raw_truth[idx], simplified_truth[idx]))
  }
}

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
