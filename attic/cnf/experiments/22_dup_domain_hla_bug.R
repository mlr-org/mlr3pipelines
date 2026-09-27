source("experiments/test_harness.R")

# Targeted test: Can duplicate domain values cause HLA tautology detection to fail?
# HLA tautology check: length(range_new) == length(universe[[symbol]])
# If domain has duplicates, length(universe[[symbol]]) is inflated, so tautology
# might not be detected even when range_new covers all unique values.

cat("=== HLA tautology detection with duplicate domain ===\n")

# Setup with CORRECT (no duplicate) domains for reference
u_correct = CnfUniverse()
X = CnfSymbol(u_correct, "X", c("x1", "x2"))
Y = CnfSymbol(u_correct, "Y", c("y1", "y2", "y3"))
Z = CnfSymbol(u_correct, "Z", c("z1", "z2"))

# Clauses designed so HLA reveals a hidden tautology:
# C: X in {x1} | Y in {y1} | Z in {z1}  (the target to be eliminated)
# D1: X in {x1} | Y in {y2}  (HLA: add complement of Y in D1 to C -> adds y3 to C's Y)
# D2: X in {x1} | Y in {y3}  (HLA: add complement of Y in D2 to C -> adds y2 to C's Y)
# After HLA: C's Y becomes {y1, y2, y3} = full domain -> tautology!

clauses_correct = list(
  as.CnfClause(X %among% "x1" | Y %among% "y1" | Z %among% "z1"),
  as.CnfClause(X %among% "x1" | Y %among% "y2"),
  as.CnfClause(X %among% "x1" | Y %among% "y3"),
  as.CnfClause(Z %among% "z1")
)

f_correct = CnfFormula(clauses_correct)
cat(sprintf("  Correct domains: formula has %s clauses after simplification\n",
  if (is.logical(unclass(f_correct))) as.logical(f_correct) else length(unclass(f_correct))))

# Now repeat with DUPLICATE domain for Y
u_dup = CnfUniverse()
X2 = CnfSymbol(u_dup, "X", c("x1", "x2"))
Y2 = CnfSymbol(u_dup, "Y", c("y1", "y1", "y2", "y3"))  # DUPLICATE y1!
Z2 = CnfSymbol(u_dup, "Z", c("z1", "z2"))

clauses_dup = list(
  as.CnfClause(X2 %among% "x1" | Y2 %among% "y1" | Z2 %among% "z1"),
  as.CnfClause(X2 %among% "x1" | Y2 %among% "y2"),
  as.CnfClause(X2 %among% "x1" | Y2 %among% "y3"),
  as.CnfClause(Z2 %among% "z1")
)

f_dup = CnfFormula(clauses_dup)
cat(sprintf("  Dup domains: formula has %s clauses after simplification\n",
  if (is.logical(unclass(f_dup))) as.logical(f_dup) else length(unclass(f_dup))))

# Check semantics of dup domain formula
# Note: with dup domain, expand.grid creates extra rows for the duplicate "y1"
varnames_dup = ls(u_dup)
domains_dup = lapply(varnames_dup, function(v) get(v, u_dup))
names(domains_dup) = varnames_dup
assignments_dup = expand.grid(domains_dup, stringsAsFactors = FALSE)
cat(sprintf("  Dup domain assignments: %d rows (vs %d for unique)\n", nrow(assignments_dup), 2*3*2))

# Evaluate raw clauses manually (using only unique domain values for comparison)
u_ref = CnfUniverse()
Xr = CnfSymbol(u_ref, "X", c("x1", "x2"))
Yr = CnfSymbol(u_ref, "Y", c("y1", "y2", "y3"))
Zr = CnfSymbol(u_ref, "Z", c("z1", "z2"))

clauses_ref = list(
  as.CnfClause(Xr %among% "x1" | Yr %among% "y1" | Zr %among% "z1"),
  as.CnfClause(Xr %among% "x1" | Yr %among% "y2"),
  as.CnfClause(Xr %among% "x1" | Yr %among% "y3"),
  as.CnfClause(Zr %among% "z1")
)

# The correctly simplified formula should be: X in {x1} & Z in {z1}
# (since the first clause is a hidden tautology and gets eliminated)
# Or equivalently: just Z in {z1} (since X %among% "x1" is already implied by D1 and D2 together?)
# Actually, D1 and D2 say: (X=x1 | Y=y2) & (X=x1 | Y=y3) & Z=z1
# When X=x2: Y must be in {y2} and Y must be in {y3} -> impossible
# So X must be x1, and Z must be z1. Clause 1 is then tautology (X=x1 is true).
# Result: X in {x1} & Z in {z1}

f_ref = CnfFormula(clauses_ref)
cat(sprintf("  Reference (clean domains): formula has %s clauses\n",
  if (is.logical(unclass(f_ref))) as.logical(f_ref) else length(unclass(f_ref))))

# Check if dup formula is semantically correct by comparing unique-domain truth table
varnames_ref = ls(u_ref)
domains_ref = lapply(varnames_ref, function(v) get(v, u_ref))
names(domains_ref) = varnames_ref
assignments_ref = expand.grid(domains_ref, stringsAsFactors = FALSE)

raw_truth = rep(TRUE, nrow(assignments_ref))
for (cl in clauses_ref) {
  cl_bare = unclass(cl)
  if (isTRUE(cl_bare)) next
  if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments_ref)); break }
  cl_truth = rep(FALSE, nrow(assignments_ref))
  for (sym in names(cl_bare)) {
    cl_truth = cl_truth | (assignments_ref[[sym]] %in% cl_bare[[sym]])
  }
  raw_truth = raw_truth & cl_truth
}

simplified_truth_ref = evaluate_formula(f_ref, u_ref)
mismatches = which(raw_truth != simplified_truth_ref)
if (length(mismatches)) {
  cat(sprintf("  FAIL: Reference formula has semantic bug at row %d!\n", mismatches[1]))
} else {
  cat("  Reference formula semantically correct.\n")
}

cat("\n=== Direct comparison: formulas with and without dup domains ===\n")
# Since the dup domain formula has extra rows from duplicates, we can't directly compare.
# Instead, let's check if both formulas agree on the UNIQUE assignments.

# Compare on the reference assignments (unique domain)
truth_dup_on_unique = evaluate_formula(f_dup, u_ref)  # This won't work since f_dup uses u_dup...

# Instead, let's manually evaluate the dup formula on the unique assignment grid
truth_dup_manual = rep(TRUE, nrow(assignments_ref))
for (cl_idx in seq_along(clauses_ref)) {  # Use ref clauses structure
  cl_bare = unclass(clauses_ref[[cl_idx]])
  if (isTRUE(cl_bare)) next
  if (isFALSE(cl_bare)) { truth_dup_manual = rep(FALSE, nrow(assignments_ref)); break }
  cl_truth = rep(FALSE, nrow(assignments_ref))
  for (sym in names(cl_bare)) {
    cl_truth = cl_truth | (assignments_ref[[sym]] %in% cl_bare[[sym]])
  }
  truth_dup_manual = truth_dup_manual & cl_truth
}

cat(sprintf("  Raw truth: %s\n", paste(which(raw_truth), collapse = ",")))
cat(sprintf("  Ref simplified: %s\n", paste(which(simplified_truth_ref), collapse = ",")))

# Now check: does the dup formula's simplified form match the raw truth
# when evaluated on the unique assignments?
# This requires evaluating f_dup on u_dup's assignments
truth_dup = evaluate_formula(f_dup, u_dup)
# The dup universe has 2*4*2 = 16 rows (some are duplicates)
# We need to check that for each unique (X,Y,Z) combination, the truth matches
unique_check = TRUE
for (i in seq_len(nrow(assignments_ref))) {
  x_val = assignments_ref[i, "X"]
  y_val = assignments_ref[i, "Y"]
  z_val = assignments_ref[i, "Z"]
  # Find this assignment in the dup grid
  dup_rows = which(assignments_dup[["X"]] == x_val & assignments_dup[["Y"]] == y_val & assignments_dup[["Z"]] == z_val)
  if (length(dup_rows) == 0) next  # shouldn't happen
  dup_truth_vals = truth_dup[dup_rows]
  if (!all(dup_truth_vals == raw_truth[i])) {
    cat(sprintf("  MISMATCH: X=%s, Y=%s, Z=%s: raw=%s, dup_simplified=%s\n",
      x_val, y_val, z_val, raw_truth[i], dup_truth_vals[1]))
    unique_check = FALSE
  }
}
if (unique_check) {
  cat("  Dup domain formula produces correct results on all unique assignments.\n")
} else {
  cat("  BUG CONFIRMED: Dup domain causes semantic mismatch!\n")
}

cat("\n=== Suboptimal but not incorrect? ===\n")
# Even if the dup domain causes missed optimizations (hidden tautology not detected),
# the formula might still be semantically correct - just less simplified.
# Let's check both formulas' clause counts.
n_clauses_ref = if (is.logical(unclass(f_ref))) 0 else length(unclass(f_ref))
n_clauses_dup = if (is.logical(unclass(f_dup))) 0 else length(unclass(f_dup))
cat(sprintf("  Reference: %d clauses, Dup: %d clauses\n", n_clauses_ref, n_clauses_dup))
if (n_clauses_dup > n_clauses_ref) {
  cat("  Dup domain formula is LESS simplified (has more clauses) - missed optimization.\n")
  cat("  This is not a semantic bug, but a missed simplification due to missing validation.\n")
} else {
  cat("  Both formulas have the same number of clauses.\n")
}

cat("\nDone.\n")
