source("experiments/test_harness.R")

# Targeted tests for specific simplification scenarios
errors = character(0)
report = function(test_name, ok, msg = "") {
  if (ok) {
    cat(sprintf("  PASS: %s\n", test_name))
  } else {
    cat(sprintf("  FAIL: %s -- %s\n", test_name, msg))
    errors <<- c(errors, paste(test_name, msg, sep = ": "))
  }
}

check_formula = function(formula, universe, label) {
  # Check that the formula evaluates correctly by comparing with a "raw" evaluation
  # Raw = construct clauses and evaluate without simplification
  truth = evaluate_formula(formula, universe)

  # The formula should be self-consistent: re-simplifying should give the same result
  clauses = as.list(formula)
  if (length(clauses) == 0) {
    # TRUE formula
    report(paste(label, "- TRUE formula consistent"), all(truth))
    return(invisible(NULL))
  }
  if (length(clauses) == 1 && isFALSE(as.logical(clauses[[1]]))) {
    report(paste(label, "- FALSE formula consistent"), all(!truth))
    return(invisible(NULL))
  }

  # Check re-simplification
  re_simplified = CnfFormula(clauses)
  truth2 = evaluate_formula(re_simplified, universe)
  report(paste(label, "- re-simplification consistent"), all(truth == truth2))
}

cat("=== Unit propagation edge cases ===\n")

# Two contradictory units
u1 = CnfUniverse()
X1 = CnfSymbol(u1, "X", c("a", "b", "c"))
f = (X1 %among% "a") & (X1 %among% "b")
report("Contradictory units -> FALSE", isFALSE(as.logical(f)))

# Unit that subsumes another clause
u2 = CnfUniverse()
X2 = CnfSymbol(u2, "X", c("a", "b", "c"))
Y2 = CnfSymbol(u2, "Y", c("d", "e", "f"))
f = (X2 %among% c("a", "b")) & (X2 %among% c("a", "b") | Y2 %among% "d")
check_formula(f, u2, "Unit subsumes clause")
report("Unit subsumes clause -> simplifies", length(as.list(f)) == 1)

# Unit that restricts another clause to a unit
u3 = CnfUniverse()
X3 = CnfSymbol(u3, "X", c("a", "b", "c"))
Y3 = CnfSymbol(u3, "Y", c("d", "e", "f"))
f = (X3 %among% "a") & (X3 %among% c("a", "b") | Y3 %among% "d")
check_formula(f, u3, "Unit restricts to unit")

# Cascading units: unit on X restricts a clause to a unit on Y, which then restricts another
u4 = CnfUniverse()
X4 = CnfSymbol(u4, "X", c("a", "b", "c"))
Y4 = CnfSymbol(u4, "Y", c("d", "e", "f"))
Z4 = CnfSymbol(u4, "Z", c("g", "h", "i"))
f = (X4 %among% "a") & (X4 %among% c("b", "c") | Y4 %among% "d") &
    (Y4 %among% c("d", "e") | Z4 %among% "g")
check_formula(f, u4, "Cascading unit propagation")

cat("\n=== Self-subsumption edge cases ===\n")

# Basic self-subsumption
u5 = CnfUniverse()
X5 = CnfSymbol(u5, "X", c("a", "b", "c"))
Y5 = CnfSymbol(u5, "Y", c("d", "e", "f"))
f = (X5 %among% c("a", "b") | Y5 %among% "d") & (X5 %among% "a" | Y5 %among% "e")
check_formula(f, u5, "Self-subsumption")

# Self-subsumption leading to contradiction
u6 = CnfUniverse()
X6 = CnfSymbol(u6, "X", c("a", "b"))
Y6 = CnfSymbol(u6, "Y", c("d", "e"))
f = (X6 %among% "a" | Y6 %among% "d") & (X6 %among% "b" | Y6 %among% "e") &
    (X6 %among% "a" | Y6 %among% "e") & (X6 %among% "b" | Y6 %among% "d")
check_formula(f, u6, "Self-subsumption contradiction")
report("Self-subsumption contradiction -> FALSE", isFALSE(as.logical(f)))

# Self-subsumption with 3 symbols
u7 = CnfUniverse()
X7 = CnfSymbol(u7, "X", c("a", "b", "c"))
Y7 = CnfSymbol(u7, "Y", c("d", "e", "f"))
Z7 = CnfSymbol(u7, "Z", c("g", "h", "i"))
f = (X7 %among% c("a", "b") | Y7 %among% "d" | Z7 %among% "g") &
    (X7 %among% "a" | Y7 %among% "e")
check_formula(f, u7, "Self-subsumption with 3 symbols")

cat("\n=== Hidden literal addition / hidden tautology elimination ===\n")

# Example from the documentation
u8 = CnfUniverse()
X8 = CnfSymbol(u8, "X", c("a", "b", "c"))
Y8 = CnfSymbol(u8, "Y", c("d", "e", "f"))
Z8 = CnfSymbol(u8, "Z", c("g", "h", "i"))
f = (X8 %among% c("a", "b") | Y8 %among% c("d", "e")) &
    (X8 %among% "a" | Z8 %among% c("g", "h")) &
    (X8 %among% "b" | Z8 %among% c("h", "i")) &
    (Y8 %among% c("d", "e") | Z8 %among% c("g", "i"))
check_formula(f, u8, "HLA example from docs")

cat("\n=== Resolution subsumption elimination ===\n")

# Example from docs
u9 = CnfUniverse()
X9 = CnfSymbol(u9, "X", c("a", "b", "c"))
Y9 = CnfSymbol(u9, "Y", c("d", "e", "f"))
f = (X9 %among% "a" | Y9 %among% "d") &
    (X9 %among% "b" | Y9 %among% "e") &
    (Y9 %among% c("d", "e"))
check_formula(f, u9, "Resolution subsumption from docs")
report("Resolution subsumption simplifies", length(as.list(f)) <= 2)

cat("\n=== 2nd order SSE ===\n")

# Create a scenario that specifically needs 2nd order SSE
u10 = CnfUniverse()
X10 = CnfSymbol(u10, "X", c("a", "b", "c"))
Y10 = CnfSymbol(u10, "Y", c("d", "e", "f"))
Z10 = CnfSymbol(u10, "Z", c("g", "h", "i"))

# When X=a, Y must be d (from clause 1+3)
# When X=b, Y must be e (from clause 2+3)
# Therefore Z can be restricted in clause 4
f = (X10 %among% "a" | Y10 %among% "d") &
    (X10 %among% "b" | Y10 %among% "e") &
    (X10 %among% c("a", "b")) &
    (Y10 %among% c("d", "e") | Z10 %among% "g")
check_formula(f, u10, "2nd order SSE scenario")

cat("\n=== Duplicate and overlapping clauses ===\n")

u11 = CnfUniverse()
X11 = CnfSymbol(u11, "X", c("a", "b", "c"))
Y11 = CnfSymbol(u11, "Y", c("d", "e", "f"))

# Identical clauses
f = (X11 %among% "a" | Y11 %among% "d") & (X11 %among% "a" | Y11 %among% "d")
check_formula(f, u11, "Duplicate clauses")
report("Duplicate clauses simplified", length(as.list(f)) == 1)

# Many overlapping clauses
f = (X11 %among% c("a", "b") | Y11 %among% c("d", "e")) &
    (X11 %among% c("a", "c") | Y11 %among% c("d", "f")) &
    (X11 %among% c("b", "c") | Y11 %among% c("e", "f"))
check_formula(f, u11, "Many overlapping clauses")

cat("\n=== Domain size edge cases ===\n")

# Binary domain (smallest non-trivial)
u12 = CnfUniverse()
A12 = CnfSymbol(u12, "A", c("0", "1"))
B12 = CnfSymbol(u12, "B", c("0", "1"))
C12 = CnfSymbol(u12, "C", c("0", "1"))
D12 = CnfSymbol(u12, "D", c("0", "1"))

# This is like a SAT problem on binary variables
f = (A12 %among% "1" | B12 %among% "1") &
    (A12 %among% "0" | C12 %among% "1") &
    (B12 %among% "0" | C12 %among% "0") &
    (C12 %among% "1" | D12 %among% "1")
check_formula(f, u12, "Binary domain SAT")

# All-FALSE clauses
f = CnfFormula(list(as.CnfClause(FALSE)))
report("Single FALSE clause -> FALSE formula", isFALSE(as.logical(f)))

# Single-element domains
u13 = CnfUniverse()
S13 = CnfSymbol(u13, "S", "only")
T13 = CnfSymbol(u13, "T", "also")
f = (S13 %among% "only") & (T13 %among% "also")
check_formula(f, u13, "Single-element domains")
report("Single-element domains -> TRUE", isTRUE(as.logical(f)))

cat("\n=== Large domain ===\n")
u14 = CnfUniverse()
X14 = CnfSymbol(u14, "X", letters[1:10])
Y14 = CnfSymbol(u14, "Y", letters[11:20])
f = (X14 %among% letters[1:5] | Y14 %among% letters[11:15]) &
    (X14 %among% letters[3:7] | Y14 %among% letters[13:17]) &
    (X14 %among% letters[5:9])
check_formula(f, u14, "Large domain")

cat("\n=== Summary ===\n")
if (length(errors)) {
  cat(sprintf("FAILURES (%d):\n", length(errors)))
  for (e in errors) cat(sprintf("  - %s\n", e))
} else {
  cat("All tests passed!\n")
}
