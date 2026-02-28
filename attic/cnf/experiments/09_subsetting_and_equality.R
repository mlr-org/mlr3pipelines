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
Z = CnfSymbol(u, "Z", c("g", "h", "i"))

cat("=== CnfUniverse [[ accessor ===\n")
report("u[[X]] works", identical(u[["X"]], c("a", "b", "c")))
report("u[[Y]] works", identical(u[["Y"]], c("d", "e", "f")))
result = tryCatch(u[["NonExistent"]], error = function(e) e)
report("u[[nonexistent]] errors", inherits(result, "error"))

cat("\n=== CnfClause [ subsetting ===\n")
clause = X %among% c("a", "b") | Y %among% "d" | Z %among% "g"

# Numeric subsetting
sub1 = clause[1]
report("[1] gives single-symbol clause", length(unclass(sub1)) == 1)
report("[1] is CnfClause", inherits(sub1, "CnfClause"))

sub2 = clause[1:2]
report("[1:2] gives two-symbol clause", length(unclass(sub2)) == 2)

# Character subsetting
sub_x = clause["X"]
report('["X"] works', inherits(sub_x, "CnfClause"))
report('["X"] has correct content', identical(unclass(sub_x)[["X"]], c("a", "b")))

sub_xy = clause[c("X", "Y")]
report('["X","Y"] works', length(unclass(sub_xy)) == 2)

# Empty subsetting
sub_empty = clause[integer(0)]
report("[integer(0)] gives FALSE clause", isFALSE(as.logical(sub_empty)))

sub_zero = clause[0]
report("[0] gives FALSE clause", isFALSE(as.logical(sub_zero)))

# Logical subsetting
sub_log = clause[c(TRUE, FALSE, TRUE)]
report("[c(T,F,T)] works", length(unclass(sub_log)) == 2)

# Full subsetting
sub_all = clause[]
report("[] returns self", isTRUE(all.equal(sub_all, clause)))

# Subset of TRUE clause
clause_T = as.CnfClause(TRUE)
sub_T1 = clause_T[1]
report("TRUE[1] returns TRUE", isTRUE(as.logical(sub_T1)))

sub_T0 = clause_T[0]
report("TRUE[0] returns FALSE", isFALSE(as.logical(sub_T0)))

# Subset of FALSE clause
clause_F = as.CnfClause(FALSE)
sub_F0 = clause_F[0]
report("FALSE[0] returns FALSE", isFALSE(as.logical(sub_F0)))
sub_Fe = clause_F[integer(0)]
report("FALSE[integer(0)] returns FALSE", isFALSE(as.logical(sub_Fe)))

result = tryCatch(clause_F[1], error = function(e) e)
report("FALSE[1] errors", inherits(result, "error"))

cat("\n=== CnfClause [[ accessor ===\n")
report('clause[["X"]] gives values', identical(clause[["X"]], c("a", "b")))
report('clause[["Y"]] gives values', identical(clause[["Y"]], "d"))

cat("\n=== all.equal methods ===\n")

# CnfAtom equality
a1 = X %among% c("a", "b")
a2 = X %among% c("b", "a")  # same but different order
report("atom all.equal same", isTRUE(all.equal(a1, a2)))

a3 = X %among% c("a", "c")
report("atom all.equal different", !isTRUE(all.equal(a1, a3)))

# TRUE atoms
aT1 = as.CnfAtom(TRUE)
aT2 = X %among% c("a", "b", "c")  # also TRUE
report("TRUE atoms equal", isTRUE(all.equal(aT1, aT2)))

# FALSE atoms
aF1 = as.CnfAtom(FALSE)
aF2 = X %among% character(0)
report("FALSE atoms equal", isTRUE(all.equal(aF1, aF2)))

report("TRUE != FALSE atom", !isTRUE(all.equal(aT1, aF1)))

# CnfClause equality
c1 = X %among% c("a", "b") | Y %among% "d"
c2 = Y %among% "d" | X %among% c("b", "a")  # reordered
report("clause all.equal reordered", isTRUE(all.equal(c1, c2)))

# CnfFormula equality
f1 = (X %among% "a" | Y %among% "d") & (Z %among% c("g", "h"))
f2 = (Z %among% c("h", "g")) & (Y %among% "d" | X %among% "a")  # reordered
report("formula all.equal reordered", isTRUE(all.equal(f1, f2)))

cat("\n=== Mixed universe errors ===\n")
u2 = CnfUniverse()
X2 = CnfSymbol(u2, "X", c("a", "b", "c"))

result = tryCatch(X %among% "a" | X2 %among% "a", error = function(e) e)
report("different universe | errors", inherits(result, "error"))

result = tryCatch(X %among% "a" & X2 %among% "a", error = function(e) e)
report("different universe & errors", inherits(result, "error"))

cat("\n=== Edge: CnfClause with conflicting atom (same symbol, different values in |) ===\n")
# X in {a} | X in {b} should simplify to X in {a, b}
c_dup = X %among% "a" | X %among% "b"
report("same symbol OR unifies", length(unclass(c_dup)) == 1)
report("same symbol OR correct values", setequal(unclass(c_dup)[["X"]], c("a", "b")))

# X in {a} | X in {a,b,c} should be TRUE (full domain)
c_full = X %among% "a" | X %among% c("a", "b", "c")
report("full domain OR is TRUE", isTRUE(as.logical(c_full)))

cat("\n=== Edge: Formula with empty clause list ===\n")
f_empty = CnfFormula(list())
report("CnfFormula(list()) is TRUE", isTRUE(as.logical(f_empty)))

# CnfClause with empty atoms list
c_empty = CnfClause(list())
report("CnfClause(list()) is FALSE", isFALSE(as.logical(c_empty)))

cat("\n=== Summary ===\n")
if (length(errors)) {
  cat(sprintf("FAILURES (%d):\n", length(errors)))
  for (e in errors) cat(sprintf("  - %s\n", e))
} else {
  cat("All tests passed!\n")
}
