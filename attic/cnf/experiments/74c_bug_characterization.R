#!/usr/bin/env Rscript
# Detailed characterization of Bugs #3 and #4
source("experiments/test_harness.R")

cat("======================================\n")
cat("Bug #3: TRUE/FALSE clause with NULL universe at beginning of list\n")
cat("======================================\n\n")

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a", "b", "c"))
B = CnfSymbol(u, "B", c("x", "y"))

# Root cause: as.CnfClause(TRUE) and as.CnfClause(FALSE) have universe = NULL
# CnfFormula constructor at line 151: universe = attr(clauses[[1]], "universe")
# When clauses[[1]] is TRUE, universe becomes NULL
# Then when a real clause comes next, identical(real_universe, NULL) -> FALSE -> error

cat("1. as.CnfClause(TRUE) with NULL universe + normal clause:\n")
cat(sprintf("   as.CnfClause(TRUE) universe is NULL: %s\n",
  is.null(attr(as.CnfClause(TRUE), "universe"))))
r = tryCatch(CnfFormula(list(as.CnfClause(TRUE), as.CnfClause(A %among% "a"))),
  error = function(e) e)
cat(sprintf("   Result: %s\n\n", if (inherits(r, "error")) r$message else "OK"))

cat("2. Normal clause + TRUE clause (this works because universe is set from first clause):\n")
r = tryCatch(CnfFormula(list(as.CnfClause(A %among% "a"), as.CnfClause(TRUE))),
  error = function(e) e)
cat(sprintf("   Result: %s\n\n", if (inherits(r, "error")) r$message else "OK"))

cat("3. as.CnfClause(FALSE) first (FALSE breaks before universe check, so this is OK):\n")
r = tryCatch(CnfFormula(list(as.CnfClause(FALSE), as.CnfClause(A %among% "a"))),
  error = function(e) e)
cat(sprintf("   Result: %s (should be FALSE formula)\n", if (inherits(r, "error")) r$message else as.logical(r)))

cat("4. Tautological clause from atom (has universe because constructed via CnfClause):\n")
taut = CnfClause(list(A %among% c("a", "b", "c")))
cat(sprintf("   Tautological clause universe is NULL: %s\n", is.null(attr(taut, "universe"))))
cat(sprintf("   isTRUE(unclass(taut)): %s\n", isTRUE(unclass(taut))))
r = tryCatch(CnfFormula(list(taut, as.CnfClause(A %among% "a"))),
  error = function(e) e)
cat(sprintf("   Result: %s\n\n", if (inherits(r, "error")) r$message else "OK"))

cat("5. as.CnfFormula(TRUE) (also NULL universe) + normal clause in CnfFormula():\n")
f_true = as.CnfFormula(TRUE)
cat(sprintf("   as.CnfFormula(TRUE) universe is NULL: %s\n",
  is.null(attr(f_true, "universe"))))
r = tryCatch(CnfFormula(list(f_true, as.CnfClause(A %among% "a"))),
  error = function(e) e)
cat(sprintf("   Result: %s\n\n", if (inherits(r, "error")) r$message else "OK"))

cat("6. Multiple TRUE clauses only:\n")
r = tryCatch(CnfFormula(list(as.CnfClause(TRUE), as.CnfClause(TRUE))),
  error = function(e) e)
cat(sprintf("   Result: %s\n\n", if (inherits(r, "error")) r$message else paste("OK, as.logical =", as.logical(r))))

cat("\n======================================\n")
cat("Bug #4: FALSE CnfFormula in list after other CnfFormulas\n")
cat("======================================\n\n")

# Root cause: In CnfFormula constructor, when iterating over clauses:
# - CnfFormula items go into other_entries (line 172)
# - FALSE items set entries = FALSE and break (line 155-156)
# After loop: if other_entries has items, entries = c(entries, unlist(other_entries))
# c(FALSE, list_of_clauses) creates a malformed list where first element is FALSE
# This gets passed to simplify_cnf which crashes

cat("1. Reproduce: list(formula, formula, FALSE_formula):\n")
u2 = CnfUniverse()
X = CnfSymbol(u2, "X", c("a", "b", "c"))
Y = CnfSymbol(u2, "Y", c("x", "y"))

f1 = CnfFormula(list(as.CnfClause(X %among% c("a", "b"))))
f2 = CnfFormula(list(as.CnfClause(Y %among% "x")))
f_false = as.CnfFormula(FALSE)
# Give it a matching universe
attr(f_false, "universe") = u2

r = tryCatch(CnfFormula(list(f1, f2, f_false)), error = function(e) e)
cat(sprintf("   Result: %s\n\n", if (inherits(r, "error")) r$message else paste("OK, as.logical =", as.logical(r))))

cat("2. Check what c(FALSE, clause_list) produces:\n")
clause_list = list(list(X = c("a", "b")), list(Y = "x"))
result = c(FALSE, clause_list)
cat(sprintf("   c(FALSE, clause_list) class: %s, length: %d\n", class(result), length(result)))
cat(sprintf("   result[[1]]: %s\n", result[[1]]))
cat(sprintf("   result[[2]] names: %s\n", paste(names(result[[2]]), collapse=", ")))

cat("\n3. The fix would be to check for isFALSE(entries) BEFORE the other_entries merge:\n")
cat("   At line 175, before 'if (length(other_entries))', add:\n")
cat("   if (isFALSE(entries)) return(structure(FALSE, universe = universe, class = 'CnfFormula'))\n")

cat("\n4. For Bug #3, the fix would be to extract universe from the first NON-logical clause:\n")
cat("   Instead of always using clauses[[1]]'s universe, scan for the first clause with a universe.\n")

cat("\n\n======================================\n")
cat("Summary of all bugs found\n")
cat("======================================\n")
cat("Bug #1: as.list.CnfClause crashes on TRUE clauses (exp 02)\n")
cat("Bug #2: u[['NonExistent']] silently returns NULL (exp 09)\n")
cat("Bug #3: CnfFormula() fails when first element is a TRUE clause with NULL universe (NEW)\n")
cat("Bug #4: CnfFormula() crashes when FALSE CnfFormula is in list after other CnfFormulas (NEW)\n")
