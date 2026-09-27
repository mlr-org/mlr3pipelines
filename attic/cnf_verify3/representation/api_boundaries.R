# Standalone public-API representation reproducers. Only checkmate is needed.
# R 3.6: Rscript attic/cnf_verify3/representation/api_boundaries.R
# R 4.6 container: add its local checkmate library through CNF_REP_RLIB.
if (nzchar(Sys.getenv("CNF_REP_RLIB"))) .libPaths(c(Sys.getenv("CNF_REP_RLIB"), .libPaths()))
suppressMessages(library(checkmate))
for (nm in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(nm, ".R")))
}
cat("R: ", R.version.string, "\ncheckmate: ", as.character(packageVersion("checkmate")), "\n", sep = "")

u = CnfUniverse()
x = CnfSymbol(u, "X", c("a", "b"))
y = CnfSymbol(u, "Y", c("a", "b"))
a = CnfAtom(x, "a")
cl = CnfClause(list(a, CnfAtom(y, "b")))

cat("\nNEW: reverse logical OR drops CnfClause class\n")
left = TRUE | cl
right = cl | TRUE
stopifnot(isTRUE(left), isTRUE(right), identical(class(left), "logical"), inherits(right, "CnfClause"))
cat("class(TRUE | cl): ", class(left), "\nclass(cl | TRUE): ", class(right), "\n", sep = "")
error = tryCatch({ CnfFormula(list(left)); NULL }, error = conditionMessage)
stopifnot(is.character(error), grepl("element 1 has type 'logical'", error, fixed = TRUE))
cat("CnfFormula(list(TRUE | cl)): ", error, "\n", sep = "")
stopifnot(isTRUE(as.logical(CnfFormula(list(right)))))
cat("CnfFormula(list(cl | TRUE)): succeeds\n")
# A single atom's OR method already coerces the same short-circuit result.
stopifnot(inherits(TRUE | a, "CnfClause"))
cat("class(TRUE | atom): ", class(TRUE | a), "\n", sep = "")

cat("\nKNOWN (attic/cnf/review_5_2_pro.md): as.list loses symbol names\n")
atoms = as.list(cl)
stopifnot(is.null(names(atoms)), identical(names(cl), c("X", "Y")), is.null(atoms$X))
cat("names(as.list(cl)): NULL; names(cl): ", paste(names(cl), collapse = ", "), "\n", sep = "")

cat("\nNEW: a missing logical subscript creates a malformed CnfClause\n")
bad = cl[c(FALSE, NA)]
stopifnot(inherits(bad, "CnfClause"), length(bad) == 1L, anyNA(names(bad)), is.null(bad[[1L]]))
cat("cl[c(FALSE, NA)] is a CnfClause with one NA symbol name and a NULL range.\n")
accepted = CnfFormula(list(bad))
stopifnot(inherits(accepted, "CnfFormula"), is.na(as.logical(accepted)))
cat("CnfFormula accepts the malformed clause as a nonconstant formula.\n")
roundtrip = CnfClause(as.list(bad))
stopifnot(isTRUE(as.logical(roundtrip)))
cat("CnfClause(as.list(bad)) becomes TRUE through vacuous absent-domain coverage.\n")
error = tryCatch({ cl[c(1, NA_real_)]; NULL }, error = conditionMessage)
stopifnot(is.character(error))
cat("The analogous missing numeric selector is rejected.\n")

cat("\nNEW: repeated matrix indices create duplicate symbols and a simplifier error\n")
matrix_u = CnfUniverse()
matrix_x = CnfSymbol(matrix_u, "X", c("a", "b", "c"))
matrix_cl = as.CnfClause(CnfAtom(matrix_x, c("a", "b")))
matrix_dup = matrix_cl[matrix(c(1, 1), nrow = 1L)]
stopifnot(identical(names(matrix_dup), c("X", "X")), identical(matrix_dup[[1L]], matrix_dup[[2L]]))
matrix_other = as.CnfClause(CnfAtom(matrix_x, c("a", "c")))
error = tryCatch({ CnfFormula(list(matrix_dup, matrix_other)); NULL }, error = conditionMessage)
stopifnot(is.character(error), grepl("attempt to select less than one element", error, fixed = TRUE))
cat("Repeated matrix selector creates two X occurrences; simplification errors: ", error, "\n", sep = "")
matrix_expected = CnfFormula(list(matrix_cl, matrix_other))
stopifnot(identical(c(matrix_expected), list(list(X = "a"))))
cat("The unduplicated formula simplifies to the single unit X in {a}.\n")

cat("\nREPRESENTATION BOUNDARY: byte-marked values and Unicode formatting\n")
byte_value = rawToChar(as.raw(255L))
Encoding(byte_value) = "bytes"
byte_u = CnfUniverse()
byte_x = CnfSymbol(byte_u, "X", c("a", byte_value))
byte_atom = CnfAtom(byte_x, byte_value)
stopifnot(inherits(byte_atom, "CnfAtom"), is.na(as.logical(byte_atom)))
error = tryCatch({ capture.output(print(byte_atom)); NULL }, error = conditionMessage)
stopifnot(is.character(error), grepl('translating strings with "bytes" encoding is not allowed', error, fixed = TRUE))
cat("Accepted atom print error: ", error, "\n", sep = "")
# Logical manipulation of the same ASCII/bytes domain succeeds.
byte_formula = as.CnfFormula(byte_atom)
stopifnot(isFALSE(as.logical(byte_formula & !byte_formula)))
cat("The same atom converts to a formula and formula & !formula is FALSE.\n")

unicode_value = enc2utf8("\u00e9")
unicode_u = CnfUniverse()
unicode_x = CnfSymbol(unicode_u, "X", c(unicode_value, byte_value))
error = tryCatch({ CnfAtom(unicode_x, unicode_value); NULL }, error = conditionMessage)
stopifnot(is.character(error), grepl('translating strings with "bytes" encoding is not allowed', error, fixed = TRUE))
cat("Accepted mixed Unicode/bytes symbol, valid-subset atom error: ", error, "\n", sep = "")
cat("\nAll reported outcomes reproduced.\n")
