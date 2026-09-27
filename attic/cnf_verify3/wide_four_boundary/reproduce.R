# Self-contained public family reproduction, repository root as working dir.
# Optional first argument: number of common fresh padding literals (default 1).
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}
args = commandArgs(trailingOnly = TRUE)
padding = if (length(args)) as.integer(args[[1L]]) else 1L
assert_count(padding, positive = TRUE)
universe = CnfUniverse()
X = CnfSymbol(universe, "X", c("0", "1", "4", "5"))
Y = CnfSymbol(universe, "Y", c("0", "1", "4", "5"))
pad_names = paste0("P", seq_len(padding))
padding_atoms = lapply(pad_names, function(name) {
  CnfAtom(CnfSymbol(universe, name, c("off", "on")), "on")
})
make_clause = function(atoms) CnfClause(c(atoms, padding_atoms))
input = list(
  make_clause(list(Y %among% c("0", "5"), X %among% c("0", "5"))),
  make_clause(list(X %among% "1", Y %among% c("0", "1", "4"))),
  make_clause(list(X %among% "0", Y %among% c("4", "0", "5"))),
  make_clause(list(Y %among% "0", X %among% c("4", "5")))
)
first = CnfFormula(input)
second = CnfFormula(as.list(first))
strip_padding = function(formula) lapply(c(formula), function(clause) clause[!names(clause) %in% pad_names])
stopifnot(identical(strip_padding(first), list(list(Y = "0", X = "5"), list(X = "0", Y = c("4", "0")))),
  identical(strip_padding(second), list(list(Y = "0", X = "5"), list(Y = c("4", "0")))),
  all(lengths(input) == padding + 2L), !identical(first, second))
cat("R", as.character(getRversion()), "four input clauses of width", padding + 2L, "\n")
cat("First output, with common padding omitted:\n")
dput(strip_padding(first))
cat("Second output, with common padding omitted:\n")
dput(strip_padding(second))
cat("The second pass removes the X=0 literal from clause 2; all padding remains.\n")
