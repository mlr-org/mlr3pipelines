# Coordinator reproduction, independent of the agents' data/evaluation helpers.
# Run from repository root. The assertion of a discrepancy is deliberate:
# this script records the unchanged source's observed incorrect result.
suppressPackageStartupMessages({
  library(checkmate)
  library(mlr3misc)
})
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("a", "b", "c"))
c1 = as.CnfClause(CnfAtom(X, c("b", "c")))
c2 = CnfClause(list(CnfAtom(X, "b"), CnfAtom(Y, "c")))
c3 = CnfClause(list(CnfAtom(X, "c"), CnfAtom(Y, "b")))
c4 = CnfClause(list(CnfAtom(X, "a"), CnfAtom(Y, "a")))

selected_matrix = c1[matrix(c(1L, 1L), nrow = 1L)]
selected_vector = c1[c(1L, 1L)]
stopifnot(identical(names(selected_matrix), c("X", "X")))
stopifnot(identical(names(selected_vector), "X"))
bad = CnfFormula(list(selected_matrix, c2, c3, c4))
good = CnfFormula(list(selected_vector, c2, c3, c4))
normalized = CnfFormula(list(CnfClause(list(selected_matrix)), c2, c3, c4))
stopifnot(isFALSE(as.logical(good)), isFALSE(as.logical(normalized)))

valuations = expand.grid(X = c("a", "b", "c"), Y = c("a", "b", "c"), stringsAsFactors = FALSE)
expected = with(valuations,
  (X %in% c("b", "c")) & (X == "b" | Y == "c") &
    (X == "c" | Y == "b") & (X == "a" | Y == "a"))
actual = rep(TRUE, nrow(valuations))
for (clause in unclass(bad)) {
  clause_value = rep(FALSE, nrow(valuations))
  for (i in seq_along(clause)) {
    clause_value = clause_value | valuations[[names(clause)[[i]]]] %in% clause[[i]]
  }
  actual = actual & clause_value
}
stopifnot(!any(expected), identical(which(actual != expected), 3L))
cat(R.version.string, "\n")
print(bad)
print(data.frame(valuations, expected = expected, actual = actual))
