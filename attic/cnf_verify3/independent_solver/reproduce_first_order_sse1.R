# Minimal pure-first-order scheduling counterexample, with exact truth checks.
# Rscript attic/cnf_verify3/independent_solver/reproduce_first_order_sse1.R
# For a current-R container, CNF_PROOF_RLIB can name a checkmate installation.
if (nzchar(Sys.getenv("CNF_PROOF_RLIB"))) .libPaths(c(Sys.getenv("CNF_PROOF_RLIB"), .libPaths()))
suppressMessages(library(checkmate))
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}
cat(R.version.string, "\ncheckmate ", as.character(packageVersion("checkmate")), "\n", sep = "")

u = CnfUniverse()
x = CnfSymbol(u, "X", c("0", "1", "4", "5"))
y = CnfSymbol(u, "Y", c("0", "1", "4", "5"))
input = list(
  list(Y = c("0", "5"), X = c("0", "5")),
  list(X = "1", Y = c("0", "1", "4")),
  list(X = "0", Y = c("4", "0", "5")),
  list(Y = "0", X = c("4", "5"))
)
construct = function(clauses) {
  CnfFormula(lapply(clauses, function(clause) {
    CnfClause(lapply(names(clause), function(symbol) {
      CnfAtom(structure(symbol, universe = u, class = "CnfSymbol"), clause[[symbol]])
    }))
  }))
}
first = construct(input)
second = construct(c(first))
stopifnot(identical(c(first), list(list(Y = "0", X = "5"), list(X = "0", Y = c("4", "0")))))
stopifnot(identical(c(second), list(list(Y = "0", X = "5"), list(Y = c("4", "0")))))

# Independent literal definition over all 16 valuations.
truth = function(clauses) {
  grid = expand.grid(X = c("0", "1", "4", "5"), Y = c("0", "1", "4", "5"), stringsAsFactors = FALSE)
  vapply(seq_len(nrow(grid)), function(i) {
    all(vapply(clauses, function(clause) {
      any(vapply(names(clause), function(symbol) grid[[symbol]][[i]] %in% clause[[symbol]], logical(1)))
    }, logical(1)))
  }, logical(1))
}
stopifnot(identical(truth(input), truth(c(first))), identical(truth(input), truth(c(second))))
cat("First result:\n")
dput(c(first))
cat("Second result:\n")
dput(c(second))
cat("All 16 valuations preserved; the second pass removes an applicable first-order literal.\n")
