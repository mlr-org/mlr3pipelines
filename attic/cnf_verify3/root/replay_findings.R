# Independent public-constructor replay of the reduced scheduling examples.
# Run from repository root. This intentionally asserts semantic preservation,
# and reports the separate first/second-pass structural difference.
suppressPackageStartupMessages({
  library(checkmate)
  library(mlr3misc)
  library(jsonlite)
})
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}

evaluate = function(clauses, assignments) {
  if (is.logical(clauses)) return(rep(as.vector(clauses), nrow(assignments)))
  answer = rep(TRUE, nrow(assignments))
  for (clause in clauses) {
    satisfied = rep(FALSE, nrow(assignments))
    for (symbol in names(clause)) {
      satisfied = satisfied | assignments[[symbol]] %in% clause[[symbol]]
    }
    answer = answer & satisfied
  }
  answer
}

weight = function(formula) {
  if (is.logical(formula)) return(0L)
  sum(vapply(formula, function(clause) sum(lengths(clause)), 0L))
}

fixtures = c(
  "minimized_subsumption.json",
  "minimized_sse1.json",
  "minimized_first_order_phase_sse1.json",
  "minimized_sse2.json",
  "directed_oneend_shrink_min.json",
  "minimized_oneend_symbol_removal.json",
  "minimized_deferred_skip.json"
)

cat(R.version.string, "\n")
for (fixture in fixtures) {
  data = fromJSON(file.path("attic/cnf_verify3/independent_solver", fixture), simplifyVector = FALSE)
  domains = lapply(data$domains, unlist, use.names = FALSE)
  clauses = lapply(data$clauses, function(clause) lapply(clause, unlist, use.names = FALSE))
  universe = CnfUniverse()
  symbols = lapply(names(domains), function(symbol) CnfSymbol(universe, symbol, domains[[symbol]]))
  names(symbols) = names(domains)
  constructed = lapply(clauses, function(clause) {
    CnfClause(lapply(names(clause), function(symbol) CnfAtom(symbols[[symbol]], clause[[symbol]])))
  })
  first = CnfFormula(constructed)
  second = CnfFormula(as.list(first))
  assignments = expand.grid(domains, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  expected = evaluate(clauses, assignments)
  stopifnot(identical(evaluate(unclass(first), assignments), expected))
  stopifnot(identical(evaluate(unclass(second), assignments), expected))
  cat(fixture, ":", nrow(assignments), "valuations,", sum(expected), "models; value occurrences",
    weight(clauses), "->", weight(unclass(first)), "->", weight(unclass(second)), "\n")
}
