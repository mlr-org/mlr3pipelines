# Read-only review harness. Run scripts from the repository root.
if (nzchar(Sys.getenv("CNF_REP_RLIB"))) .libPaths(c(Sys.getenv("CNF_REP_RLIB"), .libPaths()))
suppressMessages(library(checkmate))
# These two formatting helpers are the only mlr3misc functions used by the CNF
# files. The actual constructors use the real checkmate validators above.
stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
map_chr = function(.x, .f, ...) vapply(.x, .f, character(1), ...)
for (nm in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(nm, ".R")))
}

# This evaluator does no simplification. Every stored occurrence contributes
# separately to the disjunction, even if several occurrences have one name.
clause_truth = function(clause, assignments, by_name = FALSE) {
  if (is.logical(clause)) return(rep(as.vector(clause), nrow(assignments)))
  answer = rep(FALSE, nrow(assignments))
  for (i in seq_along(clause)) {
    symbol = names(clause)[[i]]
    if (is.na(symbol) || !symbol %in% names(assignments)) stop("No truth assignment for symbol.")
    values = if (by_name) clause[[symbol]] else clause[[i]]
    answer = answer | assignments[[symbol]] %in% values
  }
  answer
}
formula_truth = function(formula, assignments, by_name = FALSE) {
  if (is.logical(formula)) return(rep(as.vector(formula), nrow(assignments)))
  answer = rep(TRUE, nrow(assignments))
  for (clause in formula) answer = answer & clause_truth(clause, assignments, by_name)
  answer
}

valid_clause = function(clause, universe) {
  if (is.logical(clause)) return(length(clause) == 1L && !anyNA(clause))
  if (!is.list(clause) || !length(clause)) return(FALSE)
  nm = names(clause)
  if (is.null(nm) || anyNA(nm) || anyDuplicated(nm) || !all(nm %in% names(universe))) return(FALSE)
  all(vapply(seq_along(clause), function(i) {
    r = clause[[i]]
    is.character(r) && length(r) > 0L && !anyNA(r) && !anyDuplicated(as.vector(r)) &&
      is.null(attributes(r)) && all(r %in% universe[[nm[[i]]]])
  }, logical(1)))
}
valid_formula = function(formula, universe) {
  if (is.logical(formula)) return(length(formula) == 1L && !anyNA(formula))
  all(vapply(formula, valid_clause, logical(1), universe = universe))
}

build_case = function(case, dimensional = TRUE) {
  universe = CnfUniverse()
  symbols = lapply(names(case$domains), function(s) CnfSymbol(universe, s, case$domains[[s]]))
  names(symbols) = names(case$domains)
  clauses = lapply(seq_along(case$raw), function(ci) {
    raw = case$raw[[ci]]
    cl = CnfClause(lapply(seq_along(raw), function(si) CnfAtom(symbols[[names(raw)[[si]]]], raw[[si]])))
    selector = case$selectors[[ci]]
    if (dimensional) selector = matrix(selector, nrow = 1L)
    cl[selector]
  })
  list(universe = universe, clauses = clauses)
}

evaluate_case = function(case, dimensional = TRUE) {
  tryCatch({
    assignments = expand.grid(case$domains, stringsAsFactors = FALSE)
    # Input semantics are evaluated directly from selected original atoms,
    # before either the constructor or the simplifier is involved.
    selected_raw = Map(function(cl, i) cl[i], case$raw, case$selectors)
    expected = formula_truth(selected_raw, assignments)
    built = build_case(case, dimensional)
    stopifnot(identical(expected, formula_truth(lapply(built$clauses, unclass), assignments)))
    actual = CnfFormula(built$clauses)
    positional = formula_truth(unclass(actual), assignments)
    named = formula_truth(unclass(actual), assignments, by_name = TRUE)
    list(case = case, output = c(actual), assignments = assignments,
      expected = expected, positional = positional, named = named,
      positional_mismatch = which(expected != positional), named_mismatch = which(expected != named))
  }, error = function(e) list(error = conditionMessage(e)))
}
