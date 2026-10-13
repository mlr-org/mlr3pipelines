suppressMessages(library(checkmate))
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}

make_universe = function(domains) {
  universe = CnfUniverse()
  for (s in names(domains)) CnfSymbol(universe, s, domains[[s]])
  universe
}

make_clause = function(clause, universe) {
  CnfClause(lapply(names(clause), function(s) {
    CnfAtom(structure(s, universe = universe, class = "CnfSymbol"), clause[[s]])
  }))
}

evaluate_clause = function(clause, assignments) {
  if (is.logical(clause)) return(rep(c(clause), nrow(assignments)))
  Reduce(`|`, lapply(names(clause), function(s) assignments[[s]] %in% clause[[s]]),
    init = rep(FALSE, nrow(assignments)))
}

evaluate_formula = function(formula, assignments) {
  if (is.logical(formula)) return(rep(c(formula), nrow(assignments)))
  if (inherits(formula, "CnfAtom")) return(assignments[[formula$symbol]] %in% formula$values)
  if (inherits(formula, "CnfClause")) return(evaluate_clause(c(formula), assignments))
  Reduce(`&`, lapply(c(formula), evaluate_clause, assignments = assignments),
    init = rep(TRUE, nrow(assignments)))
}

canonical_clause = function(clause) {
  lapply(clause[order(names(clause))], sort)
}

canonical_formula = function(formula) {
  if (is.logical(formula)) return(c(formula))
  clauses = lapply(c(formula), canonical_clause)
  keys = vapply(clauses, function(clause) paste(capture.output(dput(clause)), collapse = ""), character(1))
  clauses[order(keys)]
}

assert_canonical = function(formula, universe) {
  stopifnot(inherits(formula, "CnfFormula"))
  if (is.logical(formula)) {
    stopifnot(length(formula) == 1L, !is.na(formula))
    return(invisible(NULL))
  }
  stopifnot(length(formula) > 0L, identical(attr(formula, "universe"), universe))
  for (clause in c(formula)) {
    stopifnot(length(clause) > 0L, !anyDuplicated(names(clause)), !anyNA(names(clause)))
    for (s in names(clause)) {
      stopifnot(is.character(clause[[s]]), length(clause[[s]]) > 0L,
        !anyNA(clause[[s]]), !anyDuplicated(clause[[s]]),
        all(clause[[s]] %in% universe[[s]]), !all(universe[[s]] %in% clause[[s]]))
    }
  }
  invisible(NULL)
}
