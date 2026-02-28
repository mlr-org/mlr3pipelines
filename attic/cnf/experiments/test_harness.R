# Test harness: source Cnf*.R files with minimal dependencies
# We need checkmate and mlr3misc for assert_* and map_chr/stopf
library(checkmate)
library(mlr3misc)

cnf_dir = "/home/binderm/pipelines_try/mlr3pipelines/R"
source(file.path(cnf_dir, "CnfUniverse.R"))
source(file.path(cnf_dir, "CnfSymbol.R"))
source(file.path(cnf_dir, "CnfAtom.R"))
source(file.path(cnf_dir, "CnfClause.R"))
source(file.path(cnf_dir, "CnfFormula_simplify.R"))
source(file.path(cnf_dir, "CnfFormula.R"))

# Helper: evaluate a CnfFormula against all possible assignments, returning a truth vector
# Returns a named logical vector where each entry corresponds to one assignment
evaluate_formula = function(formula, universe) {
  varnames = ls(universe)
  if (!length(varnames)) return(TRUE)

  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  colnames(assignments) = varnames

  formula_bare = unclass(formula)

  if (isTRUE(formula_bare)) return(rep(TRUE, nrow(assignments)))
  if (isFALSE(formula_bare)) return(rep(FALSE, nrow(assignments)))

  # For a proper formula (list of clauses):
  # The formula is TRUE iff ALL clauses are TRUE
  # A clause is TRUE iff at least one atom is TRUE
  # An atom (symbol, values) is TRUE iff the assignment for that symbol is in values
  result = rep(TRUE, nrow(assignments))
  for (clause in unclass(formula)) {
    clause_result = rep(FALSE, nrow(assignments))
    for (sym in names(clause)) {
      clause_result = clause_result | (assignments[[sym]] %in% clause[[sym]])
    }
    result = result & clause_result
  }
  result
}

# Same but for a CnfClause
evaluate_clause = function(clause, universe) {
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  colnames(assignments) = varnames

  clause_bare = unclass(clause)
  if (isTRUE(clause_bare)) return(rep(TRUE, nrow(assignments)))
  if (isFALSE(clause_bare)) return(rep(FALSE, nrow(assignments)))

  result = rep(FALSE, nrow(assignments))
  for (sym in names(clause_bare)) {
    result = result | (assignments[[sym]] %in% clause_bare[[sym]])
  }
  result
}

# Helper: build a CnfFormula directly from a list of clauses (each clause is a named list of ranges)
# This bypasses simplification to create "raw" formulas for comparison
make_raw_formula = function(clauses, universe) {
  structure(clauses, universe = universe, class = "CnfFormula")
}

# Helper: check if two formulas are semantically equivalent
formulas_equivalent = function(f1, f2, universe) {
  v1 = evaluate_formula(f1, universe)
  v2 = evaluate_formula(f2, universe)
  all(v1 == v2)
}

cat("Test harness loaded successfully.\n")
