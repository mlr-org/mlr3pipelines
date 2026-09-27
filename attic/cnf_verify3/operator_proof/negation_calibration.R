source("attic/cnf_verify3/operator_proof/bootstrap.R")

# Independent replay of review_semantics' opposed four-edge cycles.  The first
# cycle forces x = 1, and the second forces x = 0.  Keep a local explicit copy
# rather than importing either that stream's result or its semantic checker.
cycle_domains = setNames(rep(list(c("0", "1")), 7L),
  c("x", "a1", "a2", "a3", "b1", "b2", "b3"))
cycle_clauses = list(list(x = "1", a1 = "1"), list(a1 = "0", a2 = "1"),
  list(a2 = "0", a3 = "1"), list(a3 = "0", x = "1"),
  list(x = "0", b1 = "1"), list(b1 = "0", b2 = "1"),
  list(b2 = "0", b3 = "1"), list(b3 = "0", x = "0"))
cycle_universe = make_universe(cycle_domains)
cycle_assignments = expand.grid(cycle_domains, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
stopifnot(nrow(cycle_assignments) == 128L,
  !any(evaluate_formula(cycle_clauses, cycle_assignments)))
for (i in seq_along(cycle_clauses)) {
  stopifnot(any(evaluate_formula(cycle_clauses[-i], cycle_assignments)))
}
cycle_formula = CnfFormula(lapply(cycle_clauses, make_clause, universe = cycle_universe))
stopifnot(!is.logical(cycle_formula), length(cycle_formula) == 8L,
  identical(canonical_formula(cycle_formula), canonical_formula(cycle_clauses)))
cycle_before = serialize(cycle_formula, NULL)
cycle_negated = !cycle_formula
stopifnot(isTRUE(c(cycle_negated)), isFALSE(c(!cycle_negated)),
  identical(cycle_before, serialize(cycle_formula, NULL)))
cat("Opposed four-edge cycles: 128 assignments; zero models; every clause indispensable.\n")
cat("Production preserves all eight clauses; !formula is TRUE; !!formula is FALSE.\n")

# The three Boolean variables must be pairwise different: impossible with two values.
u = make_universe(setNames(rep(list(c("0", "1")), 3L), c("X", "Y", "Z")))
clauses = list()
for (pair in combn(c("X", "Y", "Z"), 2L, simplify = FALSE)) {
  for (value in c("0", "1")) {
    clauses[[length(clauses) + 1L]] = make_clause(setNames(rep(list(value), 2L), pair), u)
  }
}
formula = CnfFormula(clauses)
assignments = expand.grid(mget(names(u), envir = u), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
stopifnot(!any(evaluate_formula(formula, assignments)))
cat("Triangle contradiction is stored as", if (is.logical(formula)) c(formula) else length(formula), "\n")
negated = !formula
stopifnot(isTRUE(c(negated)))
cat("Its negation is literal TRUE.\n")

# Recheck the documentation's stated unrecognized ternary contradiction.
v = make_universe(list(X = c("a", "b", "c"), Y = c("d", "e", "f")))
documented = CnfFormula(list(make_clause(list(X = "a", Y = "d"), v),
  make_clause(list(X = "b", Y = "e"), v), make_clause(list(X = "c", Y = "f"), v)))
stopifnot(isFALSE(c(documented)))
cat("The documented three-clause example is already literal FALSE in current source.\n")
