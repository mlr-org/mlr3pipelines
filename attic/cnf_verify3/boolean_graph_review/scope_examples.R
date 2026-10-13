source("attic/cnf_verify3/operator_proof/bootstrap.R")

# The Boolean theorem cannot be extended by merely replacing complement
# literals with complement ranges.  Here complement-range vertices only point
# to singleton-range vertices, which have no outgoing edges; there is no seed.
u = make_universe(list(x = c("0", "1", "2"), y = c("0", "1", "2")))
raw = lapply(c("0", "1", "2"), function(v) list(x = v, y = v))
formula = CnfFormula(lapply(raw, make_clause, universe = u))
stopifnot(isFALSE(c(formula)))
range_vertex = function(symbol, range) paste(symbol, paste(sort(range), collapse = ","), sep = ":")
edge_sources = edge_targets = character()
for (clause in raw) for (source in names(clause)) {
  target = setdiff(names(clause), source)
  edge_sources = c(edge_sources, range_vertex(source, setdiff(u[[source]], clause[[source]])))
  edge_targets = c(edge_targets, range_vertex(target, clause[[target]]))
  stopifnot(source != target)
}
# No target vertex is a source vertex, so all paths have length at most one.
# Every edge changes symbols, so none joins a range's complement to itself.
stopifnot(length(edge_sources) == 6L, !any(edge_targets %in% edge_sources))
cat("Ternary three-clause formula: FALSE, despite no complement-range path seed.\n")

# A broad multivalued unit can be created by SSE1 and subsequently deleted by
# unit HLA.  Instrument unit births and the HLA boundary in an in-memory copy.
text = paste(readLines("R/CnfFormula_simplify.R", warn = FALSE), collapse = "\n")
text = sub("register_unit = function(unit_idx) {",
  "register_unit = function(unit_idx) {\n .record_unit(entries[[unit_idx]])", text, fixed = TRUE)
text = sub("  # Now for the big one: Asymmetric Hidden Literal Addition (Marijn et al.)",
  "  .record_boundary(entries[!eliminated])\n  # Now for the big one: Asymmetric Hidden Literal Addition (Marijn et al.)",
  text, fixed = TRUE)
state = new.env(parent = emptyenv())
state$births = list()
private = new.env(parent = .GlobalEnv)
private$.record_unit = function(unit) state$births[[length(state$births) + 1L]] = unit
private$.record_boundary = function(entries) state$boundary = entries
eval(parse(text = text), envir = private)
v = make_universe(list(x = c("0", "1", "2"), y = c("0", "1"), q = c("0", "1")))
clauses = list(list(x = c("0", "1"), q = "0"), list(x = c("0", "1"), q = "1"),
  list(x = "0", y = "0"), list(x = "1", y = "1"))
result = private$simplify_cnf(clauses, v)
ordinary = simplify_cnf(clauses, v)
stopifnot(identical(c(result), c(ordinary)),
  any(vapply(state$births, function(unit) identical(unit, list(x = c("0", "1"))), logical(1))),
  any(vapply(state$boundary, function(clause) identical(clause, list(x = c("0", "1"))), logical(1))),
  identical(canonical_formula(result), canonical_formula(clauses[3:4])),
  all(lengths(c(result)) == 2L))
assignments = expand.grid(mget(names(v), envir = v), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
stopifnot(identical(evaluate_formula(clauses, assignments), evaluate_formula(result, assignments)))
cat("Multivalued broad unit x in {0,1}: created, present before HLA, then removed; semantics unchanged.\n")

# A single four-edge forcing cycle entails x but never exposes x as a unit.
w = make_universe(setNames(rep(list(c("0", "1")), 4L), c("x", "a1", "a2", "a3")))
cycle = list(list(x = "1", a1 = "1"), list(a1 = "0", a2 = "1"),
  list(a2 = "0", a3 = "1"), list(a3 = "0", x = "1"))
cycle_formula = CnfFormula(lapply(cycle, make_clause, universe = w))
valuations = expand.grid(mget(names(w), envir = w), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
models = evaluate_formula(cycle, valuations)
stopifnot(any(models), all(valuations$x[models] == "1"),
  identical(canonical_formula(cycle_formula), canonical_formula(cycle)),
  all(lengths(c(cycle_formula)) == 2L))
cat("Boolean four-edge forcing cycle: satisfiable, entails x = 1, produces no unit.\n")

# Oppose the cycle at x.  Every individual clause is necessary, so a sound
# clause-elimination rule cannot remove one, even though the conjunction is
# unsatisfiable.  The triangle-free support also excludes productive SSE1/2.
z = make_universe(setNames(rep(list(c("0", "1")), 7L),
  c("x", "a1", "a2", "a3", "b1", "b2", "b3")))
opposed = c(cycle, list(list(x = "0", b1 = "1"), list(b1 = "0", b2 = "1"),
  list(b2 = "0", b3 = "1"), list(b3 = "0", x = "0")))
opposed_formula = CnfFormula(lapply(opposed, make_clause, universe = z))
valuations = expand.grid(mget(names(z), envir = z), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
stopifnot(nrow(valuations) == 128L, !any(evaluate_formula(opposed, valuations)),
  identical(canonical_formula(opposed_formula), canonical_formula(opposed)))
for (i in seq_along(opposed)) stopifnot(any(evaluate_formula(opposed[-i], valuations)))
cat("Opposed Boolean four-edge cycles: unsatisfiable, every clause indispensable, production unchanged.\n")
cat("R:", R.version.string, "\n")
