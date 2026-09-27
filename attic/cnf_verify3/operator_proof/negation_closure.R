source("attic/cnf_verify3/operator_proof/bootstrap.R")

# A diagnostic implementation with the production constructor/operator bodies
# but an identity kernel.  The kernel only wraps proper clauses and represents
# the empty conjunction by TRUE.  No SAT, subsumption, or resolution rule runs.
# Direct Reduce dispatch selects the private OR copy without changing S3 state.
identity_state = new.env(parent = emptyenv())
identity_state$calls = 0L
identity_state$maximum_clauses = 0L
private = new.env(parent = .GlobalEnv)
private$simplify_cnf = function(entries, universe) {
  identity_state$calls = identity_state$calls + 1L
  if (!is.logical(entries)) {
    identity_state$maximum_clauses = max(identity_state$maximum_clauses, length(entries))
  }
  structure(if (!length(entries)) TRUE else entries, universe = universe, class = "CnfFormula")
}
private$CnfFormula = CnfFormula
environment(private$CnfFormula) = private
private$.or = `|.CnfFormula`
environment(private$.or) = private
private$.not = `!.CnfFormula`
environment(private$.not) = private
negation_body = paste(deparse(body(private$.not), width.cutoff = 500L), collapse = "\n")
stopifnot(grepl("Reduce(function(x, y) x | y, negated_formulae)", negation_body, fixed = TRUE))
body(private$.not) = parse(text = sub("Reduce(function(x, y) x | y, negated_formulae)",
  "Reduce(function(x, y) .or(x, y), negated_formulae)", negation_body, fixed = TRUE))[[1L]]

counters = c(cases = 0L, valuations = 0L, contradictions = 0L,
  unrecognized_contradictions = 0L, nonconstant_negations = 0L, falsifying_witnesses = 0L)
check_case = function(formula, universe, assignments) {
  assert_canonical(formula, universe)
  before = serialize(formula, NULL)
  truth = evaluate_formula(formula, assignments)
  result = !formula
  identity_result = private$.not(formula)
  stopifnot(identical(evaluate_formula(result, assignments), !truth),
    identical(evaluate_formula(identity_result, assignments), !truth),
    identical(isTRUE(c(result)), !any(truth)),
    identical(isTRUE(c(identity_result)), !any(truth)),
    identical(before, serialize(formula, NULL)))
  assert_canonical(result, universe)
  assert_canonical(identity_result, universe)
  witnesses = 0L
  for (candidate in list(result, identity_result)) {
    if (is.logical(candidate)) next
    # Falsify one proper output clause by choosing outside every listed range.
    valuation = as.data.frame(lapply(names(universe), function(s) {
      setdiff(universe[[s]], candidate[[1L]][[s]])[[1L]]
    }), stringsAsFactors = FALSE)
    names(valuation) = names(universe)
    stopifnot(!evaluate_formula(candidate, valuation), evaluate_formula(formula, valuation))
    witnesses = witnesses + 1L
  }
  counters <<- counters + c(1L, nrow(assignments), as.integer(!any(truth)),
    as.integer(!any(truth) && !is.logical(formula)), as.integer(!is.logical(result)), witnesses)
  invisible(NULL)
}

set.seed(20260907L)
for (iteration in seq_len(600L)) {
  domains = lapply(seq_len(sample(2:4, 1L)), function(i) as.character(seq_len(sample(2:5, 1L))))
  names(domains) = paste0("s", seq_along(domains))
  universe = make_universe(domains)
  raw_clauses = lapply(seq_len(sample(1:5, 1L)), function(i) {
    width = sample(seq_len(min(length(domains), 3L)), 1L)
    symbols = sample(names(domains), width)
    setNames(lapply(symbols, function(s) {
      sample(domains[[s]], sample(seq_len(length(domains[[s]]) - 1L), 1L))
    }), symbols)
  })
  # Alternate existing constructor outputs and arbitrary canonical raw formula
  # representations.  The latter are proper but need not already be reduced.
  formula = if (iteration %% 2L) {
    CnfFormula(lapply(raw_clauses, make_clause, universe = universe))
  } else {
    structure(raw_clauses, universe = universe, class = "CnfFormula")
  }
  assignments = expand.grid(domains, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  check_case(formula, universe, assignments)
}

# The independently truth-checked contradiction which the kernel leaves intact.
domains = setNames(rep(list(c("0", "1")), 7L), c("x", "a1", "a2", "a3", "b1", "b2", "b3"))
universe = make_universe(domains)
clauses = list(list(x = "1", a1 = "1"), list(a1 = "0", a2 = "1"),
  list(a2 = "0", a3 = "1"), list(a3 = "0", x = "1"),
  list(x = "0", b1 = "1"), list(b1 = "0", b2 = "1"),
  list(b2 = "0", b3 = "1"), list(b3 = "0", x = "0"))
formula = CnfFormula(lapply(clauses, make_clause, universe = universe))
check_case(formula, universe,
  expand.grid(domains, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
for (value in c(TRUE, FALSE)) {
  check_case(as.CnfFormula(structure(value, universe = universe)), universe,
    expand.grid(domains, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
}

cat("R:", R.version.string, "\n")
print(counters)
cat("Identity kernel calls:", identity_state$calls,
  "; maximum clauses entering one identity call:", identity_state$maximum_clauses, "\n")
cat("All negations preserve semantics and recognize every tested contradiction, including with no kernel reductions.\n")
