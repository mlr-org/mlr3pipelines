source("attic/cnf_verify3/operator_proof/bootstrap.R")

domains = list(x = c("0", "1"), y = c("0", "1"))
u = make_universe(domains)
v = make_universe(domains)
assignments = expand.grid(domains, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
atom = function(s, values, owner = u) {
  CnfAtom(structure(s, universe = owner, class = "CnfSymbol"), values)
}
constant = function(value, class, owner = NULL) {
  get(paste0("as.", class))(structure(value, universe = owner))
}
all_sequences = function(n, upto = 3L) {
  answer = list(integer())
  for (width in seq_len(upto)) {
    grid = expand.grid(rep(list(seq_len(n)), width), KEEP.OUT.ATTRS = FALSE)
    answer = c(answer, lapply(seq_len(nrow(grid)), function(i) as.integer(grid[i, ])))
  }
  answer
}
clause_inputs = list(x0 = atom("x", "0"), x1 = atom("x", "1"), y0 = atom("y", "0"),
  xy = make_clause(list(x = "0", y = "1"), u),
  false_null = constant(FALSE, "CnfClause"), true_null = constant(TRUE, "CnfClause"),
  false_u = constant(FALSE, "CnfClause", u), true_u = constant(TRUE, "CnfClause", u),
  other_x0 = atom("x", "0", v))

# The owner check applies through the earliest tautological disjunction prefix,
# including the final member of that prefix.  Determine that prefix by truth
# table, independently of the constructor's union/coverage implementation.
clause_success_expected = function(arguments) {
  if (!length(arguments)) return(TRUE)
  owner = attr(arguments[[1L]], "universe")
  prefix = rep(FALSE, nrow(assignments))
  for (argument in arguments) {
    if (!identical(attr(argument, "universe"), owner)) return(FALSE)
    prefix = prefix | evaluate_formula(argument, assignments)
    if (all(prefix)) break
  }
  TRUE
}
clause_counts = c(cases = 0L, success = 0L, expected_errors = 0L)
for (indices in all_sequences(length(clause_inputs))) {
  arguments = clause_inputs[indices]
  expected_success = clause_success_expected(arguments)
  result = tryCatch(CnfClause(arguments), error = identity)
  stopifnot(identical(!inherits(result, "error"), expected_success))
  clause_counts[["cases"]] = clause_counts[["cases"]] + 1L
  if (!expected_success) {
    stopifnot(grepl("same universe", conditionMessage(result), fixed = TRUE))
    clause_counts[["expected_errors"]] = clause_counts[["expected_errors"]] + 1L
    next
  }
  expected = Reduce(`|`, lapply(arguments, evaluate_formula, assignments = assignments),
    init = rep(FALSE, nrow(assignments)))
  stopifnot(inherits(result, "CnfClause"), identical(evaluate_formula(result, assignments), expected))
  clause_counts[["success"]] = clause_counts[["success"]] + 1L
}

proper_clause = make_clause(list(x = "0", y = "1"), u)
proper_formula = CnfFormula(list(make_clause(list(x = "0"), u), make_clause(list(y = "0"), u)))
formula_inputs = list(clause = proper_clause, formula = proper_formula,
  false_null = constant(FALSE, "CnfFormula"), true_null = constant(TRUE, "CnfFormula"),
  false_u = constant(FALSE, "CnfFormula", u), true_u = constant(TRUE, "CnfFormula", u),
  false_clause = constant(FALSE, "CnfClause", u),
  other_clause = make_clause(list(x = "1", y = "0"), v))

# This classifies the API boundary, including the known malformed flattening.
# It deliberately does not predict internal simplifier reductions.
formula_outcome_expected = function(arguments) {
  if (!length(arguments)) return("success")
  owner = attr(arguments[[1L]], "universe")
  earlier_nonconstant_formula = FALSE
  for (argument in arguments) {
    if (isFALSE(c(argument))) {
      return(if (earlier_nonconstant_formula) "false_flattening" else "success")
    }
    if (isTRUE(c(argument))) next
    if (!identical(attr(argument, "universe"), owner)) return("owner_error")
    if (inherits(argument, "CnfFormula")) earlier_nonconstant_formula = TRUE
  }
  "success"
}
formula_counts = c(cases = 0L, success = 0L, owner_error = 0L, false_flattening = 0L)
for (indices in all_sequences(length(formula_inputs))) {
  arguments = formula_inputs[indices]
  expected_outcome = formula_outcome_expected(arguments)
  result = tryCatch(CnfFormula(arguments), error = identity)
  stopifnot(identical(!inherits(result, "error"), expected_outcome == "success"))
  formula_counts[["cases"]] = formula_counts[["cases"]] + 1L
  formula_counts[[expected_outcome]] = formula_counts[[expected_outcome]] + 1L
  if (expected_outcome == "owner_error") {
    stopifnot(grepl("same universe", conditionMessage(result), fixed = TRUE))
    next
  }
  if (expected_outcome == "false_flattening") next
  expected = Reduce(`&`, lapply(arguments, evaluate_formula, assignments = assignments),
    init = rep(TRUE, nrow(assignments)))
  stopifnot(inherits(result, "CnfFormula"), identical(evaluate_formula(result, assignments), expected))
  if (!is.logical(result)) assert_canonical(result, attr(result, "universe"))
}

# Record concrete boundary errors, not merely aggregate counts.
examples = list(
  formula_true_first = function() CnfFormula(list(constant(TRUE, "CnfFormula"), proper_clause)),
  formula_false_after_formula = function() CnfFormula(list(proper_formula, constant(FALSE, "CnfFormula", u))),
  clause_false_first = function() CnfClause(list(constant(FALSE, "CnfClause"), atom("x", "0"))),
  clause_false_last = function() CnfClause(list(atom("x", "0"), constant(FALSE, "CnfClause"))),
  clause_true_last = function() CnfClause(list(atom("x", "0"), constant(TRUE, "CnfClause"))))
cat("R:", R.version.string, "\n")
cat("Clause constructor:\n")
print(clause_counts)
cat("Formula constructor:\n")
print(formula_counts)
for (name in names(examples)) {
  error = tryCatch(examples[[name]](), error = identity)
  stopifnot(inherits(error, "error"))
  cat(name, ": ", conditionMessage(error), "\n", sep = "")
}

stopifnot(isTRUE(c(CnfClause(list(constant(TRUE, "CnfClause"), atom("x", "0"))))),
  identical(canonical_formula(CnfFormula(list(proper_clause, constant(TRUE, "CnfFormula")))),
    canonical_formula(as.CnfFormula(proper_clause))),
  isFALSE(c(CnfFormula(list(constant(FALSE, "CnfFormula"), proper_formula)))),
  isFALSE(c(CnfFormula(list(proper_clause, constant(FALSE, "CnfFormula"))))))

# Direct method invocation reproduces this without depending on pre-4.3 Ops
# dispatch.  The truth value is right, but the documented result class is lost.
raw_true = `|.CnfClause`(TRUE, proper_clause)
stopifnot(identical(raw_true, TRUE))
downstream = tryCatch(CnfFormula(list(raw_true)), error = identity)
stopifnot(inherits(downstream, "error"))
cat("TRUE | proper_clause: raw logical TRUE; using it in CnfFormula(list(...)) errors.\n")

# Direct formula methods avoid version-specific mixed-class dispatch.  Constants
# may have unrelated owners because both methods take their truth-table branches
# before checking identity of the two nonconstant owners.
operands = list(TRUE, FALSE, atom("x", "0"), proper_clause, proper_formula,
  CnfFormula(list(make_clause(list(x = "1", y = "0"), v))),
  constant(TRUE, "CnfAtom"), constant(FALSE, "CnfAtom", u),
  constant(TRUE, "CnfClause", u), constant(FALSE, "CnfClause"),
  constant(TRUE, "CnfFormula"), constant(FALSE, "CnfFormula", u))
binary_counts = c(cases = 0L, success = 0L, expected_owner_errors = 0L)
for (first in operands) for (second in operands) for (op in c("&", "|")) {
  result = tryCatch(get(paste0(op, ".CnfFormula"))(first, second), error = identity)
  expected_error = !is.logical(first) && !is.logical(second) &&
    !identical(attr(first, "universe"), attr(second, "universe"))
  stopifnot(identical(inherits(result, "error"), expected_error))
  binary_counts[["cases"]] = binary_counts[["cases"]] + 1L
  if (expected_error) {
    stopifnot(grepl("same universe", conditionMessage(result), fixed = TRUE))
    binary_counts[["expected_owner_errors"]] = binary_counts[["expected_owner_errors"]] + 1L
    next
  }
  assert_canonical(result, attr(result, "universe"))
  stopifnot(identical(evaluate_formula(result, assignments),
    do.call(op, list(evaluate_formula(first, assignments), evaluate_formula(second, assignments)))))
  binary_counts[["success"]] = binary_counts[["success"]] + 1L
}
cat("Direct Formula binary methods:\n")
print(binary_counts)
