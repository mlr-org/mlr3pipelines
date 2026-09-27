source("attic/cnf_verify3/operator_proof/bootstrap.R")

# Instrument only this in-memory function object; production source is read-only.
make_or_variant = function(mutant = FALSE) {
  original = `|.CnfFormula`
  text = paste(deparse(body(original), width.cutoff = 500L), collapse = "\n")
  replace_once = function(old, new) {
    stopifnot(length(gregexpr(old, text, fixed = TRUE)[[1L]]) == 1L,
      grepl(old, text, fixed = TRUE))
    text <<- sub(old, new, text, fixed = TRUE)
  }
  replace_once("distributed = lapply(unclass(e1), function(e1_clause) {",
    paste("distributed = lapply(unclass(e1), function(e1_clause) {",
      ".row_entry(environment(), e2, e1_clause)", sep = "\n"))
  replace_once("e2[!eliminated]", ".row_exit(environment(), e2, e1_clause, eliminated)\ne2[!eliminated]")
  replace_once("simplify_cnf(unlist(distributed, recursive = FALSE), universe)",
    ".capture_distribution(unlist(distributed, recursive = FALSE), universe)")
  if (mutant) replace_once("e2[[i2]] = e2_clause", "e2[[i2]] <<- e2_clause")
  state = new.env(parent = emptyenv())
  state$rows = list()
  state$distributed = NULL
  env = new.env(parent = environment(original))
  env$.row_entry = function(frame, e2, clause) {
    state$rows[[length(state$rows) + 1L]] = list(frame = frame,
      local_at_entry = exists("e2", envir = frame, inherits = FALSE),
      initial = serialize(c(e2), NULL), clause = clause)
  }
  env$.row_exit = function(frame, e2, clause, eliminated) {
    i = length(state$rows)
    state$rows[[i]]$local_at_exit = exists("e2", envir = frame, inherits = FALSE)
    state$rows[[i]]$parent_at_exit = serialize(c(get("e2", envir = parent.env(frame))), NULL)
  }
  env$.capture_distribution = function(entries, universe) {
    state$distributed = entries
    simplify_cnf(entries, universe)
  }
  fun = original
  body(fun) = parse(text = text)[[1L]]
  environment(fun) = env
  list(fun = fun, state = state)
}

cartesian_clauses = function(first, second, universe) {
  clauses = list()
  for (left in c(first)) for (right in c(second)) {
    symbols = union(names(left), names(right))
    merged = setNames(lapply(symbols, function(s) union(left[[s]], right[[s]])), symbols)
    if (any(vapply(symbols, function(s) setequal(merged[[s]], universe[[s]]), logical(1)))) next
    clauses[[length(clauses) + 1L]] = merged
  }
  clauses
}

counters = c(cases = 0L, callback_rows = 0L, cartesian_pairs = 0L,
  surviving_pairs = 0L, tautological_pairs = 0L, valuations = 0L, swaps = 0L)
check_case = function(first, second, universe, assignments) {
  # This experiment concerns the distribution branch only.
  if (is.logical(first) || is.logical(second)) return(invisible(NULL))
  first_before = serialize(first, NULL)
  second_before = serialize(second, NULL)
  original = `|.CnfFormula`(first, second)
  variant = make_or_variant()
  result = variant$fun(first, second)
  expected = cartesian_clauses(first, second, universe)
  stopifnot(identical(canonical_formula(result), canonical_formula(original)),
    identical(canonical_formula(variant$state$distributed), canonical_formula(expected)),
    identical(first_before, serialize(first, NULL)),
    identical(second_before, serialize(second, NULL)),
    identical(evaluate_formula(result, assignments),
      evaluate_formula(first, assignments) | evaluate_formula(second, assignments)))
  assert_canonical(result, universe)
  expected_initial = serialize(c(if (length(first) > length(second)) first else second), NULL)
  rows = variant$state$rows
  stopifnot(length(rows) == min(length(first), length(second)))
  for (row in rows) {
    stopifnot(!row$local_at_entry, row$local_at_exit,
      identical(row$initial, expected_initial),
      identical(row$parent_at_exit, expected_initial))
  }
  if (length(rows) > 1L) {
    for (i in 2:length(rows)) stopifnot(!identical(rows[[1L]]$frame, rows[[i]]$frame))
  }
  pairs = length(first) * length(second)
  counters <<- counters + c(1L, length(rows), pairs, length(expected), pairs - length(expected),
    nrow(assignments), as.integer(length(first) > length(second)))
  invisible(NULL)
}

set.seed(20260906L)
for (iteration in seq_len(500L)) {
  domains = lapply(seq_len(sample(2:4, 1L)), function(i) {
    as.character(seq_len(sample(2:5, 1L)))
  })
  # Set names after sampling the number of symbols.
  names(domains) = paste0("s", seq_along(domains))
  universe = make_universe(domains)
  random_formula = function() {
    clauses = lapply(seq_len(sample(1:6, 1L)), function(i) {
      symbols = sample(names(domains), sample(seq_along(domains), 1L))
      ranges = setNames(lapply(symbols, function(s) {
        sample(domains[[s]], sample(seq_len(length(domains[[s]]) - 1L), 1L))
      }), symbols)
      make_clause(ranges, universe)
    })
    CnfFormula(clauses)
  }
  first = random_formula()
  second = random_formula()
  assignments = expand.grid(domains, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  check_case(first, second, universe, assignments)
  check_case(second, first, universe, assignments)
}

# The local-binding oracle must reject a mutant that cumulatively changes the
# enclosing e2 between callback rows.  Operand immutability alone cannot catch it.
domains = setNames(rep(list(c("0", "1")), 4L), c("x", "y", "z", "w"))
universe = make_universe(domains)
first = CnfFormula(list(make_clause(list(x = "0"), universe), make_clause(list(y = "0"), universe)))
second = CnfFormula(list(make_clause(list(z = "0"), universe), make_clause(list(w = "0"), universe)))
assignments = expand.grid(domains, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
check_case(first, second, universe, assignments)
mutant = make_or_variant(TRUE)
bad = mutant$fun(first, second)
expected_truth = evaluate_formula(first, assignments) | evaluate_formula(second, assignments)
wrong = which(evaluate_formula(bad, assignments) != expected_truth)
stopifnot(length(wrong) > 0L,
  !identical(canonical_formula(mutant$state$distributed),
    canonical_formula(cartesian_clauses(first, second, universe))),
  !mutant$state$rows[[1L]]$local_at_exit,
  !identical(mutant$state$rows[[1L]]$initial, mutant$state$rows[[2L]]$initial))
cat("R:", R.version.string, "\n")
print(counters)
cat("Cumulative-parent mutation rejected; disagreeing valuations:", length(wrong), "\n")
print(assignments[wrong, , drop = FALSE])
