# Independent accepted-dimensional-atom comparison checks, no production edits.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
for (source_name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(source_name, ".R")))
}

u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("a", "b", "c"))

first_support = function(values) {
  answer = character()
  for (i in seq_along(values)) {
    value = values[[i]]
    if (!any(vapply(seq_along(answer), function(j) identical(value, answer[[j]]), TRUE))) {
      answer = c(answer, value)
    }
  }
  answer
}
truth = function(atom) {
  if (is.logical(atom)) return(rep(unclass(atom), 3L))
  vapply(c("a", "b", "c"), function(value) {
    any(vapply(seq_along(atom$values), function(i) identical(value, atom$values[[i]]), TRUE))
  }, TRUE, USE.NAMES = FALSE)
}

private_copy = function(normalizer) {
  result = all.equal.CnfAtom
  parts = as.list(body(result))
  patterns = list(quote({target$values = sort(target$values)})[[2L]],
    quote({current$values = sort(current$values)})[[2L]])
  replacements = list(quote({target$values = normalizer(target$values)})[[2L]],
    quote({current$values = normalizer(current$values)})[[2L]])
  for (i in 1:2) {
    position = which(vapply(parts, identical, TRUE, patterns[[i]]))
    stopifnot(length(position) == 1L)
    parts[position] = replacements[i]
  }
  body(result) = as.call(parts)
  local_env = new.env(parent = environment(all.equal.CnfAtom))
  local_env$normalizer = normalizer
  environment(result) = local_env
  stopifnot(identical(formals(result), formals(all.equal.CnfAtom)),
    identical(tail(as.list(body(result)), 1L), tail(as.list(body(all.equal.CnfAtom)), 1L)))
  result
}

previous = private_copy(function(values) sort(enc2utf8(values), method = "radix"))
flatten_only = private_copy(function(values) sort(enc2utf8(as.vector(values)), method = "radix"))
candidate = private_copy(function(values) sort(unique(enc2utf8(as.vector(values))), method = "radix"))

shapes = list(
  minimal_row = matrix(c("a", "a"), nrow = 1L),
  minimal_column = matrix(c("a", "a"), ncol = 1L),
  matrix_2_by_3 = matrix(c("a", "a", "b", "b", "a", "a"), nrow = 2L, byrow = TRUE),
  array_2_by_2_by_2 = array(c("a", "b", "a", "a", "b", "a", "a", "b"), c(2L, 2L, 2L)),
  array_duplicate_slabs = array(rep(c("a", "a", "b"), 4L), c(3L, 2L, 2L)),
  vector_duplicate_control = c("a", "b", "a", "a"),
  row_distinct_control = matrix(c("a", "b"), nrow = 1L)
)

records = list()
for (shape in names(shapes)) {
  values = shapes[[shape]]
  reference = CnfAtom(X, first_support(values))
  actual = CnfAtom(X, values)
  original_result = all.equal(actual, reference)
  previous_result = previous(actual, reference)
  flatten_result = flatten_only(actual, reference)
  proposed_result = candidate(actual, reference)
  expected_failure = shape %in% c("minimal_row", "matrix_2_by_3", "array_2_by_2_by_2", "array_duplicate_slabs")
  stopifnot(identical(truth(actual), truth(reference)),
    identical(isTRUE(original_result), !expected_failure),
    identical(isTRUE(previous_result), !expected_failure),
    identical(isTRUE(flatten_result), !expected_failure),
    isTRUE(proposed_result), isTRUE(candidate(reference, actual)), isTRUE(candidate(actual, actual)),
    isTRUE(all.equal(as.CnfClause(actual), as.CnfClause(reference))),
    isTRUE(all.equal(as.CnfFormula(actual), as.CnfFormula(reference))))

  # Partial-overlap and disjoint unequal supports, plus a different symbol.
  negative_values = if (length(reference$values) == 1L) list(c("a", "b"), "b", "c") else list("a", c("a", "c"), "c")
  negative_results = lapply(negative_values, function(other_values) {
    other = CnfAtom(X, other_values)
    stopifnot(!identical(truth(actual), truth(other)), !isTRUE(candidate(actual, other)),
      !isTRUE(candidate(other, actual)))
    candidate(actual, other)
  })
  other_symbol = CnfAtom(Y, first_support(values))
  stopifnot(!isTRUE(candidate(actual, other_symbol)), !isTRUE(candidate(other_symbol, actual)))
  records[[shape]] = list(input_dim = dim(values), stored_dim = dim(actual$values),
    input = as.vector(values), stored = as.vector(actual$values),
    support = reference$values, sorted_stored = sort(actual$values),
    original = original_result, previous = previous_result, flatten_only = flatten_result,
    candidate = proposed_result, truth = truth(actual), negative_results = negative_results)
}

# Every original branch outside the proper-object normalization remains verbatim.
guard_pairs = list(
  list(CnfAtom(X, matrix(c("a", "b", "c"), nrow = 1L)), as.CnfAtom(TRUE)),
  list(CnfAtom(X, matrix(character(), nrow = 0L, ncol = 2L)), as.CnfAtom(FALSE)),
  list(as.CnfAtom(TRUE), as.CnfAtom(FALSE)),
  list(CnfAtom(X, "a"), as.CnfAtom(TRUE)),
  list(as.CnfAtom(FALSE), CnfAtom(X, "a")),
  list(CnfAtom(X, "a"), list(symbol = "X", values = "a")),
  list(CnfAtom(X, "a"), X),
  list(CnfAtom(X, "a"), NULL)
)
guard_results = lapply(guard_pairs, function(pair) {
  original = all.equal.CnfAtom(pair[[1L]], pair[[2L]])
  proposed = candidate(pair[[1L]], pair[[2L]])
  stopifnot(identical(original, proposed))
  original
})

# Check the parent's exact normalizer without running its output-writing bank.
parent_expressions = as.list(parse("attic/cnf_verify3/root/atom_shape_comparison.R"))
normalizer_assignment = Filter(function(expr) is.call(expr) && identical(expr[[1L]], as.name("=")) &&
  identical(expr[[2L]], as.name("normalizer")), parent_expressions)
stopifnot(length(normalizer_assignment) == 1L)
parent_env = new.env(parent = baseenv())
eval(normalizer_assignment[[1L]], parent_env)
parent_candidate = private_copy(parent_env$normalizer)
for (shape in names(shapes)) {
  left = CnfAtom(X, shapes[[shape]])
  right = CnfAtom(X, first_support(shapes[[shape]]))
  stopifnot(identical(candidate(left, right), parent_candidate(left, right)))
}

old_collate = Sys.getlocale("LC_COLLATE")
stopifnot(identical(Sys.setlocale("LC_COLLATE", "C"), "C"))
c_locale_result = all.equal(CnfAtom(X, shapes$minimal_row), CnfAtom(X, "a"))
stopifnot(!isTRUE(c_locale_result), isTRUE(candidate(CnfAtom(X, shapes$minimal_row), CnfAtom(X, "a"))))
invisible(Sys.setlocale("LC_COLLATE", old_collate))

result = list(runtime = R.version.string, locale = Sys.getlocale(),
  package_versions = vapply(c("checkmate", "mlr3misc", "jsonlite"), function(package) as.character(packageVersion(package)), ""),
  source_md5 = tools::md5sum(c("R/CnfAtom.R", "R/CnfClause.R", "R/CnfFormula.R", "attic/cnf_verify3/root/atom_shape_comparison.R")),
  shape_pairs = length(shapes), original_false_negatives = 4L, prior_candidate_false_negatives = 4L,
  flatten_only_false_negatives = 4L, candidate_disagreements = 0L,
  support_negative_directions = 6L * length(shapes), symbol_negative_directions = 2L * length(shapes),
  guard_checks = length(guard_pairs), records = records, guard_results = guard_results,
  c_locale_minimal = c_locale_result)
suffix = if (getRversion() < "4.0") "r36" else "r46"
output = file.path("attic", "cnf_verify3", "atom_shape_review", paste0("results_", suffix))
saveRDS(result, paste0(output, ".rds"))
write_json(result, paste0(output, ".json"), pretty = TRUE, auto_unbox = TRUE)
print(result[1:12])
