# Check the intersection of dimensional atom acceptance and set comparison.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
for (source_name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(source_name, ".R")))
}

u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
first_support = function(values) {
  result = character()
  for (i in seq_along(values)) {
    if (!any(vapply(result, function(value) identical(value, values[[i]]), TRUE,
      USE.NAMES = FALSE))) result = c(result, values[[i]])
  }
  result
}
candidate = all.equal.CnfAtom
candidate_body = body(candidate)
normalizer = function(x) sort(unique(enc2utf8(as.vector(x))), method = "radix")
replace_sorts = function(node) {
  if (identical(node, quote(sort(target$values)))) return(quote(normalizer(target$values)))
  if (identical(node, quote(sort(current$values)))) return(quote(normalizer(current$values)))
  if (is.call(node)) for (i in seq_along(node)) {
    if (!identical(node[[i]], quote(expr = ))) node[i] = list(replace_sorts(node[[i]]))
  }
  node
}
body(candidate) = replace_sorts(candidate_body)
records = list()
examples = list()
for (size in 1:5) {
  sequences = as.matrix(expand.grid(rep(list(c("a", "b")), size), stringsAsFactors = FALSE))
  for (row in seq_len(nrow(sequences))) {
    flat = unname(sequences[row, ])
    shapes = list(vector = flat, row = matrix(flat, nrow = 1L),
      column = matrix(flat, ncol = 1L), array = array(flat, c(1L, size, 1L)))
    reference = CnfAtom(X, first_support(flat))
    for (shape in names(shapes)) {
      actual = CnfAtom(X, shapes[[shape]])
      expected_truth = vapply(c("a", "b", "c"), function(value) any(vapply(seq_along(flat),
        function(i) identical(value, flat[[i]]), TRUE)), TRUE, USE.NAMES = FALSE)
      actual_truth = vapply(c("a", "b", "c"), function(value) any(vapply(seq_along(actual$values),
        function(i) identical(value, actual$values[[i]]), TRUE)), TRUE, USE.NAMES = FALSE)
      comparison = all.equal(actual, reference)
      proposed = candidate(actual, reference)
      roundtrip = all.equal(as.CnfClause(actual), as.CnfClause(reference))
      negative = candidate(actual, CnfAtom(X, "c"))
      stopifnot(identical(expected_truth, actual_truth), isTRUE(proposed), isTRUE(roundtrip),
        !isTRUE(negative))
      record = list(size = size, row = row, shape = shape, equal = isTRUE(comparison),
        actual_length = length(actual$values), support_length = length(reference$values))
      records[[length(records) + 1L]] = record
      if (!isTRUE(comparison) && is.null(examples[[shape]])) {
        examples[[shape]] = list(input = shapes[[shape]], stored = actual$values,
          reference = reference$values, comparison = comparison,
          truth = actual_truth, clause_comparison = roundtrip)
      }
    }
  }
}
minimal = CnfAtom(X, matrix(c("a", "a"), nrow = 1L))
flat = CnfAtom(X, "a")
stopifnot(!isTRUE(all.equal(minimal, flat)), isTRUE(candidate(minimal, flat)),
  isTRUE(all.equal(as.CnfClause(minimal), as.CnfClause(flat))))
result = list(runtime = R.version.string, comparisons = length(records),
  false_negatives = sum(vapply(records, function(x) !x$equal, TRUE)),
  unequal_controls = length(records), positional_rows = 3L * length(records),
  examples = examples, records = records)
suffix = if (getRversion() < "4.0") "r36" else "r46"
base_path = file.path("attic", "cnf_verify3", "root", paste0("atom_shape_comparison_", suffix))
saveRDS(result, paste0(base_path, ".rds"))
write_json(result, paste0(base_path, ".json"), auto_unbox = TRUE, pretty = TRUE)
print(result[1:6])
