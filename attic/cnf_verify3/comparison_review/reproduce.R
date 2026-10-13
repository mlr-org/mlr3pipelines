source("attic/cnf_verify3/comparison_review/guarded_candidate.R")

records = list()
record = function(label, left, right, expected = TRUE) {
  stopifnot(identical(attr(left, "universe"), attr(right, "universe")))
  oracle = same_payload(left, right)
  stopifnot(identical(oracle, expected))
  original = capture(all.equal(left, right))
  candidate = capture(candidate_equal(left, right))
  guarded = capture(guarded_candidate_equal(left, right))
  stopifnot(is.null(candidate$error), identical(candidate$equal, oracle),
    is.null(guarded$error), identical(guarded$equal, oracle))
  records[[length(records) + 1L]] <<- list(
    label = label, locale = Sys.getlocale("LC_COLLATE"), class = class(left),
    expected = expected, identical = identical(left, right), original = original, candidate = candidate, guarded = guarded)
}

collation_locales = c("C", "C.UTF-8", "en_US.UTF-8")
initial_locale = Sys.getlocale("LC_COLLATE")
collation_traces = list()
encoding_traces = list()
for (locale in collation_locales) {
  stopifnot(nzchar(Sys.setlocale("LC_COLLATE", locale)))
  spellings = c("\u00e9", "e\u0301")
  u = CnfUniverse()
  symbol = CnfSymbol(u, "X", c(spellings, "other"))
  a = symbol %among% spellings
  b = symbol %among% rev(spellings)
  record("collation_atom_values", a, b)
  record("collation_clause_values", as.CnfClause(a), as.CnfClause(b))
  record("collation_formula_values", as.CnfFormula(a), as.CnfFormula(b))
  # Unequal finite sets: NFC and NFD are distinct R values and must stay so.
  record("unequal_atom_values", symbol %among% spellings[[1L]], symbol %among% spellings[[2L]], FALSE)
  record("unequal_clause_values", as.CnfClause(symbol %among% spellings[[1L]]),
    as.CnfClause(symbol %among% spellings[[2L]]), FALSE)
  record("unequal_formula_values", as.CnfFormula(symbol %among% spellings[[1L]]),
    as.CnfFormula(symbol %among% spellings[[2L]]), FALSE)
  collation_traces[[locale]] = list(
    distinct_values = !identical(spellings[[1L]], spellings[[2L]]),
    ordinary_orders_agree = identical(sort(spellings), sort(rev(spellings))),
    radix_orders_agree = identical(sort(enc2utf8(spellings), method = "radix"),
      sort(enc2utf8(rev(spellings)), method = "radix")),
    input_codepoints = lapply(spellings, utf8ToInt),
    ordered_codepoints = lapply(sort(spellings), utf8ToInt),
    reverse_ordered_codepoints = lapply(sort(rev(spellings)), utf8ToInt))

  u = CnfUniverse()
  first_symbol = CnfSymbol(u, spellings[[1L]], c("0", "1"))
  second_symbol = CnfSymbol(u, spellings[[2L]], c("0", "1"))
  left = CnfClause(list(first_symbol %among% "0", second_symbol %among% "1"))
  right = CnfClause(list(second_symbol %among% "1", first_symbol %among% "0"))
  unequal = CnfClause(list(second_symbol %among% "0", first_symbol %among% "1"))
  record("collation_clause_symbols", left, right)
  record("collation_formula_symbols", as.CnfFormula(left), as.CnfFormula(right))
  record("unequal_symbol_association_clause", left, unequal, FALSE)
  record("unequal_symbol_association_formula", as.CnfFormula(left), as.CnfFormula(unequal), FALSE)

  u = CnfUniverse()
  values = c("\u00e9", "\u00f6")
  x = CnfSymbol(u, "X", c(values, "other"))
  y = CnfSymbol(u, "Y", c("a", "b", "c"))
  make_formula = function(ranges) CnfFormula(list(
    CnfClause(list(x %among% ranges[[1L]], y %among% "a")),
    CnfClause(list(x %among% ranges[[2L]], y %among% "b"))))
  left = make_formula(values)
  right = make_formula(encode_as(values, "latin1"))
  stopifnot(identical(left, right))
  record("encoding_identical_formula", left, right)
  for (i in seq_along(left)) record(paste0("encoding_clause_", i), as.list(left)[[i]], as.list(right)[[i]])
  get_keys = function(formula) vapply(unclass(formula), function(clause) {
    paste0(paste(names(clause), collapse = ".__."), digest::digest(c(clause), algo = "xxhash64"))
  }, "")
  left_truth = evaluate_formula(left, list(X = c(values, "other"), Y = c("a", "b", "c")))
  right_truth = evaluate_formula(right, list(X = c(values, "other"), Y = c("a", "b", "c")))
  stopifnot(identical(left_truth, right_truth), sum(left_truth) == 2L)
  encoding_traces[[locale]] = list(
    utf8_keys = get_keys(left), latin1_keys = get_keys(right),
    utf8_clause_order = order(get_keys(left)), latin1_clause_order = order(get_keys(right)),
    utf8_marks = lapply(unclass(left), function(clause) lapply(clause, Encoding)),
    latin1_marks = lapply(unclass(right), function(clause) lapply(clause, Encoding)),
    assignments = length(left_truth), models = sum(left_truth), truth_agrees = identical(left_truth, right_truth))
}
invisible(Sys.setlocale("LC_COLLATE", initial_locale))

summary = list(
  comparisons = length(records), expected_equal = sum(vapply(records, function(x) x$expected, logical(1))),
  expected_unequal = sum(!vapply(records, function(x) x$expected, logical(1))),
  production_false_negatives = sum(vapply(records, function(x) x$expected && !x$original$equal, logical(1))),
  production_false_positives = sum(vapply(records, function(x) !x$expected && x$original$equal, logical(1))),
  candidate_errors = sum(vapply(records, function(x) !is.null(x$candidate$error), logical(1))),
  candidate_disagreements = sum(vapply(records, function(x) x$candidate$equal != x$expected, logical(1))),
  guarded_disagreements = sum(vapply(records, function(x) x$guarded$equal != x$expected, logical(1))))
stopifnot(summary$comparisons == 39L, summary$production_false_negatives == 13L,
  summary$production_false_positives == 0L, summary$candidate_errors == 0L, summary$candidate_disagreements == 0L)
write_results(list(metadata = metadata(), summary = summary, collation_traces = collation_traces,
  encoding_traces = encoding_traces, records = records), "reproduce")
