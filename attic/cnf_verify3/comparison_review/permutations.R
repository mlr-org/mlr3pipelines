source("attic/cnf_verify3/comparison_review/guarded_candidate.R")

initial_locale = Sys.getlocale("LC_COLLATE")
rows = list()
examples = list()
name_encoding_marks = character()
value_encoding_marks = character()
variant_counts = list()
record_check = function(group, case, comparison, left, right, expected) {
  stopifnot(identical(attr(left, "universe"), attr(right, "universe")))
  oracle = same_payload(left, right)
  stopifnot(identical(oracle, expected))
  original = capture(all.equal(left, right))
  candidate = capture(candidate_equal(left, right))
  guarded = capture(guarded_candidate_equal(left, right))
  stopifnot(is.null(original$error), is.null(candidate$error), identical(candidate$equal, oracle),
    is.null(guarded$error), identical(guarded$equal, oracle))
  rows[[length(rows) + 1L]] <<- data.frame(
    group = group, case = case, comparison = comparison,
    locale = Sys.getlocale("LC_COLLATE"), expected = expected,
    original = original$equal, candidate = candidate$equal, guarded = guarded$equal,
    stringsAsFactors = FALSE)
  example_key = paste(group, Sys.getlocale("LC_COLLATE"), comparison, sep = "/")
  if (!identical(original$equal, expected) && is.null(examples[[example_key]])) {
    examples[[example_key]] <<- list(case = case, message = original$message)
  }
}
check_variant = function(group, case, reference, variant, negative) {
  record_check(group, case, "forward_equal", reference, variant, TRUE)
  record_check(group, case, "reverse_equal", variant, reference, TRUE)
  record_check(group, case, "reflexivity", variant, variant, TRUE)
  record_check(group, case, "unequal_control", variant, negative, FALSE)
}

u = CnfUniverse()
domain = c("\u00e9", "e\u0301", "\u00f6", "plain", "other")
x = CnfSymbol(u, "X", domain)
encoding_patterns = expand.grid(first = c("utf8", "latin1", "native"),
  second = c("utf8", "native"), third = c("utf8", "latin1", "native"),
  stringsAsFactors = FALSE)
value_orders = permutations(1:4)
atom_reference = x %among% domain[1:4]
atom_negative = x %among% domain[c(1L, 2L, 3L, 5L)]
atom_variants = list()
for (pattern_index in seq_len(nrow(encoding_patterns))) {
  values = domain[1:4]
  for (i in 1:3) values[[i]] = encode_as(values[[i]], encoding_patterns[[i]][[pattern_index]])
  value_encoding_marks = union(value_encoding_marks, Encoding(values))
  for (value_order in value_orders) {
    atom_variants[[length(atom_variants) + 1L]] = x %among% values[value_order]
  }
}
variant_counts$atoms = length(atom_variants)

u = CnfUniverse()
symbol_names = domain[1:3]
symbols = lapply(symbol_names, function(name) CnfSymbol(u, name, domain))
symbol_orders = permutations(1:3)
clause_ranges = list(domain[1:2], domain[3:4], domain[c(1L, 3L)])
formula_ranges = list(
  list(domain[1:2], domain[4L], domain[1L]),
  list(domain[3L], domain[c(1L, 3L)], domain[4L]),
  list(domain[4L], domain[2L], domain[c(2L, 3L)]))
plain_clause = function(ranges) CnfClause(lapply(1:3, function(i) symbols[[i]] %among% ranges[[i]]))
clause_reference = plain_clause(clause_ranges)
clause_negative = plain_clause(clause_ranges[c(2L, 1L, 3L)])
formula_reference = CnfFormula(lapply(formula_ranges, plain_clause))
negative_ranges = formula_ranges
negative_ranges[[1L]] = negative_ranges[[1L]][c(2L, 1L, 3L)]
formula_negative = CnfFormula(lapply(negative_ranges, plain_clause))
stopifnot(length(formula_reference) == 3L, length(formula_negative) == 3L)
truth_reference = evaluate_formula(formula_reference, setNames(rep(list(domain), 3L), symbol_names))
truth_negative = evaluate_formula(formula_negative, setNames(rep(list(domain), 3L), symbol_names))
stopifnot(!identical(truth_reference, truth_negative))

# Every encoding schedule uses all allowed marks for the three non-ASCII names.
# The same schedule is shifted across occurrences to produce mixed range marks.
make_variant_clause = function(ranges, symbol_order, pattern_index, reverse_values, shift) {
  atoms = lapply(symbol_order, function(i) {
    symbol_name = encode_as(symbol_names[[i]], encoding_patterns[[i]][[pattern_index]])
    symbol = `$.CnfUniverse`(u, symbol_name)
    name_encoding_marks <<- union(name_encoding_marks, Encoding(c(symbol)))
    values = ranges[[i]]
    for (j in seq_along(values)) {
      allowed = if (identical(values[[j]], domain[[2L]])) c("utf8", "native") else c("utf8", "latin1", "native")
      encoding = allowed[[1L + (pattern_index + shift + i + j) %% length(allowed)]]
      values[[j]] = encode_as(values[[j]], encoding)
    }
    value_encoding_marks <<- union(value_encoding_marks, Encoding(values))
    symbol %among% if (reverse_values) rev(values) else values
  })
  CnfClause(atoms)
}

clause_variants = list()
for (pattern_index in seq_len(nrow(encoding_patterns))) {
  for (symbol_order in symbol_orders) {
    for (reverse_values in c(FALSE, TRUE)) {
      clause_variants[[length(clause_variants) + 1L]] = make_variant_clause(
        clause_ranges, symbol_order, pattern_index, reverse_values, 0L)
    }
  }
}
variant_counts$clauses = length(clause_variants)

formula_variants = list()
for (pattern_index in seq_len(nrow(encoding_patterns))) {
  for (symbol_order in symbol_orders) {
    for (clause_order in permutations(1:3)) {
      clauses = lapply(clause_order, function(i) make_variant_clause(formula_ranges[[i]],
        symbol_order[c(seq.int(i, 3L), if (i > 1L) seq_len(i - 1L) else integer())],
        pattern_index, (pattern_index + i) %% 2L == 0L, i))
      formula = CnfFormula(clauses)
      # This makes comparison failure diagnosis independent of simplification:
      # every constructed result has exactly the reference's clause multiset.
      stopifnot(length(formula) == 3L, same_formula(formula_reference, formula))
      formula_variants[[length(formula_variants) + 1L]] = formula
    }
  }
}
variant_counts$formulas = length(formula_variants)
stopifnot(identical(unlist(variant_counts, use.names = FALSE), c(432L, 216L, 648L)),
  setequal(name_encoding_marks, c("UTF-8", "latin1", "unknown")),
  setequal(value_encoding_marks, c("UTF-8", "latin1", "unknown")))

for (locale in c("C", "C.UTF-8", "en_US.UTF-8")) {
  stopifnot(nzchar(Sys.setlocale("LC_COLLATE", locale)))
  for (i in seq_along(atom_variants)) check_variant("atom", i, atom_reference, atom_variants[[i]], atom_negative)
  cat(locale, "atom variants completed\n")
  for (i in seq_along(clause_variants)) check_variant("clause", i, clause_reference, clause_variants[[i]], clause_negative)
  cat(locale, "clause variants completed\n")
  for (i in seq_along(formula_variants)) check_variant("formula", i, formula_reference, formula_variants[[i]], formula_negative)
  cat(locale, "formula variants completed\n")
}
invisible(Sys.setlocale("LC_COLLATE", initial_locale))
rows = do.call(rbind, rows)
stopifnot(nrow(rows) == 15552L, all(rows$candidate == rows$expected), all(rows$guarded == rows$expected),
  !any(rows$original & !rows$expected))
write.csv(rows, file.path(review_dir, paste0("permutations_checks_", runtime_tag, ".csv")), row.names = FALSE)
saveRDS(rows, file.path(review_dir, paste0("permutations_checks_", runtime_tag, ".rds")), version = 2)
summary = do.call(rbind, lapply(split(rows, interaction(rows$group, rows$locale, drop = TRUE)), function(group) {
  data.frame(group = group$group[[1L]], locale = group$locale[[1L]],
    checks = nrow(group), expected_equal = sum(group$expected), expected_unequal = sum(!group$expected),
    production_false_negatives = sum(group$expected & !group$original),
    production_false_positives = sum(!group$expected & group$original),
    candidate_disagreements = sum(group$expected != group$candidate),
    guarded_disagreements = sum(group$expected != group$guarded), stringsAsFactors = FALSE)
}))
write_results(list(metadata = metadata(), variant_counts = variant_counts,
  checks = nrow(rows), name_encoding_marks = name_encoding_marks,
  value_encoding_marks = value_encoding_marks,
  formula_negative_control = list(assignments = length(truth_reference),
    reference_models = sum(truth_reference), negative_models = sum(truth_negative),
    differing_assignments = sum(truth_reference != truth_negative)),
  summary = summary, original_failure_examples = examples), "permutations")
