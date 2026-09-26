# Reduced residual limitation of normalizing only the proper CNF payload.
source("attic/cnf_verify3/comparison_review/guarded_candidate.R")

initial_locale = Sys.getlocale("LC_COLLATE")
symbol_names = c("38\u00e9", "38e\u0301")
make_universe = function(insertion_order) {
  universe = CnfUniverse()
  for (i in insertion_order) CnfSymbol(universe, symbol_names[[i]], c("0", "1"))
  universe
}
left_universe = make_universe(1:2)
right_universe = make_universe(2:1)
stopifnot(same_values(names(left_universe), names(right_universe)),
  all(vapply(symbol_names, function(name) identical(left_universe[[name]], right_universe[[name]]), logical(1))))
left_atom = `$.CnfUniverse`(left_universe, symbol_names[[1L]]) %among% "0"
right_atom = `$.CnfUniverse`(right_universe, symbol_names[[1L]]) %among% "0"

records = list()
for (locale in c("C", "C.UTF-8", "en_US.UTF-8")) {
  stopifnot(nzchar(Sys.setlocale("LC_COLLATE", locale)))
  for (class_name in c("CnfAtom", "CnfClause", "CnfFormula")) {
    coerce = get(paste0("as.", class_name))
    left = coerce(left_atom)
    right = coerce(right_atom)
    stopifnot(same_payload(left, right))
    original = capture(all.equal(left, right))
    candidate = capture(candidate_equal(left, right))
    guarded = capture(guarded_candidate_equal(left, right))
    stopifnot(identical(original$equal, locale == "C"),
      identical(candidate$equal, locale == "C"), identical(guarded$equal, locale == "C"),
      is.null(original$error), is.null(candidate$error), is.null(guarded$error))
    records[[length(records) + 1L]] = list(locale = locale, class = class_name,
      same_payload = TRUE, same_bindings = TRUE,
      base_environment = capture(all.equal(left_universe, right_universe)),
      left_sorted_bindings = names(as.list.environment(left_universe, sorted = TRUE)),
      right_sorted_bindings = names(as.list.environment(right_universe, sorted = TRUE)),
      original = original, candidate = candidate, guarded = guarded)
  }
}
invisible(Sys.setlocale("LC_COLLATE", initial_locale))
write_results(list(metadata = metadata(), records = records), "universe_residual")
