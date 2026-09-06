source("attic/cnf_verify3/comparison_review/guarded_candidate.R")

initial_locale = Sys.getlocale("LC_COLLATE")
records = list()
record_scope = function(label, left, right) {
  records[[length(records) + 1L]] <<- list(label = label,
    locale = Sys.getlocale("LC_COLLATE"), class = class(left),
    identical = identical(left, right),
    original = capture(all.equal(left, right)),
    candidate = capture(candidate_equal(left, right)),
    guarded = capture(guarded_candidate_equal(left, right)))
}
for (class_name in c("CnfAtom", "CnfClause", "CnfFormula")) {
  coerce = get(paste0("as.", class_name))
  record_scope("equal_bare_TRUE", coerce(TRUE), coerce(TRUE))
  record_scope("TRUE_versus_FALSE", coerce(TRUE), coerce(FALSE))
  record_scope("named_versus_unnamed_TRUE", coerce(c(label = TRUE)), coerce(TRUE))
  record_scope("differently_named_TRUE", coerce(c(first = TRUE)), coerce(c(second = TRUE)))
  record_scope("named_versus_unnamed_FALSE", coerce(c(label = FALSE)), coerce(FALSE))
  u = CnfUniverse()
  x = CnfSymbol(u, "X", c("a", "b"))
  record_scope("TRUE_with_versus_without_universe", coerce(x %among% c("a", "b")), coerce(TRUE))
  record_scope("proper_versus_TRUE", coerce(x %among% "a"), coerce(TRUE))
  record_scope("proper_versus_wrong_class", coerce(x %among% "a"), "not a CNF object")
}

# Different-universe attribute behavior has an explicit test contract: domain
# vector order matters (tests/testthat/test_CnfUniverse.R). It is not a new bug.
u = CnfUniverse()
v = CnfUniverse()
x = CnfSymbol(u, "X", c("a", "b", "c"))
y = CnfSymbol(v, "X", c("c", "b", "a"))
for (class_name in c("CnfAtom", "CnfClause", "CnfFormula")) {
  coerce = get(paste0("as.", class_name))
  record_scope("universe_domain_order_contract", coerce(x %among% "a"), coerce(y %among% "a"))
}

# A separate, ordinary-string candidate boundary: default environment equality
# also sorts symbol bindings by locale. Search only ordinary public universes
# with the same two bindings inserted in opposite orders; no direct mutation.
search_count = 0L
environment_example = NULL
stopifnot(nzchar(Sys.setlocale("LC_COLLATE", "C.UTF-8")))
base_pairs = list(c("\u00e9", "e\u0301"), c("\u00f6", "o\u0308"),
  c("\u00e5", "a\u030a"), c("\u00c5", "A\u030a"), c("\u00f1", "n\u0303"))
for (pair in base_pairs) {
  if (!is.null(environment_example)) break
  for (prefix in as.character(0:1000)) {
    symbol_names = paste0(prefix, pair)
    search_count = search_count + 1L
    first = CnfUniverse()
    second = CnfUniverse()
    for (i in 1:2) CnfSymbol(first, symbol_names[[i]], c("a", "b", as.character(i)))
    for (i in 2:1) CnfSymbol(second, symbol_names[[i]], c("a", "b", as.character(i)))
    left_names = names(as.list.environment(first, sorted = TRUE))
    right_names = names(as.list.environment(second, sorted = TRUE))
    if (identical(left_names, right_names)) next
    stopifnot(same_values(names(first), names(second)),
      all(vapply(symbol_names, function(name) identical(first[[name]], second[[name]]), logical(1))))
    left_atom = `$.CnfUniverse`(first, symbol_names[[1L]]) %among% "a"
    right_atom = `$.CnfUniverse`(second, symbol_names[[1L]]) %among% "a"
    environment_example = list(symbol_names = symbol_names,
      codepoints = lapply(symbol_names, utf8ToInt),
      unsorted_left_names = names(first), unsorted_right_names = names(second),
      sorted_left_names = left_names, sorted_right_names = right_names,
      payloads_match_by_name = TRUE,
      base_universe_comparison = capture(all.equal(first, second)))
    for (locale in c("C", "C.UTF-8", "en_US.UTF-8")) {
      stopifnot(nzchar(Sys.setlocale("LC_COLLATE", locale)))
      for (class_name in c("CnfAtom", "CnfClause", "CnfFormula")) {
        coerce = get(paste0("as.", class_name))
        record_scope("universe_binding_collation", coerce(left_atom), coerce(right_atom))
      }
    }
    break
  }
}
invisible(Sys.setlocale("LC_COLLATE", initial_locale))

# These scope cases intentionally preserve the original comparison contract.
stopifnot(all(vapply(records, function(record) identical(record$original, record$guarded), logical(1))))

# ... still reaches base comparison: ignoring attributes can ignore universes,
# but does not discard the proper clause's symbol names or string payloads.
attribute_controls = list()
for (class_name in c("CnfAtom", "CnfClause", "CnfFormula")) {
  coerce = get(paste0("as.", class_name))
  left = coerce(x %among% "a")
  right = coerce(y %among% "a")
  answer = capture(guarded_candidate_equal(left, right, check.attributes = FALSE))
  stopifnot(answer$equal, is.null(answer$error))
  attribute_controls[[class_name]] = answer
}

write_results(list(metadata = metadata(), records = records,
  environment_search_pairs = search_count, environment_example = environment_example,
  forwarded_attribute_controls = attribute_controls), "scope")
