# Public CNF equality checks on ordinary Unicode labels and encoding variants.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}

canonical_range = function(values) sort(enc2utf8(values), method = "radix")
canonical_clause = function(clause) {
  bare = unclass(clause)
  if (is.logical(bare)) return(clause)
  ordering = order(enc2utf8(names(bare)), method = "radix")
  clause[] = lapply(bare[ordering], canonical_range)
  names(clause) = enc2utf8(names(bare)[ordering])
  clause
}
candidate_equal = function(target, current) {
  if (inherits(target, "CnfAtom")) {
    target$symbol = enc2utf8(target$symbol)
    current$symbol = enc2utf8(current$symbol)
    target$values = canonical_range(target$values)
    current$values = canonical_range(current$values)
  } else if (inherits(target, "CnfClause")) {
    target = canonical_clause(target)
    current = canonical_clause(current)
  } else {
    normalize_formula = function(formula) {
      formula[] = lapply(unclass(formula), canonical_clause)
      keys = vapply(unclass(formula), function(clause) {
        paste0(paste(names(clause), collapse = ".__."), digest::digest(c(clause), algo = "xxhash64"))
      }, "")
      formula[] = formula[order(keys, method = "radix")]
      formula
    }
    target = normalize_formula(target)
    current = normalize_formula(current)
  }
  all.equal.list(target, current)
}

record_pair = function(kind, target, current, locale) {
  answer = all.equal(target, current)
  candidate = candidate_equal(target, current)
  stopifnot(isTRUE(candidate))
  list(kind = kind, locale = locale, class = class(target),
    identical = identical(target, current), original_equal = isTRUE(answer),
    original_message = if (isTRUE(answer)) NULL else answer,
    candidate_equal = isTRUE(candidate))
}

original_locale = Sys.getlocale("LC_COLLATE")
records = list()
append_record = function(...) records[[length(records) + 1L]] <<- record_pair(...)
for (locale in unique(c(original_locale, "C", "C.UTF-8", "en_US.UTF-8"))) {
  available = suppressWarnings(Sys.setlocale("LC_COLLATE", locale))
  if (!nzchar(available)) next
  # Distinct ordinary Unicode values that have equal collation weight.
  values = c("\u00e9", "e\u0301")
  stopifnot(!identical(values[[1L]], values[[2L]]))
  universe = CnfUniverse()
  X = CnfSymbol(universe, "X", c(values, "other"))
  first = X %among% values
  second = X %among% rev(values)
  stopifnot(setequal(first$values, second$values))
  append_record("collation_value_order", first, second, available)
  append_record("collation_value_order", as.CnfClause(first), as.CnfClause(second), available)
  append_record("collation_value_order", as.CnfFormula(first), as.CnfFormula(second), available)

  # The same collation tie in distinct, valid symbol names.
  universe = CnfUniverse()
  X = CnfSymbol(universe, values[[1L]], c("0", "1"))
  Y = CnfSymbol(universe, values[[2L]], c("0", "1"))
  first = CnfClause(list(X %among% "0", Y %among% "1"))
  second = CnfClause(list(Y %among% "1", X %among% "0"))
  append_record("collation_symbol_order", first, second, available)
  append_record("collation_symbol_order", as.CnfFormula(first), as.CnfFormula(second), available)

  # Encodings denote equal R strings, but digest serializes encoding marks.
  values = c("\u00e9", "\u00f6")
  latin = iconv(values, to = "latin1")
  stopifnot(identical(values, latin), !identical(Encoding(values), Encoding(latin)))
  universe = CnfUniverse()
  X = CnfSymbol(universe, "X", c(values, "other"))
  Y = CnfSymbol(universe, "Y", c("a", "b", "c"))
  make_formula = function(v) CnfFormula(list(
    CnfClause(list(X %among% v[[1L]], Y %among% "a")),
    CnfClause(list(X %among% v[[2L]], Y %among% "b"))
  ))
  first = make_formula(values)
  second = make_formula(latin)
  stopifnot(identical(first, second))
  append_record("encoding_hash_order", first, second, available)
  append_record("encoding_hash_clause_control", as.list(first)[[1L]], as.list(second)[[1L]], available)
  append_record("encoding_hash_clause_control", as.list(first)[[2L]], as.list(second)[[2L]], available)

  # Independent concrete truth comparison; no comparison-helper oracle.
  assignments = expand.grid(X = c(values, "other"), Y = c("a", "b", "c"), stringsAsFactors = FALSE)
  evaluate = function(formula) {
    Reduce(`&`, lapply(unclass(formula), function(clause) {
      Reduce(`|`, lapply(names(clause), function(symbol) assignments[[symbol]] %in% clause[[symbol]]))
    }))
  }
  stopifnot(identical(evaluate(first), evaluate(second)), sum(evaluate(first)) == 2L)
}
invisible(Sys.setlocale("LC_COLLATE", original_locale))

result = list(runtime = as.character(getRversion()), original_locale = original_locale,
  records = records, calls = length(records),
  false_negatives = sum(!vapply(records, function(record) record$original_equal, logical(1))),
  identical_false_negatives = sum(vapply(records, function(record) record$identical && !record$original_equal, logical(1))),
  candidate_equal = all(vapply(records, function(record) record$candidate_equal, logical(1))),
  production_unchanged = TRUE)
prefix = if (getRversion() >= "4.0") "comparison_normalization_r46" else "comparison_normalization_r36"
write_json(result, file.path("attic/cnf_verify3/root", paste0(prefix, ".json")), pretty = TRUE, auto_unbox = TRUE)
saveRDS(result, file.path("attic/cnf_verify3/root", paste0(prefix, ".rds")), version = 2)
cat(toJSON(result, pretty = TRUE, auto_unbox = TRUE), "\n")
