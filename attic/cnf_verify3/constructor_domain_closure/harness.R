# Run from the repository root. All production function bodies are sourced
# unchanged, and constructor argument checks are provided by real checkmate.
suppressPackageStartupMessages(library(checkmate))
suppressPackageStartupMessages(library(jsonlite))
closure_here = "attic/cnf_verify3/constructor_domain_closure"
closure_version = if (getRversion() < "4.0.0") "r36" else "r46"
stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
map_chr = function(.x, .f, ...) vapply(.x, .f, character(1L), ...)
closure_sources = file.path("R", paste0(c("CnfUniverse", "CnfSymbol", "CnfAtom",
  "CnfClause", "CnfFormula", "CnfFormula_simplify"), ".R"))
for (path in closure_sources) source(path)

# Positional extraction is intentional: as.list.CnfClause changes its contents.
flat_values = function(x) {
  if (!length(x)) return(character())
  vapply(seq_along(x), function(i) x[[i]], character(1L), USE.NAMES = FALSE)
}
flat_unique = function(x) {
  x = flat_values(x)
  x[!duplicated(x)]
}
plain_character = function(x) is.character(x) && is.null(attributes(x)) &&
  !anyNA(x) && !anyDuplicated(x)
proper_clause = function(x, universe) {
  if (!is.list(x) || !length(x)) return(FALSE)
  symbols = names(x)
  if (is.null(symbols) || anyNA(symbols) || anyDuplicated(symbols) ||
      any(!symbols %in% names(universe))) return(FALSE)
  all(vapply(seq_along(x), function(i) {
    values = x[[i]]
    domain = flat_unique(universe[[symbols[[i]]]])
    plain_character(values) && length(values) > 0L &&
      all(values %in% domain) && !all(domain %in% values)
  }, logical(1L)))
}
canonical_object = function(x) {
  if (is.logical(x)) return(length(x) == 1L && !anyNA(x))
  if (inherits(x, "CnfAtom")) {
    values = flat_unique(x$values)
    domain = flat_unique(attr(x, "universe")[[x$symbol]])
    return(length(values) > 0L && all(values %in% domain) && !all(domain %in% values))
  }
  if (inherits(x, "CnfClause")) return(proper_clause(unclass(x), attr(x, "universe")))
  if (!inherits(x, "CnfFormula") || !length(x)) return(FALSE)
  all(vapply(unclass(x), proper_clause, logical(1L), universe = attr(x, "universe")))
}
bare_payload = function(x) {
  attributes(x) = if (is.list(x)) list(names = names(x)) else NULL
  x
}
literal_truth = function(symbol, values, assignments) {
  # Independent equality comparisons, with no constructor or set operation.
  values = flat_values(values)
  vapply(assignments[[symbol]], function(value) any(value == values), logical(1L), USE.NAMES = FALSE)
}
raw_clause_truth = function(clause, assignments) {
  if (is.logical(clause)) return(rep(as.vector(clause), nrow(assignments)))
  answer = rep(FALSE, nrow(assignments))
  for (i in seq_along(clause)) {
    answer = answer | literal_truth(names(clause)[[i]], clause[[i]], assignments)
  }
  answer
}
object_truth = function(x, assignments) {
  if (is.logical(x)) return(rep(as.vector(x), nrow(assignments)))
  if (inherits(x, "CnfAtom")) return(literal_truth(x$symbol, x$values, assignments))
  if (inherits(x, "CnfClause")) return(raw_clause_truth(unclass(x), assignments))
  answer = rep(TRUE, nrow(assignments))
  for (clause in unclass(x)) answer = answer & raw_clause_truth(clause, assignments)
  answer
}
shapes = function(x) {
  size = length(x)
  result = list(plain = x,
    named = setNames(x, rep(c(NA_character_, "", "same"), length.out = size)),
    metadata = structure(x, audit = list(note = "ordinary metadata", integer = 1:2), comment = "inert"),
    one_array = array(x, size),
    row_matrix = matrix(x, nrow = 1L),
    column_matrix = matrix(x, ncol = 1L),
    first_array = array(x, c(size, 1L, 1L)),
    last_array = array(x, c(1L, 1L, size)),
    as_is = I(x),
    inert_class = structure(x, class = "cnf_closure_no_methods"))
  result$named_matrix = matrix(x, nrow = 1L,
    dimnames = list("row", rep(c("", "same"), length.out = size)))
  names(result$named_matrix) = rep(c(NA_character_, "", "same"), length.out = size)
  if (size %% 2L == 0L) {
    result$two_matrix = matrix(x, nrow = 2L)
    result$two_array = array(x, c(2L, 1L, size %/% 2L))
  }
  result
}
subsets = function(x, proper = FALSE) {
  masks = expand.grid(rep(list(c(FALSE, TRUE)), length(x)))
  result = lapply(seq_len(nrow(masks)), function(i) x[as.logical(masks[i, ])])
  if (proper) result = Filter(function(v) length(v) && length(v) < length(x), result)
  result
}
record_results = function(stem, counts, extra = list()) {
  report = c(list(R = R.version.string, checkmate = as.character(packageVersion("checkmate")),
    source_md5 = as.list(tools::md5sum(closure_sources)), counts = as.list(counts)), extra)
  write_json(report, file.path(closure_here, paste0(stem, "_", closure_version, ".json")),
    pretty = TRUE, auto_unbox = TRUE)
  cat(toJSON(report, pretty = TRUE, auto_unbox = TRUE), "\n")
}
attempt = function(expr) {
  warnings = character()
  value = withCallingHandlers(tryCatch(force(expr), error = identity), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warnings,
    error = if (inherits(value, "error")) conditionMessage(value) else "")
}
