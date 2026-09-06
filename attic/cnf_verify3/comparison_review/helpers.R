# Shared, independent comparison-review helpers. Run from the repository root.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
for (source_name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(source_name, ".R")))
}

review_dir = "attic/cnf_verify3/comparison_review"
runtime_tag = if (getRversion() < "4.0") "r36" else "r46"

# Evaluate only the three candidate definitions, without running the parent's
# experiments or writing to its directory. Its production methods stay intact.
candidate_source = "attic/cnf_verify3/root/comparison_normalization.R"
candidate_names = c("canonical_range", "canonical_clause", "candidate_equal")
candidate_env = new.env(parent = globalenv())
candidate_expressions = Filter(function(expression) {
  is.call(expression) && identical(expression[[1L]], as.name("=")) &&
    as.character(expression[[2L]]) %in% candidate_names
}, as.list(parse(candidate_source)))
stopifnot(length(candidate_expressions) == length(candidate_names))
for (expression in candidate_expressions) eval(expression, candidate_env)
candidate_equal = candidate_env$candidate_equal

capture = function(expression) {
  tryCatch({
    answer = force(expression)
    list(equal = isTRUE(answer), message = if (isTRUE(answer)) NULL else answer, error = NULL)
  }, error = function(error) list(equal = FALSE, message = NULL, error = conditionMessage(error)))
}

# The independent oracle matches exact R strings by membership; no sorting,
# transcoding, digest, or all.equal method participates in matching payloads.
same_values = function(left, right) {
  length(left) == length(right) && all(left %in% right) && all(right %in% left)
}
same_clause = function(left, right) {
  left = unclass(left)
  right = unclass(right)
  if (is.logical(left) || is.logical(right)) return(identical(c(left), c(right)))
  stopifnot(!anyDuplicated(names(left)), !anyDuplicated(names(right)))
  if (!same_values(names(left), names(right))) return(FALSE)
  corresponding = match(names(left), names(right))
  all(vapply(seq_along(left), function(i) same_values(left[[i]], right[[corresponding[[i]]]]), logical(1)))
}
same_formula = function(left, right) {
  left = unclass(left)
  right = unclass(right)
  if (is.logical(left) || is.logical(right)) return(identical(c(left), c(right)))
  if (length(left) != length(right)) return(FALSE)
  used = rep(FALSE, length(right))
  for (i in seq_along(left)) {
    matches = which(!used & vapply(right, function(clause) same_clause(left[[i]], clause), logical(1)))
    if (!length(matches)) return(FALSE)
    used[matches[[1L]]] = TRUE
  }
  TRUE
}
same_payload = function(left, right) {
  stopifnot(identical(class(left), class(right)))
  if (is.logical(left) || is.logical(right)) return(identical(c(left), c(right)))
  if (inherits(left, "CnfAtom")) {
    identical(left$symbol, right$symbol) && same_values(left$values, right$values)
  } else if (inherits(left, "CnfClause")) {
    same_clause(left, right)
  } else {
    same_formula(left, right)
  }
}

evaluate_formula = function(formula, domains) {
  assignments = do.call(expand.grid, c(unname(domains), stringsAsFactors = FALSE))
  names(assignments) = names(domains)
  if (is.logical(formula)) return(rep(c(formula), nrow(assignments)))
  result = rep(TRUE, nrow(assignments))
  for (clause in unclass(formula)) {
    clause_result = rep(FALSE, nrow(assignments))
    for (i in seq_along(clause)) {
      position = match(names(clause)[[i]], names(domains))
      clause_result = clause_result | assignments[[position]] %in% clause[[i]]
    }
    result = result & clause_result
  }
  result
}

permutations = function(values) {
  if (length(values) < 2L) return(list(values))
  unlist(lapply(seq_along(values), function(i) {
    lapply(permutations(values[-i]), function(rest) c(values[[i]], rest))
  }), recursive = FALSE)
}

encode_as = function(values, encoding) {
  answer = switch(encoding,
    utf8 = enc2utf8(values),
    latin1 = iconv(values, from = "UTF-8", to = "latin1"),
    native = {
      # mark = FALSE is an ordinary iconv path to valid unmarked native text.
      iconv(values, from = "UTF-8", to = "", mark = FALSE)
    },
    stop("Unknown test encoding"))
  stopifnot(!anyNA(answer), identical(values, answer))
  answer
}

metadata = function() list(
  runtime = as.character(getRversion()),
  locale = Sys.getlocale(),
  package_versions = setNames(lapply(c("checkmate", "mlr3misc", "digest", "jsonlite"), function(package) {
    as.character(packageVersion(package))
  }), c("checkmate", "mlr3misc", "digest", "jsonlite")),
  source_md5 = as.list(tools::md5sum(c(candidate_source, file.path("R", paste0(c("CnfAtom", "CnfClause", "CnfFormula"), ".R")))))
)

write_results = function(results, name) {
  destination = file.path(review_dir, paste0(name, "_", runtime_tag))
  write_json(results, paste0(destination, ".json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
  saveRDS(results, paste0(destination, ".rds"), version = 2)
  cat(toJSON(results, pretty = TRUE, auto_unbox = TRUE, null = "null"), "\n")
}
