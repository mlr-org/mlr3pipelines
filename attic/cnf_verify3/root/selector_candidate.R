# A diagnostic boundary repair, evaluated privately; production is unchanged.
suppressPackageStartupMessages({ library(checkmate); library(mlr3misc); library(jsonlite) })
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
production_subset = `[.CnfClause`
candidate_text = paste(deparse(production_subset, width.cutoff = 500L), collapse = "\n")
stopifnot(grepl("i = unclass(i)", candidate_text, fixed = TRUE),
  grepl("check_logical(i, len = true_length)", candidate_text, fixed = TRUE))
candidate_text = sub("i = unclass(i)", "i = as.vector(unclass(i))", candidate_text, fixed = TRUE)
candidate_text = sub("check_logical(i, len = true_length)",
  "check_logical(i, len = true_length, any.missing = FALSE)", candidate_text, fixed = TRUE)
candidate_subset = eval(parse(text = candidate_text), new.env(parent = globalenv()))
attempt = function(expr) tryCatch(list(ok = TRUE, value = force(expr)),
  error = function(condition) list(ok = FALSE, error = conditionMessage(condition)))
universe = CnfUniverse()
symbols = setNames(lapply(c("X", "Y", "Z"), function(name) CnfSymbol(universe, name, c("a", "b", "c"))), c("X", "Y", "Z"))
clauses = lapply(seq_len(3L), function(n) CnfClause(lapply(seq_len(n), function(i) CnfAtom(symbols[[i]], letters[[i]]))))
clauses = c(clauses, list(as.CnfClause(structure(TRUE, universe = universe)), as.CnfClause(structure(FALSE, universe = universe))))
grid = expand.grid(X = letters[1:3], Y = letters[1:3], Z = letters[1:3], stringsAsFactors = FALSE)
evaluate = function(clause) {
  if (is.logical(clause)) return(rep(as.vector(clause), nrow(grid)))
  truth = rep(FALSE, nrow(grid))
  for (i in seq_along(clause)) truth = truth | grid[[names(clause)[[i]]]] %in% clause[[i]]
  truth
}
canonical = function(clause) {
  if (is.logical(clause)) return(length(clause) == 1L && !anyNA(clause))
  clause = unclass(clause)
  !anyNA(names(clause)) && !anyDuplicated(names(clause)) && all(vapply(clause, function(range) {
    is.character(range) && length(range) > 0L && !anyNA(range) && !anyDuplicated(range)
  }, FALSE))
}
stopifnot(all(vapply(clauses, canonical, FALSE)))
selection_truth = function(clause, flat) {
  if (is.logical(clause)) {
    selected = length(flat) > 0L
    if (is.numeric(flat)) selected = any(floor(flat) > 0)
    if (is.logical(flat)) selected = any(flat)
    return(rep(isTRUE(clause) && selected, nrow(grid)))
  }
  indices = if (is.numeric(flat)) floor(flat[flat >= 1]) else if (is.character(flat)) {
    match(flat, names(clause))
  } else if (is.logical(flat)) which(flat) else integer()
  answer = rep(FALSE, nrow(grid))
  for (index in indices) answer = answer | grid[[names(clause)[[index]]]] %in% clause[[index]]
  answer
}
vectors = list(NULL, integer(), character(), logical(), raw(), complex(), 0, FALSE)
for (bank in list(c(-1, 0, 1, 2, 3, 1.9, Inf, NA_real_), c("X", "Y", "Z", "absent", NA_character_), c(FALSE, TRUE, NA))) {
  for (n in seq_len(4L)) {
    combinations = expand.grid(rep(list(bank), n), stringsAsFactors = FALSE)
    vectors = c(vectors, lapply(seq_len(nrow(combinations)), function(i) unname(unlist(combinations[i, ], use.names = FALSE))))
  }
}
stats = list(calls = 0L, candidate_accepted = 0L, rejected_missing_selectors = 0L,
  baseline_noncanonical = 0L, exact_flat_reference_matches = 0L)
for (selector in vectors) {
  variants = list(selector)
  if (!is.null(selector)) variants = c(variants, list(setNames(selector, rep("metadata", length(selector)))))
  if (length(selector)) variants = c(variants, list(matrix(selector, nrow = 1L),
    array(selector, dim = c(1L, 1L, length(selector)))))
  for (index in variants) for (clause in clauses) {
    stats$calls = stats$calls + 1L
    actual = attempt(candidate_subset(clause, index))
    flat = as.vector(unclass(index))
    reference = attempt(production_subset(clause, flat))
    baseline = attempt(production_subset(clause, index))
    if (baseline$ok && !canonical(baseline$value)) stats$baseline_noncanonical = stats$baseline_noncanonical + 1L
    # checkmate also accepts an all-missing numeric vector as logical when
    # missing values are allowed, so this exception is about values, not type.
    if (anyNA(flat)) {
      stopifnot(!actual$ok)
      stats$rejected_missing_selectors = stats$rejected_missing_selectors + 1L
      next
    }
    stopifnot(identical(actual$ok, reference$ok))
    if (!actual$ok) next
    if (!canonical(actual$value)) {
      dput(list(index = index, flat = flat, input = c(clause), actual = c(actual$value)))
      stop("Diagnostic selector returned a noncanonical clause")
    }
    stopifnot(canonical(actual$value), identical(actual$value, reference$value),
      identical(attr(actual$value, "universe"), attr(clause, "universe")),
      identical(evaluate(actual$value), selection_truth(clause, flat)))
    stats$candidate_accepted = stats$candidate_accepted + 1L
    stats$exact_flat_reference_matches = stats$exact_flat_reference_matches + 1L
  }
}
# Reconstruct the strongest saved semantic failure through the private selector.
X = symbols$X
Y = symbols$Y
bad_anchor = as.CnfClause(CnfAtom(X, c("b", "c")))
others = list(CnfClause(list(CnfAtom(X, "b"), CnfAtom(Y, "c"))),
  CnfClause(list(CnfAtom(X, "c"), CnfAtom(Y, "b"))),
  CnfClause(list(CnfAtom(X, "a"), CnfAtom(Y, "a"))))
bad = CnfFormula(c(list(production_subset(bad_anchor, matrix(c(1L, 1L), nrow = 1L))), others))
repaired = CnfFormula(c(list(candidate_subset(bad_anchor, matrix(c(1L, 1L), nrow = 1L))), others))
stopifnot(!is.logical(bad), isFALSE(repaired))
# The two-clause unit-HLA error becomes the intended ordinary unit.
narrow = as.CnfClause(CnfAtom(X, "a"))
broad = as.CnfClause(CnfAtom(X, c("a", "b")))
stopifnot(!attempt(CnfFormula(list(production_subset(narrow, matrix(c(1L, 1L), 1L)), broad)))$ok)
unit_result = CnfFormula(list(candidate_subset(narrow, matrix(c(1L, 1L), 1L)), broad))
stopifnot(identical(unit_result, CnfFormula(list(narrow))))
result = list(R_version = R.version.string, stats = stats,
  canonical_contradiction_repaired = isFALSE(repaired), unary_HLA_error_repaired = TRUE,
  production_subset_unchanged = identical(production_subset, `[.CnfClause`))
suffix = if (getRversion() < "4.0.0") "r36" else "r46"
writeLines(toJSON(result, auto_unbox = TRUE, pretty = TRUE),
  paste0("attic/cnf_verify3/root/selector_candidate_", suffix, ".json"))
print(result)
