source("attic/cnf_verify3/selector_semantic_trace/common.R")
cnf = new_cnf_environment()
original = cnf$`[.CnfClause`
candidate = private_selector_candidate(cnf)
universe = cnf$CnfUniverse()
symbols = setNames(lapply(c("X", "Y", "Z"), function(name) {
  cnf$CnfSymbol(universe, name, letters[1:3])
}), c("X", "Y", "Z"))
assignments = expand.grid(X = letters[1:3], Y = letters[1:3], Z = letters[1:3],
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
counts = c(calls = 0L, accepted = 0L, missing_selectors_rejected = 0L,
  baseline_noncanonical = 0L, exact_flat_matches = 0L)

for (width in 1:3) {
  clause = cnf$CnfClause(lapply(seq_len(width), function(i) cnf$CnfAtom(symbols[[i]], letters[[i]])))
  selectors = list(NULL, integer(), character(), logical(), raw(), complex(), -1, Inf,
    NA_real_, NA_character_, rep(NA_real_, width), rep(NA_character_, width), "absent", c(1.9, 1.1))
  for (bank in list(0:width, names(symbols)[seq_len(width)], c(FALSE, TRUE, NA))) {
    for (length in 1:3) {
      words = expand.grid(rep(list(bank), length), stringsAsFactors = FALSE)
      selectors = c(selectors, lapply(seq_len(nrow(words)), function(row) {
        unname(unlist(words[row, ], use.names = FALSE))
      }))
    }
  }
  for (selector in selectors) {
    variants = list(selector)
    if (!is.null(selector)) variants = c(variants,
      list(setNames(selector, rep("metadata", length(selector)))))
    if (length(selector)) variants = c(variants, list(matrix(selector, nrow = 1L),
      matrix(selector, ncol = 1L), array(selector, dim = c(1L, length(selector), 1L))))
    for (index in variants) {
      counts[["calls"]] = counts[["calls"]] + 1L
      result = attempt(candidate(clause, index))
      baseline = attempt(original(clause, index))
      flat = as.vector(unclass(index))
      if (is.null(baseline$error) && !is.logical(baseline$value) &&
          (anyNA(names(baseline$value)) || anyDuplicated(names(baseline$value)))) {
        counts[["baseline_noncanonical"]] = counts[["baseline_noncanonical"]] + 1L
      }
      if (anyNA(flat)) {
        stopifnot(!is.null(result$error))
        counts[["missing_selectors_rejected"]] = counts[["missing_selectors_rejected"]] + 1L
        next
      }
      reference = attempt(original(clause, flat))
      if (!identical(is.null(result$error), is.null(reference$error))) {
        dput(list(width = width, index = index, result = result, reference = reference))
        stop("Private candidate and flattened reference disagree.")
      }
      if (!is.null(result$error)) next
      counts[["accepted"]] = counts[["accepted"]] + 1L
      stopifnot(identical(result$value, reference$value))
      counts[["exact_flat_matches"]] = counts[["exact_flat_matches"]] + 1L
      positions = if (is.numeric(flat)) floor(flat) else if (is.character(flat)) {
        match(flat, names(clause))
      } else if (is.logical(flat)) which(flat) else integer()
      expected = truth_clause(bare(clause)[positions], assignments)
      stopifnot(identical(truth_clause(bare(result$value), assignments), expected))
      if (!is.logical(result$value)) {
        stopifnot(!anyNA(names(result$value)), !anyDuplicated(names(result$value)),
          all(vapply(result$value, function(range) {
            is.character(range) && length(range) > 0L && !anyNA(range) && !anyDuplicated(range)
          }, logical(1))))
      }
    }
  }
}
stopifnot(identical(original, cnf$`[.CnfClause`))
suffix = if (getRversion() < "4.0.0") "r36" else "r46"
result = list(R_version = R.version.string, checkmate = as.character(packageVersion("checkmate")),
  counts = counts, production_unchanged = TRUE)
saveRDS(result, file.path(trace_dir, paste0("candidate_boundary_", suffix, ".rds")))
print(result)
