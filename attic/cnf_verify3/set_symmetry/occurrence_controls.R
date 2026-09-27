# Independent trace checks for the occurrence-preserving extension. These are
# accepted positive matrix clause selectors, including an error and a stale
# positional result. This file makes no semantic-correctness assertion.
suppressPackageStartupMessages({library(checkmate); library(mlr3misc); library(jsonlite)})
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
plain_simplify = simplify_cnf
source("attic/cnf_verify3/set_symmetry/harness.R")
observed = symmetry_instrument(plain_simplify)
here = "attic/cnf_verify3/set_symmetry"
bank = readRDS(file.path(here, "checks_r36.rds"))$cases
capture = function(expr) {
  warnings = character()
  result = tryCatch(withCallingHandlers(
    list(output = symmetry_bare(force(expr))),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) list(error = conditionMessage(e)))
  c(result, list(warnings = warnings))
}
run = function(case) {
  universe = symmetry_universe(case$domains)
  selected = lapply(case$clauses, function(clause) {
    unique_clause = clause[!duplicated(names(clause))]
    base = symmetry_public_clauses(list(unique_clause), universe)[[1L]]
    base[matrix(match(names(clause), names(unique_clause)), nrow = 1L)]
  })
  stopifnot(identical(lapply(selected, symmetry_bare), case$clauses))
  public = capture(CnfFormula(selected))
  baseline = capture(plain_simplify(case$clauses, universe))
  symmetry_trace <<- list()
  instrumented = capture(observed(case$clauses, universe))
  stopifnot(identical(public, baseline), identical(baseline, instrumented))
  list(outcome = baseline, trace = symmetry_trace)
}
set.seed(906169L)
cases = list(
  list(label = "accepted_empty_unit_hla_selector", domains = list(X = letters[1:3]),
    clauses = list(setNames(list("a", "a"), c("X", "X")), list(X = c("a", "b")))),
  list(label = "accepted_stale_occurrence", domains = list(X = letters[1:2], Y = letters[1:3]),
    clauses = list(list(Y = c("b", "c"), X = "a"),
      setNames(list(c("c", "a"), "a", c("c", "a")), c("Y", "X", "Y")))))
for (case in bank[16:55]) {
  case$clauses = lapply(case$clauses, function(clause) {
    width = length(clause)
    positions = c(seq_len(width), sample.int(width, sample.int(3L, 1L), replace = TRUE))
    clause[positions[sample.int(length(positions))]]
  })
  cases[[length(cases) + 1L]] = case
}
counts = c(normal = 0L, error = 0L, transformations = 0L, events = 0L)
records = lapply(cases, function(case) {
  original = run(case)
  category = if (is.null(original$outcome$error)) "normal" else "error"
  counts[[category]] <<- counts[[category]] + 1L
  fibers = symmetry_fibers(case, split = TRUE)
  transformed_case = symmetry_transform(case, fibers, reorder = TRUE)
  # A selector copies the same stored vector. Keep those copies identically
  # ordered so this extension control still has an exact accepted API route.
  transformed_case$clauses = lapply(transformed_case$clauses, function(clause) {
    for (i in seq_along(clause)) clause[[i]] = clause[[match(names(clause)[[i]], names(clause))]]
    clause
  })
  transformed = run(transformed_case)
  quotient = symmetry_quotient(case)
  collapsed = run(quotient$case)
  stopifnot(identical(original$trace, transformed$trace), identical(original$trace, collapsed$trace),
    identical(original$outcome$error, transformed$outcome$error),
    identical(original$outcome$error, collapsed$outcome$error),
    identical(original$outcome$warnings, transformed$outcome$warnings),
    identical(original$outcome$warnings, collapsed$outcome$warnings))
  if (category == "normal") {
    stopifnot(identical(symmetry_project(original$outcome$output, symmetry_fibers(case)),
      symmetry_project(transformed$outcome$output, fibers)),
      identical(symmetry_project(original$outcome$output, quotient$fibers),
        symmetry_project(collapsed$outcome$output, symmetry_fibers(quotient$case))))
  }
  counts[["transformations"]] <<- counts[["transformations"]] + 2L
  counts[["events"]] <<- counts[["events"]] + length(original$trace) * 3L
  list(case = case, original = original)
})
stopifnot(!is.null(records[[1L]]$original$outcome$error),
  is.null(records[[2L]]$original$outcome$error),
  anyDuplicated(names(records[[2L]]$original$outcome$output[[1L]])) > 0L)
suffix = if (getRversion() >= "4.6") "r46" else "r36"
saveRDS(list(version = R.version.string, counts = counts, records = records),
  file.path(here, paste0("occurrence_controls_", suffix, ".rds")))
write_json(list(version = R.version.string, counts = as.list(counts), success = TRUE),
  file.path(here, paste0("occurrence_controls_", suffix, ".json")), auto_unbox = TRUE, pretty = TRUE)
cat("PASS:", length(records), "accepted occurrence cases,", counts[["transformations"]],
  "trace-matched refinements/quotients;", counts[["error"]], "baseline errors preserved.\n")
