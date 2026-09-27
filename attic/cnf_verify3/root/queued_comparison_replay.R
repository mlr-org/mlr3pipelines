# Replay the reduced private-copy control without repeating the search.
suppressPackageStartupMessages({
  library(checkmate)
  library(mlr3misc)
  library(jsonlite)
})
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
case = readRDS("attic/cnf_verify3/root/queued_comparison_control.rds")$case
source_text = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
guard = "      if (!is_not_subset_of[[meta_idx]][other_meta_idx, is_not_subset_of_col]) next"
decrement = "      not_subset_count[meta_idx, other_meta_idx] <<- (rowsum = not_subset_count[meta_idx, other_meta_idx] - 1L)"
stopifnot(grepl(guard, source_text, fixed = TRUE), grepl(decrement, source_text, fixed = TRUE))
observed_guard = paste(
  "      if (!is_not_subset_of[[meta_idx]][other_meta_idx, is_not_subset_of_col]) {",
  "        .record(\"already cleared\", clause_idx, clause_idx_other, symbol,",
  "          not_subset_count[meta_idx, other_meta_idx],",
  "          sum(is_not_subset_of[[meta_idx]][other_meta_idx, ]))",
  "        next",
  "      }", sep = "\n")
observed_decrement = paste(decrement,
  "      if (rowsum != sum(is_not_subset_of[[meta_idx]][other_meta_idx, ])) {",
  "        .record(\"count differs from row\", clause_idx, clause_idx_other, symbol,",
  "          rowsum, sum(is_not_subset_of[[meta_idx]][other_meta_idx, ]))",
  "      }", sep = "\n")

evaluate = function(clauses, assignments) {
  if (is.logical(clauses)) return(rep(as.vector(clauses), nrow(assignments)))
  answer = rep(TRUE, nrow(assignments))
  for (clause in clauses) {
    satisfies = rep(FALSE, nrow(assignments))
    for (symbol in names(clause)) satisfies = satisfies | assignments[[symbol]] %in% clause[[symbol]]
    answer = answer & satisfies
  }
  answer
}
grid = expand.grid(case$domains, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
expected = evaluate(case$clauses, grid)
results = list()
universe = CnfUniverse()
for (symbol in names(case$domains)) CnfSymbol(universe, symbol, case$domains[[symbol]])
for (mode in c("production", "observed_production", "changed_copy", "observed_changed_copy")) {
  events = list()
  warnings = character()
  environment = new.env(parent = globalenv())
  environment$.record = function(kind, donor, target, symbol, stored_count, row_sum) {
    if (length(events) >= 12L) return(invisible(NULL))
    events[[length(events) + 1L]] <<- list(kind = kind, donor = donor, target = target,
      symbol = symbol, stored_count = stored_count, row_sum = row_sum,
      call_heads = vapply(sys.calls(), function(call) paste(deparse(call[[1L]]), collapse = " "), ""))
    invisible(NULL)
  }
  code = source_text
  if (grepl("observed", mode, fixed = TRUE)) {
    replacement = if (grepl("changed", mode, fixed = TRUE)) {
      sub("        next\n", "", observed_guard, fixed = TRUE)
    } else observed_guard
    code = sub(guard, replacement, code, fixed = TRUE)
    code = sub(decrement, observed_decrement, code, fixed = TRUE)
  } else if (mode == "changed_copy") code = sub(guard, "", code, fixed = TRUE)
  eval(parse(text = code), environment)
  error = NULL
  output = tryCatch(withCallingHandlers(environment$simplify_cnf(case$clauses, universe),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }), error = function(condition) { error <<- conditionMessage(condition); NULL })
  results[[mode]] = list(error = error, warnings = unique(warnings), events = events,
    output = unclass(output), agrees_with_truth_table = if (is.null(error)) {
      identical(evaluate(unclass(output), grid), expected)
    } else NULL)
}
stopifnot(isTRUE(results$production$agrees_with_truth_table),
  identical(results$production$output, results$observed_production$output),
  length(results$observed_production$events) > 0L,
  !is.null(results$changed_copy$error), !is.null(results$observed_changed_copy$error),
  any(vapply(results$observed_changed_copy$events, function(event) {
    event$kind == "count differs from row" && event$stored_count < event$row_sum
  }, FALSE)))
result = list(R_version = R.version.string, valuations = nrow(grid), models = sum(expected), modes = results)
suffix = if (getRversion() < "4.0.0") "r36" else "r46"
writeLines(toJSON(result, auto_unbox = TRUE, pretty = TRUE, null = "null"),
  paste0("attic/cnf_verify3/root/queued_comparison_replay_", suffix, ".json"))
cat(R.version.string, ": both original copies preserve", sum(expected), "models in", nrow(grid), "assignments.\n")
cat("Changed copy:", results$changed_copy$error, "\n")
for (event in results$observed_changed_copy$events) {
  cat(event$kind, ": donor", event$donor, "target", event$target, "symbol", event$symbol,
    "stored", event$stored_count, "actual", event$row_sum, "\n")
}
