source("attic/cnf_verify3/selector_semantic_trace/common.R")
suffix = if (getRversion() < "4.0.0") "r36" else "r46"
hashes = system2("sha256sum", source_paths, stdout = TRUE)
writeLines(hashes, file.path(trace_dir, "SOURCE_HASHES.sha256"))

show_clause = function(clause) {
  if (is.logical(clause)) return(as.character(clause))
  paste(vapply(seq_along(clause), function(i) {
    paste0(i, ":", names(clause)[[i]], "={", paste(clause[[i]], collapse = ","), "}")
  }, character(1)), collapse = " OR ")
}
show_formula = function(formula) {
  if (is.logical(formula)) return(as.character(formula))
  paste(vapply(formula, function(clause) paste0("[", show_clause(clause), "]"), character(1)), collapse = " AND ")
}
rows_text = function(rows) if (length(rows)) paste(rows, collapse = ",") else "-"

runs = lapply(case_specs, run_trace_case)
repaired = lapply(case_specs[c("four_clause", "stale_range", "unit_hla_error")],
  run_trace_case, candidate = TRUE)
stopifnot(identical(runs$four_clause$output, list(list(X = "c"), list(Y = "a"))),
  !any(runs$four_clause$expected), identical(repaired$four_clause$output, FALSE),
  grepl("attempt to select less than one element", runs$unit_hla_error$error))
for (run in repaired) {
  stopifnot(is.null(run$error), identical(truth_formula(run$output, run$assignments), run$expected),
    all(vapply(run$events, function(event) identical(event$truth$positional, run$expected), logical(1))))
}

for (name in names(runs)) {
  run = runs[[name]]
  cat("\nCASE:", name, "\n")
  cat("Input:", show_formula(run$input), "\n")
  if (is.null(run$error)) cat("Output:", show_formula(run$output), "\n") else cat("Error:", run$error, "\n")
  cat("Truth-table assignments:\n")
  print(data.frame(run$assignments, input = run$expected,
    positional = tail(run$events, 1L)[[1L]]$truth$positional,
    projection = tail(run$events, 1L)[[1L]]$truth$projection))
  for (commit in run$evidence$commits) {
    cat("\nEvent", commit$event, "source line", commit$line, "clause", commit$clause_idx, "\n")
    cat("Before:", show_clause(commit$before), "\nAfter: ", show_clause(commit$after), "\n")
    cat("Still-live context:", show_formula(commit$context), "\n")
    for (meaning in names(commit$checks)) {
      check = commit$checks[[meaning]]
      cat(meaning, "raw delta", rows_text(check$raw_delta), "under live units", rows_text(check$unit_context_delta),
        "under all live constraints", rows_text(check$live_context_delta), "\n")
    }
  }
  for (proposal in run$evidence$proposals) {
    cat("\nHLA proposal event", proposal$event, "source line", proposal$line, "target", proposal$clause_idx,
      "donor", proposal$donor, "symbol", proposal$symbol, "\n")
    cat("Before:", show_clause(proposal$before), "\nAfter: ", show_clause(proposal$after), "\n")
    for (meaning in names(proposal$checks)) {
      check = proposal$checks[[meaning]]
      stopifnot(length(check$live_context_delta) == 0L)
      cat(meaning, "raw delta", rows_text(check$raw_delta),
        "under all live constraints", rows_text(check$live_context_delta), "\n")
    }
  }
  for (unit in run$evidence$units) {
    cat("\nUnit registry commit event", unit$event, "source line", unit$line,
      "clause", unit$clause_idx, ":", show_clause(unit$after), "\n")
    for (check in unit$checks) stopifnot(length(check$live_context_delta) == 0L)
    cat("The independently interpreted live formula already entails this recorded unit.\n")
  }
  rows = lapply(run$events, function(event) {
    data.frame(event = event$event, source_line = event$line, kind = event$kind, changed = event$changed,
      positional_models = rows_text(which(event$truth$positional)),
      projection_models = rows_text(which(event$truth$projection)),
      positional_delta = rows_text(event$delta$positional), projection_delta = rows_text(event$delta$projection),
      live = show_formula(event$live), stringsAsFactors = FALSE)
  })
  write.table(do.call(rbind, rows), file.path(trace_dir, paste0(name, "_events_", suffix, ".tsv")),
    sep = "\t", row.names = FALSE, quote = TRUE)
}
result = list(R_version = R.version.string, checkmate = as.character(packageVersion("checkmate")),
  hashes = hashes, runs = runs, repaired = repaired)
saveRDS(result, file.path(trace_dir, paste0("results_", suffix, ".rds")))
stopifnot(identical(readRDS(file.path(trace_dir, paste0("results_", suffix, ".rds"))), result),
  identical(hashes, system2("sha256sum", source_paths, stdout = TRUE)))
cat("\nAll unchanged/instrumented outputs agree, including duplicate positions and the runtime error.\n")
cat("R:", result$R_version, "; checkmate:", result$checkmate, "\n")
