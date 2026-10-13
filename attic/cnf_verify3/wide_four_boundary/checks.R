suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
audit_dir = "attic/cnf_verify3/wide_four_boundary"
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}
source(file.path(audit_dir, "harness.R"))
plain = simplify_cnf
observed = padding_observer(plain)
source_case = read_json("attic/cnf_verify3/independent_solver/minimized_first_order_phase_sse1.json")
domains = lapply(source_case$domains, unlist, use.names = FALSE)
core = lapply(source_case$clauses, function(clause) lapply(clause, unlist, use.names = FALSE))
expected_first = list(list(X1 = "0", X0 = "5"), list(X0 = "0", X1 = c("4", "0")))
expected_second = list(list(X1 = "0", X0 = "5"), list(X1 = c("4", "0")))
expected_rule = list(list(donor = 1L, target = 2L, symbol = "X0", removed = "0"))
core_grid = expand.grid(domains, stringsAsFactors = FALSE)
core_truth = padding_truth(core, core_grid)
stopifnot(sum(core_truth) == 5L)
baseline = NULL
records = list()
traces = list()

for (padding in c(0L, 1L, 2L, 3L, 7L, 31L, 256L)) {
  padding_names = if (padding) paste0("P", seq_len(padding)) else character()
  padding_literals = setNames(rep(list("on"), padding), padding_names)
  padded = lapply(core, function(clause) c(clause, padding_literals))
  universe = CnfUniverse()
  for (symbol in names(domains)) CnfSymbol(universe, symbol, domains[[symbol]])
  for (symbol in padding_names) CnfSymbol(universe, symbol, c("off", "on"))
  public_clauses = lapply(padded, function(clause) CnfClause(lapply(seq_along(clause), function(i) {
    CnfAtom(`$.CnfUniverse`(universe, names(clause)[[i]]), clause[[i]])
  })))
  stopifnot(identical(lapply(public_clauses, c), padded), all(lengths(padded) == padding + 2L))
  first = CnfFormula(public_clauses)
  second = CnfFormula(as.list(first))
  stopifnot(identical(padding_core(c(first)), expected_first),
    identical(padding_core(c(second)), expected_second),
    identical(padding_sse1(c(first)), expected_rule), !length(padding_sse1(c(second))),
    identical(attr(first, "universe"), universe), identical(attr(second, "universe"), universe))
  # Confirm every ordered literal range, including every padding position.
  stopifnot(identical(c(first), lapply(expected_first, function(clause) c(clause, padding_literals))),
    identical(c(second), lapply(expected_second, function(clause) c(clause, padding_literals))))
  stopifnot(identical(padding_truth(padding_core(c(first)), core_grid), core_truth),
    identical(padding_truth(padding_core(c(second)), core_grid), core_truth))

  # Full positional truth tables for small padding, individual-padding and
  # all-padding controls at larger widths. The exact common-padding output
  # checks plus the 16-row core equality prove all remaining assignments too.
  if (padding <= 7L) {
    grid_domains = c(domains, setNames(rep(list(c("off", "on")), padding), padding_names))
    grid = expand.grid(grid_domains, stringsAsFactors = FALSE)
    truth_mode = "full"
  } else {
    patterns = matrix("off", nrow = padding + 2L, ncol = padding,
      dimnames = list(NULL, padding_names))
    for (i in seq_len(padding)) patterns[i + 1L, i] = "on"
    patterns[padding + 2L, ] = "on"
    grid = core_grid[rep(seq_len(nrow(core_grid)), times = nrow(patterns)), , drop = FALSE]
    for (i in seq_len(padding)) grid[[padding_names[[i]]]] = rep(patterns[, i], each = nrow(core_grid))
    truth_mode = "every_single_padding_plus_all_false_all_true"
  }
  before = padding_truth(padded, grid)
  stopifnot(identical(before, padding_truth(c(first), grid)), identical(before, padding_truth(c(second), grid)))

  run_traces = list()
  for (pass in 1:2) {
    entries = if (pass == 1L) padded else c(first)
    padding_trace = list()
    result = observed(entries, universe)
    expected = if (pass == 1L) first else second
    stopifnot(identical(result, expected))
    run_traces[[pass]] = padding_trace
  }
  if (padding == 1L) baseline = run_traces
  if (padding > 1L) stopifnot(identical(run_traces, baseline))
  traces[[as.character(padding)]] = run_traces
  records[[length(records) + 1L]] = list(padding = padding, initial_width = padding + 2L,
    first_widths = lengths(c(first)), second_widths = lengths(c(second)),
    first_core = padding_core(c(first)), second_core = padding_core(c(second)),
    first_leftover = padding_sse1(c(first)), second_leftovers = length(padding_sse1(c(second))),
    evaluated_assignments = nrow(grid), truth_mode = truth_mode, core_models = sum(core_truth),
    exact_model_count = paste0("16 * 2^", padding, " - 11"),
    first_trace_events = length(run_traces[[1L]]), second_trace_events = length(run_traces[[2L]]),
    register_unit_entries = sum(vapply(unlist(run_traces, recursive = FALSE), function(event) {
      identical(event$owner, "register_unit") && identical(event$kind, "enter")
    }, FALSE)),
    all_truth_preserved = TRUE, productive_second_pass = TRUE)
  cat("padding", padding, "width", padding + 2L, "truth rows", nrow(grid),
    "core trace lengths", lengths(run_traces), "\n")
}
summary = list(runtime = as.character(getRversion()), checkmate = as.character(packageVersion("checkmate")),
  configurations = length(records), largest_initial_width = max(vapply(records, function(r) r$initial_width, 1L)),
  assignment_rows = sum(vapply(records, function(r) r$evaluated_assignments, 1L)),
  positive_padding_traces_equal = TRUE, every_second_pass_productive = TRUE,
  common_padding_factored_truth_preserved = TRUE, all_public_outputs_match_observer = TRUE)
suffix = if (getRversion() < "4.0") "r36" else "r46"
write_json(list(summary = summary, records = records), file.path(audit_dir, paste0("results_", suffix, ".json")),
  pretty = TRUE, auto_unbox = TRUE)
saveRDS(list(summary = summary, records = records, traces = traces, core = core, domains = domains),
  file.path(audit_dir, paste0("checks_", suffix, ".rds")), version = 2)
cat(toJSON(summary, pretty = TRUE, auto_unbox = TRUE), "\n")
