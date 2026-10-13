# Fresh small controls for the positive-padding source coupling.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}
plain = simplify_cnf
core = list(list(Y = c("0", "5"), X = c("0", "5")),
  list(X = "1", Y = c("0", "1", "4")),
  list(X = "0", Y = c("4", "0", "5")),
  list(Y = "0", X = c("4", "5")))
expected_first = list(list(Y = "0", X = "5"), list(X = "0", Y = c("4", "0")))
expected_second = list(list(Y = "0", X = "5"), list(Y = c("4", "0")))
pad_names = character()
events = list()
strip = function(entries) lapply(entries, function(clause) clause[!names(clause) %in% pad_names])

audit = function(label, frame) {
  owner = frame
  while (!exists("entries", owner, inherits = FALSE)) owner = parent.env(owner)
  optional = function(name) if (exists(name, owner, inherits = FALSE)) get(name, owner, inherits = FALSE) else NULL
  entries = get("entries", owner, inherits = FALSE)
  eliminated = optional("eliminated")
  matrices = optional("is_not_subset_of")
  counts = optional("not_subset_count")
  if (length(pad_names)) {
    stopifnot(all(lengths(strip(entries)[!eliminated]) >= 1L), !any(optional("is_unit")))
    for (clause in entries) {
      stopifnot(identical(names(clause)[names(clause) %in% pad_names], pad_names))
      for (pad in pad_names) stopifnot(identical(clause[[pad]], "on"))
    }
    for (i in seq_along(matrices)) {
      if (is.null(matrices[[i]])) next
      initialized = which(!is.na(counts[i, ]))
      stopifnot(!any(matrices[[i]][initialized, pad_names, drop = FALSE]))
      for (pad in pad_names) stopifnot(identical(matrices[[i]][, pad], matrices[[i]][, pad_names[[1L]]]))
    }
  }
  registry = optional("symbol_registry")
  if (length(pad_names) && !is.null(registry)) {
    for (pad in pad_names) stopifnot(identical(registry[[pad]], registry[[pad_names[[1L]]]]))
  }
  virtual = NULL
  if (label == "hla_expand") {
    symbol = get("symbol", frame)
    clause = get("clause", frame)
    stopifnot(symbol %in% c("X", "Y"))
    for (pad in pad_names) stopifnot(identical(clause[[pad]], "on"))
    virtual = list(target = get("clause_idx", frame), donor = get("clause_idx_other", frame),
      symbol = symbol, clause = strip(list(clause))[[1L]], new_range = get("range_new", frame),
      counts = get("not_subset_count_current", frame), used = get("was_used", frame))
  }
  default_pad_true = length(pad_names) > 0L && any(vapply(matrices, function(matrix) {
    !is.null(matrix) && any(matrix[, pad_names[[1L]]])
  }, FALSE))
  events[[length(events) + 1L]] <<- list(label = label, entries = strip(entries), eliminated = eliminated,
    is_unit = optional("is_unit"), available = optional("available"), inverse = optional("available_inverse"),
    counts = counts, matrices = lapply(matrices, function(matrix) {
      if (is.null(matrix)) NULL else matrix[, !colnames(matrix) %in% pad_names, drop = FALSE]
    }), enabled = optional("second_order_enabled"), queue = optional("sse_to_trigger"),
    core_registry = if (is.null(registry)) NULL else list(X = registry[["X"]], Y = registry[["Y"]]),
    pad_registry = if (!length(pad_names) || is.null(registry)) NULL else registry[[pad_names[[1L]]]],
    default_pad_true = default_pad_true, virtual = virtual)
  invisible(NULL)
}

source_lines = readLines("R/CnfFormula_simplify.R", warn = FALSE)
stopifnot(identical(unname(tools::md5sum("R/CnfFormula_simplify.R")), "376aac6eb81334e751e9351f9eddc02f"))
points = c(`52` = "initial", `88` = "unit", `169` = "range_write", `245` = "symbol_write",
  `475` = "clause_delete", `570` = "matrix_ready", `611` = "pair_ready", `650` = "hla_entry", `694` = "hla_expand")
instrumented = unlist(lapply(seq_along(source_lines), function(i) {
  label = points[as.character(i)]
  c(if (!is.na(label)) sprintf('audit("%s", environment())', label), source_lines[[i]])
}), use.names = FALSE)
eval(parse(text = instrumented))
observed = simplify_cnf
simplify_cnf = plain

scalar_truth = function(entries, assignments) {
  vapply(seq_len(nrow(assignments)), function(i) {
    all(vapply(entries, function(clause) {
      any(vapply(names(clause), function(symbol) assignments[[symbol]][[i]] %in% clause[[symbol]], FALSE))
    }, FALSE))
  }, FALSE)
}

records = list()
baseline = NULL
for (configuration in list(list(k = 1L, reverse = FALSE), list(k = 4L, reverse = FALSE),
    list(k = 11L, reverse = FALSE), list(k = 11L, reverse = TRUE), list(k = 0L, reverse = FALSE))) {
  k = configuration$k
  pad_names = if (k) paste0("P", seq_len(k)) else character()
  if (configuration$reverse) pad_names = rev(pad_names)
  pads = setNames(rep(list("on"), k), pad_names)
  u = CnfUniverse()
  for (symbol in c("X", "Y")) CnfSymbol(u, symbol, c("0", "1", "4", "5"))
  for (pad in pad_names) CnfSymbol(u, pad, c("off", "on"))
  input = lapply(core, function(clause) c(clause, pads))
  public = lapply(input, function(clause) CnfClause(lapply(names(clause), function(symbol) {
    CnfAtom(`$.CnfUniverse`(u, symbol), clause[[symbol]])
  })))
  first = CnfFormula(public)
  second = CnfFormula(as.list(first))
  stopifnot(identical(c(first), lapply(expected_first, function(clause) c(clause, pads))),
    identical(c(second), lapply(expected_second, function(clause) c(clause, pads))))
  stopifnot(all(c(first)[[1L]]$Y %in% c(first)[[2L]]$Y),
    !any(c(first)[[2L]]$X %in% c(first)[[1L]]$X))
  two_traces = list()
  for (pass in 1:2) {
    events = list()
    answer = observed(if (pass == 1L) input else c(first), u)
    stopifnot(identical(answer, if (pass == 1L) first else second))
    two_traces[[pass]] = events
  }
  if (k == 1L) baseline = two_traces
  if (k > 1L) stopifnot(identical(two_traces, baseline))
  flat_events = unlist(two_traces, recursive = FALSE)
  unit_calls = sum(vapply(flat_events, function(event) event$label == "unit", FALSE))
  stopifnot(unit_calls == if (k) 0L else 2L)
  if (k) stopifnot(any(vapply(flat_events, function(event) event$default_pad_true, FALSE)))
  domains = c(list(X = c("0", "1", "4", "5"), Y = c("0", "1", "4", "5")),
    setNames(rep(list(c("off", "on")), k), pad_names))
  grid = expand.grid(domains, stringsAsFactors = FALSE)
  truth = scalar_truth(input, grid)
  stopifnot(identical(truth, scalar_truth(c(first), grid)), identical(truth, scalar_truth(c(second), grid)),
    sum(truth) == 16 * 2^k - 11)
  records[[length(records) + 1L]] = list(k = k, reversed_pad_order = configuration$reverse,
    width = k + 2L, assignments = nrow(grid), models = sum(truth), unit_calls = unit_calls,
    trace_lengths = lengths(two_traces), traces = two_traces)
}
suffix = if (getRversion() < "4.0") "r36" else "r46"
report = list(runtime = R.version.string, configurations = length(records),
  assignment_rows = sum(vapply(records, function(record) record$assignments, 1L)), records = records)
output = file.path("attic", "cnf_verify3", "wide_four_review", paste0("results_", suffix))
saveRDS(report, paste0(output, ".rds"), version = 2)
write_json(report, paste0(output, ".json"), pretty = TRUE, auto_unbox = TRUE, na = "null")
print(report[1:3])
