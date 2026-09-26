# Independent direct-kernel bridge and observational source checkpoints.
# No constructors, package helper, previous campaign observer, or production edit.
suppressMessages(library(jsonlite))
source("R/CnfFormula_simplify.R")
plain_simplify = simplify_cnf

observation = new.env(parent = emptyenv())
observe = function(label, frame) {
  if (!observation$enabled) return(invisible(NULL))
  owner = frame
  while (!exists("entries", owner, inherits = FALSE)) owner = parent.env(owner)
  get_local = function(name) {
    if (exists(name, frame, inherits = FALSE)) get(name, frame, inherits = FALSE) else NULL
  }
  get_owner = function(name) {
    if (exists(name, owner, inherits = FALSE)) get(name, owner, inherits = FALSE) else NULL
  }
  local_names = c("clause_idx", "symbol", "restringent", "is_unit_propagation", "meta_idx",
    "meta_idx_other", "other_meta_idx", "rows_to_check", "rows_changed", "meta_idx_target",
    "second_order_only", "potential_targets", "targets_while_oneend", "targets_while_twoend")
  state_names = c("entries", "eliminated", "is_unit", "available", "not_subset_count",
    "is_not_subset_of", "meta_idx_outer", "meta_idx_inner", "second_order_enabled",
    "second_order_enabled_matrix")
  state = setNames(lapply(state_names, get_owner), state_names)
  state$columns = lapply(state$is_not_subset_of, colnames)
  state$symbol_registry = as.list(get_owner("symbol_registry"))
  local = setNames(lapply(local_names, get_local), local_names)
  observation$events[[length(observation$events) + 1L]] = list(
    label = label, local = local, state = state,
    stack = vapply(sys.calls(), function(call) paste(deparse(call[[1L]]), collapse = " "), ""))
  invisible(NULL)
}

lines = readLines("R/CnfFormula_simplify.R", warn = FALSE)
checkpoints = c(`147` = "range_request", `194` = "source_begin", `207` = "source_row",
  `223` = "source_clear", `231` = "source_tail", `237` = "symbol_remove",
  `275` = "symbol_batch", `284` = "symbol_visit", `298` = "ordinary_enter",
  `348` = "notify_enter", `351` = "notify_targets", `354` = "notify_one",
  `361` = "notify_two", `408` = "twoend_enter", `453` = "sse2_try",
  `611` = "init_ready", `618` = "init_reverse", `628` = "enabled_boundary",
  `646` = "queue_visit", `650` = "hla_boundary")
annotated = unlist(lapply(seq_along(lines), function(i) {
  label = checkpoints[as.character(i)]
  c(if (!is.na(label)) sprintf('observe("%s", environment())', label), lines[[i]])
}), use.names = FALSE)
eval(parse(text = annotated))
observed_simplify = simplify_cnf

decode = function(x) {
  if (is.logical(x)) return(x)
  lapply(x, function(clause) lapply(clause, function(v) as.character(unlist(v, use.names = FALSE))))
}
encode = function(x) {
  attributes(x) = NULL
  x
}
stdin = file("stdin", open = "r")
repeat {
  line = readLines(stdin, n = 1L, warn = FALSE)
  if (!length(line)) break
  answer = tryCatch({
    request = fromJSON(line, simplifyVector = FALSE)
    if (isTRUE(request$version)) {
      list(ok = TRUE, version = R.version.string)
    } else {
      entries = decode(request$clauses)
      domains = lapply(request$domains, function(v) as.character(unlist(v, use.names = FALSE)))
      first = plain_simplify(entries, domains)
      second = plain_simplify(encode(first), domains)
      observation$enabled = isTRUE(request$trace)
      observation$events = list()
      if (observation$enabled) {
        observed = observed_simplify(entries, domains)
        stopifnot(identical(first, observed))
      }
      list(ok = TRUE, first = encode(first), second = encode(second), events = observation$events)
    }
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  cat(toJSON(answer, auto_unbox = TRUE, null = "null", na = "null", digits = NA), "\n", sep = "")
  flush(stdout())
}
