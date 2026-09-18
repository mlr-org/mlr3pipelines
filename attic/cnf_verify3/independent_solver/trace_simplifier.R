# Trace nested helper calls on a reduced JSON input without changing sources.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}
trace_state = new.env(parent = emptyenv())
trace_state$events = list()
trace_state$depth = 0L
trace_state$call = 0L
snapshot = function(env) {
  take = function(name) if (exists(name, env, inherits = FALSE)) get(name, env, inherits = FALSE) else NULL
  list(entries = take("entries"), eliminated = take("eliminated"), is_unit = take("is_unit"),
    unit_registry = as.list(take("unit_registry")), unit_domains = as.list(take("unit_domains")),
    symbol_registry = as.list(take("symbol_registry")), available = take("available"),
    not_subset_count = take("not_subset_count"), is_not_subset_of = take("is_not_subset_of"),
    meta_idx_outer = take("meta_idx_outer"))
}
traced_helper = function(original, name) {
  force(original)
  force(name)
  function(...) {
    trace_state$call = trace_state$call + 1L
    id = trace_state$call
    trace_state$depth = trace_state$depth + 1L
    trace_state$events[[length(trace_state$events) + 1L]] = list(
      edge = "enter", id = id, name = name, depth = trace_state$depth,
      expression = paste(deparse(substitute(list(...))), collapse = " "), state = snapshot(environment(original)))
    answer = original(...)
    trace_state$events[[length(trace_state$events) + 1L]] = list(
      edge = "leave", id = id, name = name, depth = trace_state$depth,
      answer = answer, state = snapshot(environment(original)))
    trace_state$depth = trace_state$depth - 1L
    answer
  }
}
src = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
marker = "  # process units:\n"
stopifnot(length(gregexpr(marker, src, fixed = TRUE)[[1L]]) == 1L)
insertion = paste0(
  "  for (helper_name in c('register_unit', 'apply_domain_restriction', 'eliminate_symbol_from_clause', ",
  "'on_updated_subset_relations', 'on_update_range', 'handle_sse_2nd_order_oneend', ",
  "'handle_sse_2nd_order_twoend', 'try_sse_2nd_order', 'eliminate_clause_update_sr')) {\n",
  "    assign(helper_name, traced_helper(get(helper_name), helper_name))\n",
  "  }\n", marker)
src = sub(marker, insertion, src, fixed = TRUE)
eval(parse(text = src))
args = commandArgs(trailingOnly = TRUE)
request = fromJSON(args[[1L]], simplifyVector = FALSE)
domains = lapply(request$domains, function(d) as.character(unlist(d, use.names = FALSE)))
universe = CnfUniverse()
for (symbol in names(domains)) CnfSymbol(universe, symbol, domains[[symbol]])
clauses = lapply(request$clauses, function(c) lapply(c, function(v) as.character(unlist(v, use.names = FALSE))))
objects = lapply(clauses, function(c) CnfClause(lapply(names(c), function(s) CnfAtom(structure(s, universe = universe, class = "CnfSymbol"), c[[s]]))))
result = CnfFormula(objects)
cat(toJSON(list(result = c(result), events = trace_state$events), auto_unbox = TRUE, null = "null", digits = NA), "\n", sep = "")
