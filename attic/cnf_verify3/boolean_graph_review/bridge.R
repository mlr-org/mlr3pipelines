suppressMessages(library(jsonlite))
source_text = paste(readLines("R/CnfFormula_simplify.R", warn = FALSE), collapse = "\n")
state = new.env(parent = emptyenv())
private = new.env(parent = .GlobalEnv)

private$.audit = function(kind, frame, index = NULL) {
  get_local = function(name) get(name, envir = frame, inherits = TRUE)
  entries = get_local("entries")
  eliminated = get_local("eliminated")
  original = get_local(".original_entries")
  live_binary = which(!eliminated & lengths(entries) == 2L)
  state$counts[["snapshots"]] = state$counts[["snapshots"]] + 1L
  for (i in live_binary) {
    stopifnot(identical(entries[[i]], original[[i]]))
    state$counts[["unchanged_binary_checks"]] = state$counts[["unchanged_binary_checks"]] + 1L
  }
  if (!state$hla) {
    comparisons = get_local("is_not_subset_of")
    if (!is.null(comparisons)) {
      available = get_local("available")
      inverse = get_local("available_inverse")
      counts = get_local("not_subset_count")
      for (i in live_binary) for (j in live_binary) {
        if (i == j || is.na(counts[inverse[[i]], inverse[[j]]])) next
        actual = vapply(names(entries[[i]]), function(s) {
          !all(entries[[i]][[s]] %in% entries[[j]][[s]])
        }, logical(1))
        stopifnot(identical(unname(comparisons[[inverse[[i]]]][inverse[[j]], ]), unname(actual)),
          counts[inverse[[i]], inverse[[j]]] == sum(actual))
        state$counts[["exact_live_pairs"]] = state$counts[["exact_live_pairs"]] + 1L
      }
    }
  }
  if (kind == "register_unit") {
    unit = entries[[index]]
    stopifnot(length(unit) == 1L, length(unit[[1L]]) == 1L,
      identical(unit[[1L]], original[[index]][[names(unit)]]))
    state$births[[length(state$births) + 1L]] = list(index = index, unit = unit)
  }
  if (kind == "before_hla") {
    state$boundary = entries[!eliminated]
    state$hla = TRUE
    units = entries[!eliminated & lengths(entries) == 1L]
    unit_symbols = unlist(lapply(units, names), use.names = FALSE)
    for (i in live_binary) stopifnot(!any(names(entries[[i]]) %in% unit_symbols))
    state$counts[["unit_symbol_absence_checks"]] = state$counts[["unit_symbol_absence_checks"]] + length(live_binary)
  }
  invisible(NULL)
}

replace_once = function(old, new) {
  locations = gregexpr(old, source_text, fixed = TRUE)[[1L]]
  stopifnot(length(locations) == 1L, locations[[1L]] > 0L)
  source_text <<- sub(old, new, source_text, fixed = TRUE)
}
replace_once("is_not_subset_of = NULL  # see further down",
  "is_not_subset_of = NULL  # see further down\n  .original_entries = entries")
helpers = c("register_unit", "apply_domain_restriction", "eliminate_symbol_from_clause",
  "on_updated_subset_relations", "on_update_range", "handle_sse_2nd_order_oneend",
  "handle_sse_2nd_order_twoend", "try_sse_2nd_order", "eliminate_clause_update_sr")
for (helper in helpers) {
  pattern = paste0("  ", helper, " = function(")
  lines = strsplit(source_text, "\n", fixed = TRUE)[[1L]]
  position = which(startsWith(lines, pattern))
  stopifnot(length(position) == 1L, endsWith(lines[[position]], "{"))
  extra = if (helper == "register_unit") ", unit_idx" else ""
  replace_once(lines[[position]], paste0(lines[[position]],
    "\n    .audit(\"", helper, "\", environment()", extra, ")"))
}
replace_once("  # Now for the big one: Asymmetric Hidden Literal Addition (Marijn et al.)",
  "  .audit(\"before_hla\", environment())\n  # Now for the big one: Asymmetric Hidden Literal Addition (Marijn et al.)")
eval(parse(text = source_text), envir = private)
source("R/CnfFormula_simplify.R")

connection = file("stdin", "r")
repeat {
  line = readLines(connection, n = 1L, warn = FALSE)
  if (!length(line)) break
  input = fromJSON(line, simplifyVector = FALSE)
  domains = lapply(input$domains, function(x) unlist(x, use.names = FALSE))
  universe = list2env(domains, parent = emptyenv())
  entries = if (is.logical(input$clauses)) input$clauses else {
    lapply(input$clauses, function(clause) lapply(clause, function(x) unlist(x, use.names = FALSE)))
  }
  state$counts = c(snapshots = 0L, unchanged_binary_checks = 0L, exact_live_pairs = 0L,
    unit_symbol_absence_checks = 0L)
  state$births = list()
  state$boundary = NULL
  state$hla = FALSE
  answer = tryCatch({
    result = private$simplify_cnf(entries, universe)
    ordinary = simplify_cnf(entries, universe)
    stopifnot(identical(c(result), c(ordinary)))
    list(result = c(result), births = state$births, boundary = state$boundary,
      counts = as.list(state$counts))
  }, error = function(error) list(error = conditionMessage(error)))
  cat(toJSON(answer, auto_unbox = TRUE, null = "null", force = TRUE), "\n", sep = "")
  flush(stdout())
}
