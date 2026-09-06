# Independent parsed-source prefix bridge. Only the JSON library is required.
# Prefix truncation and optional observations are private; production is unchanged.
suppressMessages(library(jsonlite))
source("R/CnfFormula_simplify.R")
unchanged_simplify = simplify_cnf
audit = new.env(parent = emptyenv())

get_if_present = function(name, frame) {
  if (exists(name, frame, inherits = FALSE)) get(name, frame, inherits = FALSE) else NULL
}

owner_of = function(frame) {
  while (!exists("universe", frame, inherits = FALSE)) frame = parent.env(frame)
  frame
}

capture_state = function(frame) {
  owner = owner_of(frame)
  plain_names = c("entries", "eliminated", "is_unit", "clause_idx", "clause_symbol_isct")
  state = setNames(lapply(plain_names, get_if_present, frame = owner), plain_names)
  for (name in c("unit_domains", "unit_registry", "symbol_registry")) {
    value = get_if_present(name, owner)
    state[[name]] = if (is.null(value)) list() else as.list(value)
  }
  state
}

observe = function(label, frame) {
  if (!audit$trace) return(invisible(NULL))
  local_names = c("unit_idx", "clause_idx", "symbol", "restringent", "nu", "unit")
  audit$events[[length(audit$events) + 1L]] = list(
    label = label,
    local = setNames(lapply(local_names, get_if_present, frame = frame), local_names),
    state = capture_state(frame),
    stack = vapply(sys.calls(), function(call) paste(deparse(call[[1L]]), collapse = " "), "")
  )
  invisible(NULL)
}

is_assignment = function(node, name) {
  is.call(node) && identical(node[[1L]], as.name("=")) && identical(node[[2L]], as.name(name))
}

parts = as.list(body(unchanged_simplify))
cut = which(vapply(parts, is_assignment, logical(1L), name = "available"))
stopifnot(length(cut) == 1L)
# Keep the original body through the nonunit preprocessing loop, then return.
prefix_body = as.call(c(parts[seq_len(cut - 1L)],
  list(quote(return(return_entries(entries[!eliminated]))))))

instrument = function(node) {
  if (!is.call(node)) return(node)
  if (is_assignment(node, "return_entries")) {
    original = node[[3L]][[3L]]
    node[[3L]][[3L]] = substitute({
      audit$final = capture_state(environment())
      BODY
    }, list(BODY = original))
    return(node)
  }
  for (name in c("register_unit", "apply_domain_restriction", "eliminate_symbol_from_clause",
    "eliminate_clause_update_sr")) {
    if (is_assignment(node, name)) {
      original = node[[3L]][[3L]]
      node[[3L]][[3L]] = substitute({
        observe(ENTER, environment())
        on.exit(observe(EXIT, environment()), add = TRUE)
        BODY
      }, list(ENTER = paste0(name, "_enter"), EXIT = paste0(name, "_exit"), BODY = original))
      return(node)
    }
  }
  if (is_assignment(node, "clause_symbol_isct")) {
    return(substitute({ BODY; observe("captured_symbols", environment()) }, list(BODY = node)))
  }
  for (i in seq_along(node)[-1L]) node[i] = list(instrument(node[[i]]))
  node
}

raw_prefix_simplify = unchanged_simplify
body(raw_prefix_simplify) = prefix_body
prefix_simplify = unchanged_simplify
body(prefix_simplify) = instrument(prefix_body)

decode_clauses = function(clauses) {
  if (is.logical(clauses)) return(clauses)
  lapply(clauses, function(clause) {
    lapply(clause, function(value) as.character(unlist(value, use.names = FALSE)))
  })
}

strip_attributes = function(value) {
  attributes(value) = NULL
  value
}

input = file("stdin", "r")
repeat {
  line = readLines(input, n = 1L, warn = FALSE)
  if (!length(line)) break
  answer = tryCatch({
    request = fromJSON(line, simplifyVector = FALSE)
    if (isTRUE(request$version)) {
      list(ok = TRUE, version = R.version.string,
        source_md5 = unname(tools::md5sum("R/CnfFormula_simplify.R")))
    } else {
      domains = lapply(request$domains, function(value) as.character(unlist(value, use.names = FALSE)))
      entries = decode_clauses(request$clauses)
      audit$events = list()
      audit$trace = isTRUE(request$trace)
      audit$final = NULL
      prefix = prefix_simplify(entries, domains)
      stopifnot(identical(prefix, raw_prefix_simplify(entries, domains)))
      full = if (isTRUE(request$full)) unchanged_simplify(entries, domains) else NULL
      list(ok = TRUE, prefix = strip_attributes(prefix), full = strip_attributes(full),
        final = audit$final, events = audit$events)
    }
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  cat(toJSON(answer, auto_unbox = TRUE, null = "null", digits = NA), "\n", sep = "")
  flush(stdout())
}
