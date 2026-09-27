# Independent small controls and write trace, not a rerun of root's grid.
# Run from repository root with host Rscript or the existing R 4.6 launcher.
suppressPackageStartupMessages(library(checkmate))
suppressPackageStartupMessages(library(jsonlite))
out_dir = "attic/cnf_verify3/ctype_semantics_review"
runtime = if (getRversion() < "4") "r36" else "r46"
stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
map_chr = function(.x, .f, ...) vapply(.x, .f, character(1L), ...)
production = new.env(parent = globalenv())
sources = file.path("R", paste0(c("CnfUniverse", "CnfSymbol", "CnfAtom",
  "CnfClause", "CnfFormula", "CnfFormula_simplify"), ".R"))
for (path in sources) source(path, local = production)
plain = production$simplify_cnf

bare = function(x) {
  attr(x, "class") = NULL
  attr(x, "universe") = NULL
  x
}
codepoints = function(x) utf8ToInt(enc2utf8(x))
symbol_position = function(x, labels) {
  hits = vapply(labels, function(label) identical(codepoints(x), codepoints(label)), logical(1L))
  stopifnot(sum(hits) == 1L)
  which(hits)
}
evaluate = function(entries, labels, assignments) {
  if (is.logical(entries)) return(rep(as.vector(entries), nrow(assignments)))
  vapply(seq_len(nrow(assignments)), function(row) {
    valuation = assignments[row, ]
    for (clause in entries) {
      truth = FALSE
      for (j in seq_along(clause)) {
        pos = symbol_position(names(clause)[[j]], labels)
        if (any(vapply(clause[[j]], function(value) identical(value, valuation[[pos]]), logical(1L)))) {
          truth = TRUE
          break
        }
      }
      if (!truth) return(FALSE)
    }
    TRUE
  }, logical(1L))
}
plain_names = function(entries, labels) {
  if (is.logical(entries)) return(as.vector(entries))
  lapply(entries, function(clause) {
    names(clause) = paste0("s", vapply(names(clause), symbol_position, integer(1L), labels = labels))
    clause
  })
}
native_keys = function(labels) {
  vapply(labels, function(label) {
    e = new.env(parent = emptyenv())
    e[[label]] = TRUE
    names(e)[[1L]]
  }, character(1L), USE.NAMES = FALSE)
}

# The root candidate changes exactly this single expression in memory.
old_expression = quote(char_intersect(names(entries[[clause_idx]]), names(unit_domains)))
new_expression = quote(Filter(function(symbol) !is.null(unit_domains[[symbol]]), names(entries[[clause_idx]])))
replacements = 0L
replace_candidate = function(node) {
  if (identical(node, old_expression)) {
    replacements <<- replacements + 1L
    return(new_expression)
  }
  if (is.call(node)) for (i in seq_along(node)) {
    if (!identical(node[[i]], quote(expr = ))) node[i] = list(replace_candidate(node[[i]]))
  }
  node
}
candidate = plain
body(candidate) = replace_candidate(body(plain))
stopifnot(replacements == 1L, identical(production$simplify_cnf, plain))

cases = list(
  empty = list(),
  one_unit = list(list(`1` = "a")),
  one_wide = list(list(`1` = "b", `2` = "a")),
  two_units_overlap = list(list(`1` = c("a", "b")), list(`1` = "a")),
  two_units_disjoint = list(list(`1` = "a"), list(`1` = "b")),
  two_units_distinct = list(list(`1` = "a"), list(`2` = "a")),
  unit_wide_disjoint = list(list(`1` = "a"), list(`1` = "b", `2` = "a")),
  unit_wide_contains = list(list(`1` = "a"), list(`1` = c("a", "b"), `2` = "a")),
  unit_three_wide = list(list(`1` = "a"), list(`1` = "b", `2` = "a", `3` = "a")),
  two_wide_new_unit = list(list(`1` = "a", `2` = "a"), list(`1` = "a", `2` = "b")),
  two_wide_no_unit = list(list(`1` = "a", `2` = "a"), list(`1` = "b", `2` = "b")),
  two_units_one_wide = list(list(`1` = "a"), list(`2` = "a"), list(`1` = "b", `2` = "b")),
  three_unsatisfiable = list(list(`1` = "a"), list(`1` = "b", `2` = "a"), list(`1` = "c", `2` = "b")),
  three_satisfiable = list(list(`1` = "a"), list(`1` = "a", `2` = "a"), list(`1` = "b", `2` = "b"))
)
domains = list(c("a", "b", "c"), c("a", "b"), c("a", "b"))
assignments = expand.grid(domains, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
abstract_labels = as.character(seq_along(domains))
make_public = function(spec, labels) {
  u = production$CnfUniverse()
  symbols = lapply(seq_along(labels), function(i) production$CnfSymbol(u, labels[[i]], domains[[i]]))
  clauses = lapply(spec, function(clause) production$CnfClause(lapply(seq_along(clause), function(j) {
    production$CnfAtom(symbols[[as.integer(names(clause)[[j]])]], clause[[j]])
  })))
  list(universe = u, clauses = clauses, entries = lapply(clauses, bare))
}

old_ctype = Sys.getlocale("LC_CTYPE")
old_collate = Sys.getlocale("LC_COLLATE")
records = list()
for (ctype in c("C.UTF-8", "C")) {
  stopifnot(nzchar(Sys.setlocale("LC_CTYPE", ctype)), nzchar(Sys.setlocale("LC_COLLATE", "C")))
  label_sets = list(ascii = c("X", "Y", "Z"), utf8 = c("\u00e9", "Y", "Z"),
    latin1 = c(iconv("\u00e9", from = "UTF-8", to = "latin1"), "Y", "Z"),
    greek = c("\u03bb", "Y", "Z"), two_utf8 = c("\u00e9", "\u03bb", "Z"))
  for (kind in names(label_sets)) for (case_name in names(cases)) {
    labels = label_sets[[kind]]
    warnings = character()
    record = withCallingHandlers({
      keys = native_keys(labels)
      stopifnot(!anyDuplicated(keys), !anyDuplicated(labels))
      made = make_public(cases[[case_name]], labels)
      expected = evaluate(cases[[case_name]], abstract_labels, assignments)
      original = production$CnfFormula(made$clauses)
      corrected = candidate(made$entries, made$universe)
      actual = evaluate(bare(original), labels, assignments)
      corrected_truth = evaluate(bare(corrected), labels, assignments)
      stopifnot(identical(expected, corrected_truth))
      if (ctype != "C" || kind == "ascii") stopifnot(identical(bare(original), bare(corrected)))
      changed = !identical(expected, actual)
      expected_change = ctype == "C" && kind != "ascii" &&
        case_name %in% c("three_unsatisfiable", "three_satisfiable")
      stopifnot(identical(changed, expected_change))
      list(ctype = ctype, name_kind = kind, case = case_name, clauses = length(cases[[case_name]]),
        source_models = sum(expected), original_models = sum(actual), candidate_models = sum(corrected_truth),
        changed = changed, enumerated_membership = labels %in% names(made$universe),
        candidate_payload = plain_names(bare(corrected), labels))
    }, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    record$warning_count = length(warnings)
    records[[length(records) + 1L]] = record
  }
}
cat(R.version.string, "\nSmall directed cases:", length(records), "\n")
cat("Changed original cases:", sum(vapply(records, function(r) r$changed, logical(1L))), "\n")
cat("Every candidate result has the independently expected truth.\n")

# The trace observes every write to actual entries/eliminated, including writes
# in nested helper frames. It evaluates the original assignment in its original
# lexical frame; separate baseline comparison checks the returned payload.
events = list()
audit_labels = c("\u00e9", "Y")
audit_assignments = expand.grid(domains[1:2], KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
snapshot = function(frame) {
  entries = get("entries", frame)
  eliminated = if (exists("eliminated", frame, inherits = TRUE)) get("eliminated", frame) else rep(FALSE, length(entries))
  live = if (is.logical(entries)) entries else entries[!eliminated]
  list(entries = plain_names(entries, audit_labels), eliminated = eliminated,
    truth = evaluate(live, audit_labels, audit_assignments))
}
physical_counts = function(donors, target) {
  vapply(donors, function(donor) {
    sum(vapply(seq_along(donor), function(i) {
      name = names(donor)[[i]]
      any(!vapply(donor[[i]], function(value) any(vapply(target[[name]], identical,
        logical(1L), y = value)), logical(1L)))
    }, logical(1L)))
  }, integer(1L))
}
audit_write = function(expr, frame, context) {
  before = snapshot(frame)
  value = eval(expr, frame)
  after = snapshot(frame)
  events[[length(events) + 1L]] <<- list(kind = "actual_write", context = context,
    expression = paste(deparse(expr), collapse = " "), before = before, after = after,
    changed_truth = !identical(before$truth, after$truth))
  invisible(value)
}
audit_point = function(tag, frame) {
  extra = list()
  if (tag == "line502") {
    names_in_clause = names(get("entries", frame)[[get("clause_idx", frame)]])
    ud = get("unit_domains", frame)
    extra = list(clause = get("clause_idx", frame), selected = get("clause_symbol_isct", frame),
      enumerated = names(ud), direct_lookup = vapply(names_in_clause,
        function(s) !is.null(ud[[s]]), logical(1L), USE.NAMES = FALSE))
  } else if (tag == "line738") {
    extra = list(cached = get("not_subset_count", frame),
      physical = physical_counts(get("entries", frame)[get("remaining_nonunit_entries", frame)], get("clause", frame)))
  } else if (tag == "line777_before") {
    virtual = get("clause", frame)
    virtual[[get("symbol", frame)]] = get("range_new", frame)
    extra = list(cached = get("not_subset_count", frame),
      virtual = plain_names(list(virtual), audit_labels)[[1L]],
      physical = physical_counts(get("entries", frame)[get("remaining_nonunit_entries", frame)], virtual))
  }
  events[[length(events) + 1L]] <<- list(kind = "point", tag = tag, extra = extra, state = snapshot(frame))
  invisible(NULL)
}
root_lhs = function(node) {
  if (is.symbol(node)) return(as.character(node))
  if (is.call(node) && as.character(node[[1L]]) %in% c("[", "[[", "$")) return(root_lhs(node[[2L]]))
  ""
}
point_call = function(tag) as.call(list(as.name("audit_point"), tag, quote(environment())))
instrument = function(node, context = "main") {
  if (!is.call(node)) return(node)
  original = node
  if (identical(node[[1L]], as.name("if")) &&
      identical(node[[2L]], quote(not_subset_count[[updating_hla_clause_idx]] == 0))) context = "unit_hse_line777"
  for (i in seq_along(node)) {
    if (!identical(node[[i]], quote(expr = ))) node[i] = list(instrument(node[[i]], context))
  }
  op = as.character(original[[1L]])
  if (length(op) == 1L && op %in% c("=", "<-", "<<-") &&
      root_lhs(original[[2L]]) %in% c("entries", "eliminated")) {
    wrapped = as.call(list(as.name("audit_write"), as.call(list(as.name("quote"), node)), quote(environment()), context))
    if (context == "unit_hse_line777") return(as.call(list(as.name("{"), point_call("line777_before"), wrapped)))
    return(wrapped)
  }
  if (identical(original, quote({clause_symbol_isct = char_intersect(names(entries[[clause_idx]]), names(unit_domains))})[[2L]])) {
    return(as.call(list(as.name("{"), node, point_call("line502"))))
  }
  if (identical(original, quote({not_subset_count = lengths(entries[remaining_nonunit_entries]) - (remaining_nonunit_entries %in% symbol_registry[[unitsymbol]])})[[2L]])) {
    return(as.call(list(as.name("{"), node, point_call("line738"))))
  }
  node
}
observed = plain
body(observed) = instrument(body(plain))
stopifnot(nzchar(Sys.setlocale("LC_CTYPE", "C")))
withCallingHandlers({
  made = make_public(cases$three_unsatisfiable, audit_labels)
  baseline = production$CnfFormula(made$clauses)
  traced = observed(made$entries, made$universe)
  stopifnot(identical(bare(baseline), bare(traced)))
}, warning = function(w) invokeRestart("muffleWarning"))
changing = Filter(function(event) identical(event$changed_truth, TRUE), events)
stopifnot(length(changing) == 1L, changing[[1L]]$context == "unit_hse_line777",
  identical(changing[[1L]]$expression, "eliminated[[clause_idx]] = TRUE"),
  sum(changing[[1L]]$before$truth) == 0L, sum(changing[[1L]]$after$truth) == 2L)
early = Filter(function(event) identical(event$tag, "line502"), events)
initial_count = Filter(function(event) identical(event$tag, "line738"), events)
before_delete = Filter(function(event) identical(event$tag, "line777_before"), events)
stopifnot(length(early) == 2L, all(vapply(early, function(e) !length(e$extra$selected) &&
  identical(e$extra$direct_lookup, c(TRUE, FALSE)), logical(1L))),
  length(initial_count) == 1L, identical(initial_count[[1L]]$extra$cached, c(1L, 1L)),
  identical(initial_count[[1L]]$extra$physical, c(2L, 2L)),
  length(before_delete) == 1L, identical(before_delete[[1L]]$extra$cached, c(1L, 0L)),
  identical(before_delete[[1L]]$extra$physical, c(2L, 1L)))
for (event in events) {
  if (event$kind == "point") {
    cat("\n", event$tag, "\n", sep = "")
    print(event$extra)
  } else cat("write", event$context, event$expression, "models",
    sum(event$before$truth), "->", sum(event$after$truth), "\n")
}

# Deliberately outside scope: mixed encoding aliases still create duplicate
# ordinary symbol names before the candidate's changed line is reached.
alias_record = withCallingHandlers({
  utf8 = "\u00e9"
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  au = production$CnfUniverse()
  ax = production$CnfSymbol(au, utf8, c("a", "b", "c"))
  ay = production$CnfSymbol(au, latin1, c("a", "b", "c"))
  acl = production$CnfClause(list(production$CnfAtom(ax, "a"), production$CnfAtom(ay, "b")))
  corrected = candidate(list(bare(acl)), au)
  stopifnot(anyDuplicated(names(acl)) > 0L, anyDuplicated(names(corrected[[1L]])) > 0L)
  list(universe_bindings = length(au), incoming_clause_width = length(acl),
    duplicate_input_names = anyDuplicated(names(acl)) > 0L,
    duplicate_candidate_names = anyDuplicated(names(corrected[[1L]])) > 0L)
}, warning = function(w) invokeRestart("muffleWarning"))
cat("\nExcluded alias construction remains malformed under candidate:\n")
print(alias_record)
invisible(Sys.setlocale("LC_CTYPE", old_ctype))
invisible(Sys.setlocale("LC_COLLATE", old_collate))
saveRDS(list(records = records, events = events, aliases = alias_record),
  file.path(out_dir, paste0("evidence_", runtime, ".rds")), version = 2L)
summary = list(R = R.version.string, checkmate = as.character(packageVersion("checkmate")),
  cases = length(records), candidate_replacements = replacements,
  changed_original = sum(vapply(records, function(r) r$changed, logical(1L))),
  original_two_or_fewer_preserved = all(vapply(records, function(r) r$clauses > 2L || !r$changed, logical(1L))),
  actual_truth_changing_writes = length(changing), first_truth_changing_context = changing[[1L]]$context,
  alias_control = alias_record, source_md5 = as.list(tools::md5sum(sources)))
write_json(summary, file.path(out_dir, paste0("results_", runtime, ".json")), pretty = TRUE, auto_unbox = TRUE)
cat(toJSON(summary, pretty = TRUE, auto_unbox = TRUE), "\n")
