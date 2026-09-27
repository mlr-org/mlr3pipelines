# Focused dependency controls. Run from the repository root with base R only.
# All hooks observe a private source copy. No production expression is changed.
out_dir = "attic/cnf_verify3/proof_dependency_review"
source_file = "R/CnfFormula_simplify.R"
source_lines = readLines(source_file)
tag = if (getRversion() < "4") "r36" else "r46"
ordinary = new.env(parent = baseenv())
sys.source(source_file, ordinary)

# Independently extract the static helper graph, then include the inspected
# promise-forcing edge. This overapproximates syntactic and dynamic calls.
definition = body(ordinary$simplify_cnf)
helpers = list()
for (statement in as.list(definition)[-1L]) {
  if (is.call(statement) && identical(statement[[1L]], as.name("=")) &&
      is.call(statement[[3L]]) && identical(statement[[3L]][[1L]], as.name("function"))) {
    helpers[[as.character(statement[[2L]])]] = statement[[3L]][[3L]]
  }
}
stopifnot(length(helpers) == 13L)
calls_in = function(x) {
  if (!is.call(x) && !is.pairlist(x)) return(character())
  direct = if (is.call(x) && is.symbol(x[[1L]])) as.character(x[[1L]]) else character()
  unique(c(direct, unlist(lapply(as.list(x)[-1L], calls_in), use.names = FALSE)))
}
graph = lapply(helpers, function(x) intersect(calls_in(x), names(helpers)))
graph$char_intersect = union(graph$char_intersect, "char_union")
longest_path = function(graph) {
  visit = function(node, ancestors) {
    if (node %in% ancestors) stop("Cycle remains: ", paste(c(ancestors, node), collapse = " -> "))
    children = graph[[node]]
    if (!length(children)) return(node)
    paths = lapply(children, function(child) visit(child, c(ancestors, node)))
    c(node, paths[[which.max(lengths(paths))]])
  }
  paths = lapply(names(graph), function(node) visit(node, character()))
  paths[[which.max(lengths(paths))]]
}
canonical_graph = graph[names(graph) != "apply_domain_restriction"]
canonical_graph = lapply(canonical_graph, setdiff, "apply_domain_restriction")
canonical_longest = longest_path(canonical_graph)
stopifnot(length(canonical_longest) == 5L)
occurrence_graph = graph
# These two edges occur only after the impossible nonempty strict singleton
# intersection at line 167. Their removal needs singleton closure, not semantics.
occurrence_graph$apply_domain_restriction = setdiff(
  occurrence_graph$apply_domain_restriction,
  c("on_updated_subset_relations", "on_update_range"))
# Each removed edge follows the actual occurrence-deletion write at line 244.
occurrence_graph$eliminate_symbol_from_clause = setdiff(
  occurrence_graph$eliminate_symbol_from_clause,
  c("register_unit", "on_updated_subset_relations"))
occurrence_longest = longest_path(occurrence_graph)
stopifnot(length(occurrence_longest) <= 13L)

state = new.env(parent = emptyenv())
reset_state = function(label, occurrence, stop_at_nested = FALSE) {
  state$label = label
  state$occurrence = occurrence
  state$stop_at_nested = stop_at_nested
  state$restriction_entries = 0L
  state$nested_decreases = 0L
  state$pair_checks = 0L
  state$registry_skips = 0L
  state$raw_hla_counterexamples = list()
  state$lazy_counterexamples = list()
  state$unit_initializations = 0L
  state$prefix = NULL
}
stored_values = function(entries) sum(vapply(entries, function(clause) sum(lengths(clause)), numeric(1L)))
range_of = function(clause, symbol) {
  unique(unlist(clause[names(clause) == symbol], use.names = FALSE))
}
pdr_entry = function(frame) {
  kernel = parent.env(frame)
  q = stored_values(kernel$entries)
  frames = sys.frames()
  ancestors = Filter(function(e) !identical(e, frame) &&
    exists("pdr_entry_q", e, inherits = FALSE), frames)
  old_q = vapply(ancestors, function(e) get("pdr_entry_q", e, inherits = FALSE), numeric(1L))
  stopifnot(all(q < old_q))
  assign("pdr_entry_q", q, frame)
  state$restriction_entries = state$restriction_entries + 1L
  state$nested_decreases = state$nested_decreases + length(old_q)
  if (length(old_q) && state$stop_at_nested) {
    state$prefix = list(ancestor_stored_values = old_q, descendant_stored_values = q,
      entries = kernel$entries, eliminated = kernel$eliminated)
    stop(structure(list(message = "Intentional observer stop after a valid nested prefix", call = NULL),
      class = c("pdr_prefix_stop", "error", "condition")))
  }
}
pdr_pairs = function(frame) {
  kernel = parent.env(frame)
  counts = kernel$not_subset_count
  matrices = kernel$is_not_subset_of
  for (i in seq_len(nrow(counts))) {
    for (j in which(!is.na(counts[i, ]))) {
      stopifnot(i != j, identical(length(matrices[[i]][j, ]), ncol(matrices[[i]])),
        counts[i, j] == sum(matrices[[i]][j, ]))
      state$pair_checks = state$pair_checks + 1L
    }
  }
}
pdr_skip = function(frame) {
  if (!state$occurrence) return(invisible(NULL))
  kernel = parent.env(frame)
  i = frame$s_clause_idx_meta
  if (i > kernel$meta_idx_outer) return(invisible(NULL))
  skip = frame$inso_column[[i]] &&
    !kernel$is_not_subset_of[[i]][frame$unit_idx_meta, frame$nu]
  if (skip) {
    stopifnot(!frame$s_clause_idx %in% kernel$symbol_registry[[frame$nu]])
    state$registry_skips = state$registry_skips + 1L
  }
}
pdr_nonunit_hla = function(kernel) {
  donors = kernel$remaining_other_entries
  for (position in seq_along(donors)) {
    donor_idx = donors[[position]]
    donor_meta = kernel$available_inverse[[donor_idx]]
    row = kernel$is_not_subset_of[[donor_meta]][kernel$meta_idx, ]
    stopifnot(kernel$not_subset_count_current[[position]] == sum(row))
    for (symbol in unique(names(row))) {
      if (any(row[names(row) == symbol])) next
      donor_range = range_of(kernel$entries[[donor_idx]], symbol)
      stopifnot(all(donor_range %in% range_of(kernel$clause, symbol)))
      if (!all(donor_range %in% range_of(kernel$entries[[kernel$clause_idx]], symbol))) {
        state$raw_hla_counterexamples[[length(state$raw_hla_counterexamples) + 1L]] = list(
          target = kernel$clause_idx, donor = donor_idx, symbol = symbol, row = row,
          actual_target = kernel$entries[[kernel$clause_idx]], virtual_target = kernel$clause)
      }
    }
  }
}
pdr_unit_hla = function(kernel) {
  if (!state$occurrence) return(invisible(NULL))
  stopifnot(!length(kernel$symbol_registry[[kernel$unitsymbol]]),
    all(kernel$not_subset_count >= 2L),
    is.na(match(TRUE, kernel$not_subset_count == 1L & !kernel$was_used)))
  state$unit_initializations = state$unit_initializations + 1L
  for (i in seq_along(kernel$remaining_nonunit_entries)) {
    donor = kernel$entries[[kernel$remaining_nonunit_entries[[i]]]]
    row = structure(names(donor) != kernel$unitsymbol, names = names(donor))
    if (sum(row) != kernel$not_subset_count[[i]]) {
      state$lazy_counterexamples[[length(state$lazy_counterexamples) + 1L]] = list(
        unit = kernel$unitsymbol, donor = donor, hypothetical_row = row,
        physical_count = kernel$not_subset_count[[i]], selected = NA_integer_)
    }
  }
}
pdr_strict_range = function() {
  stopifnot(!state$occurrence)
}
hooks = list(
  "  apply_domain_restriction = function(clause_idx, symbol, restringent, is_unit_propagation) {" =
    "    pdr_entry(environment())",
  "  on_updated_subset_relations = function(meta_idx, meta_idx_other, second_order_only) {" =
    "    pdr_pairs(environment())",
  "        s_clause_idx_meta = available_inverse[[s_clause_idx]]" =
    "        pdr_skip(environment())",
  "    entries[[clause_idx]] <<- clause" = "    pdr_strict_range()"
)
observed_lines = source_lines
# Use source-coordinate placement for the three anchors shared by source text.
after_line = list("146" = hooks[[1L]], "295" = hooks[[2L]], "132" = hooks[[3L]],
  "167" = hooks[[4L]], "685" = "      pdr_nonunit_hla(environment())",
  "743" = "    pdr_unit_hla(environment())")
stopifnot(identical(source_lines[[146L]], names(hooks)[[1L]]),
  identical(source_lines[[295L]], names(hooks)[[2L]]),
  identical(source_lines[[132L]], names(hooks)[[3L]]),
  identical(source_lines[[167L]], names(hooks)[[4L]]),
  grepl("hla_clause_idx = match", source_lines[[686L]], fixed = TRUE),
  identical(source_lines[[744L]], "    repeat {"))
for (line in sort(as.integer(names(after_line)), decreasing = TRUE)) {
  observed_lines = append(observed_lines, after_line[[as.character(line)]], after = line)
}
observed = new.env(parent = globalenv())
eval(parse(text = observed_lines), observed)

from_words = function(words) lapply(words, function(word) {
  setNames(lapply(word, function(lit) if (lit > 0L) "1" else "0"), paste0("X", abs(word)))
})
chain = c(list(1L), rev(lapply(seq_len(7L), function(i) c(-i, i + 1L))))
cases = list(
  reversed_unit_chain = from_words(chain),
  raw_hla_canonical = from_words(list(c(1L, 2L), c(1L, 3L), c(-3L, 4L))),
  raw_hla_occurrences = from_words(list(c(1L, 1L, 2L), c(1L, 3L), c(-3L, 4L))),
  equal_name_sse = from_words(list(c(1L, 2L), c(1L, 1L, 3L), c(-1L, -1L, 3L))),
  unit_lazy_mismatch = from_words(list(c(-1L, 2L), c(3L, -2L), c(1L, 1L, 1L), c(-3L, -1L)))
)
truth = function(entries, universe) {
  rows = expand.grid(as.list(universe), stringsAsFactors = FALSE)
  entries = if (is.object(entries)) unclass(entries) else entries
  vapply(seq_len(nrow(rows)), function(i) {
    if (is.logical(entries)) return(as.vector(entries))
    all(vapply(entries, function(clause) any(vapply(seq_along(clause), function(j) {
      rows[[names(clause)[[j]]]][[i]] %in% clause[[j]]
    }, logical(1L))), logical(1L)))
  }, logical(1L))
}
results = list()
for (label in names(cases)) {
  entries = cases[[label]]
  universe = list2env(setNames(rep(list(c("0", "1")),
    length(unique(unlist(lapply(entries, names))))), unique(unlist(lapply(entries, names)))),
    parent = emptyenv())
  reset_state(label, occurrence = TRUE)
  expected = ordinary$simplify_cnf(entries, universe)
  actual = observed$simplify_cnf(entries, universe)
  stopifnot(identical(actual, expected), identical(truth(entries, universe), truth(actual, universe)))
  results[[label]] = as.list(state)
}
stopifnot(length(results$raw_hla_canonical$raw_hla_counterexamples) > 0L,
  length(results$raw_hla_occurrences$raw_hla_counterexamples) > 0L,
  length(results$unit_lazy_mismatch$lazy_counterexamples) > 0L,
  results$unit_lazy_mismatch$registry_skips > 0L,
  results$reversed_unit_chain$nested_decreases > 0L)

# An intentionally interrupted observer checks the same local descent before
# any assumption that this private invocation returns its result can be used.
reset_state("interrupted_prefix", occurrence = TRUE, stop_at_nested = TRUE)
entries = cases$reversed_unit_chain
universe = list2env(setNames(rep(list(c("0", "1")), 8L), paste0("X", 1:8)), parent = emptyenv())
stopped = tryCatch({ observed$simplify_cnf(entries, universe); FALSE }, pdr_prefix_stop = function(e) TRUE)
stopifnot(stopped, !is.null(state$prefix), state$nested_decreases > 0L)
results$interrupted_prefix = as.list(state)

# Scalar physical pivots do not imply distinct names or correct donor premises.
# This is an ordinary-R local control, not a reachable source counterexample.
row = c(X = TRUE, Y = TRUE)
two_names = c("X", "X")
stopifnot(sum(row) == 2L, sum(row[match(two_names, names(row))]) == 2L,
  isTRUE(row[["Y"]]), length(two_names[[1L]]) == 1L, length(two_names[[2L]]) == 1L)
results$repeated_match_countermodel = list(row = row, exceptional_names = two_names,
  repeated_positions = match(two_names, names(row)))
results$graphs = list(full = graph, canonical_longest = canonical_longest,
  occurrence_longest = occurrence_longest)
results$R = R.version.string
results$source_md5 = unname(tools::md5sum(source_file))
saveRDS(results, file.path(out_dir, paste0("controls_", tag, ".rds")))
summary = lapply(results[names(cases)], function(x) c(restriction_entries = x$restriction_entries,
  ancestor_decrease_checks = x$nested_decreases, physical_pair_checks = x$pair_checks,
  orphan_snapshot_skips = x$registry_skips, unit_hla_initializations = x$unit_initializations,
  raw_hla_counterexamples = length(x$raw_hla_counterexamples),
  hypothetical_lazy_counterexamples = length(x$lazy_counterexamples)))
print(list(R = R.version.string, controls = summary, interrupted_prefix = state$prefix[c(
  "ancestor_stored_values", "descendant_stored_values")], canonical_longest = canonical_longest,
  occurrence_longest = occurrence_longest, repeated_match_countermodel = results$repeated_match_countermodel))
