# Independent dynamic-frame audit; base R only, run from the repository root.
# Unlike the root's observer, this reads actual R frames and maintains no
# synthetic enter/leave stack. It never writes a production source file.
options(warn = 2)
out_dir = "attic/cnf_verify3/pass_bound_check"
sha = strsplit(system2("sha256sum", "R/CnfFormula_simplify.R", stdout = TRUE), " +")[[1L]][[1L]]
stopifnot(sha == "7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc")
source_env = new.env(parent = baseenv())
sys.source("R/CnfFormula_simplify.R", envir = source_env)
plain_kernel = source_env$simplify_cnf

# Manually audited conservative helper adjacency, including the deferred edge.
edges = list(
  char_intersect = "char_union", char_setdiff = character(),
  char_union = character(), return_entries = character(),
  register_unit = "apply_domain_restriction",
  apply_domain_restriction = c("char_intersect", "eliminate_clause_update_sr",
    "eliminate_symbol_from_clause", "on_updated_subset_relations", "on_update_range"),
  eliminate_symbol_from_clause = c("register_unit", "on_updated_subset_relations"),
  on_updated_subset_relations = c("handle_sse_2nd_order_twoend", "eliminate_clause_update_sr",
    "apply_domain_restriction", "handle_sse_2nd_order_oneend"),
  on_update_range = c("handle_sse_2nd_order_oneend", "handle_sse_2nd_order_twoend"),
  handle_sse_2nd_order_oneend = "try_sse_2nd_order",
  handle_sse_2nd_order_twoend = "try_sse_2nd_order",
  try_sse_2nd_order = c("apply_domain_restriction", "char_union"),
  eliminate_clause_update_sr = character()
)
restriction = "apply_domain_restriction"
longest_path = function(node, earlier = character()) {
  stopifnot(!node %in% earlier)
  children = setdiff(edges[[node]], restriction)
  1L + if (length(children)) max(vapply(children, longest_path, 0L,
    earlier = c(earlier, node))) else 0L
}
stopifnot(max(vapply(setdiff(names(edges), restriction), longest_path, 0L)) == 5L)

original_body = body(plain_kernel)
definitions = which(vapply(as.list(original_body), function(expr) {
  is.call(expr) && identical(expr[[1L]], as.name("=")) &&
    is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))
}, FALSE))
found = vapply(definitions, function(i) as.character(original_body[[i]][[2L]]), "")
stopifnot(length(found) == 13L, setequal(found, names(edges)))
marked_body = original_body
for (i in definitions) {
  name = as.character(original_body[[i]][[2L]])
  old = original_body[[i]][[3L]][[3L]]
  marked_body[[i]][[3L]][[3L]] = substitute({
    .independent_depth_observer(NAME, environment())
    OLD
  }, list(NAME = name, OLD = old))
}

range_at = function(clause, s) {
  i = match(s, names(clause))
  if (is.na(i)) character() else clause[[i]]
}
signature = function(s, clauses) {
  ranges = lapply(clauses, range_at, s = s)
  bits = c(lengths(ranges) > 0L, unlist(lapply(seq_along(ranges), function(i)
    vapply(setdiff(seq_along(ranges), i), function(j)
      all(!is.na(match(ranges[[i]], ranges[[j]]))), FALSE)), use.names = FALSE))
  paste(as.integer(bits), collapse = "")
}
context = new.env(parent = emptyenv())
all_edges = integer()
helper_counts = setNames(integer(length(edges)), names(edges))
case_records = list()

fiber_weight = function(entries, eliminated, only_small = FALSE) {
  if (is.logical(entries)) return(0L)
  result = 0L
  for (clause in entries[!eliminated]) {
    symbols = if (only_small) intersect(names(clause), context$small) else names(clause)
    for (s in symbols) result = result + length(unique(context$fiber_ids[[s]][clause[[s]]]))
  }
  result
}
live_potentials = function(state) {
  list(W = fiber_weight(state$entries, state$eliminated),
    Phi = sum(!state$eliminated) + fiber_weight(state$entries, state$eliminated, TRUE))
}

.independent_depth_observer = function(name, frame) {
  # Identify precisely the instrumented source helpers by the marker which
  # begins their body. sys.function/sys.frame expose actual dynamic frames,
  # including helpers called while promises are being forced.
  indices = seq_len(sys.nframe())
  marked = vapply(indices, function(i) {
    fn = sys.function(i)
    b = if (is.function(fn) && !is.primitive(fn)) body(fn) else NULL
    if (is.call(b) && identical(b[[1L]], as.name("{")) && length(b) >= 2L &&
        is.call(b[[2L]]) && identical(b[[2L]][[1L]], as.name(".independent_depth_observer"))) {
      as.character(b[[2L]][[2L]])
    } else ""
  }, "")
  selected = indices[nzchar(marked)]
  names_now = marked[nzchar(marked)]
  stopifnot(tail(names_now, 1L) == name, identical(sys.frame(tail(selected, 1L)), frame))
  helper_counts[[name]] <<- helper_counts[[name]] + 1L
  context$observations = context$observations + 1L
  if (length(names_now) >= 2L) {
    parent = names_now[[length(names_now) - 1L]]
    stopifnot(name %in% edges[[parent]])
    edge = paste(parent, name, sep = " -> ")
    if (!edge %in% names(all_edges)) all_edges[[edge]] <<- 0L
    all_edges[[edge]] <<- all_edges[[edge]] + 1L
  }
  active = selected[names_now == restriction]
  if (name == restriction) {
    state = parent.env(frame)
    # Progress is charged to live storage; a write to an already eliminated
    # slot would not pay for a descendant through this potential.
    stopifnot(!state$eliminated[[frame$clause_idx]])
    birth = live_potentials(state)
    if (length(active) >= 2L) {
      parent_birth = get(".independent_birth_potentials", sys.frame(active[[length(active) - 1L]]),
        inherits = FALSE)
      stopifnot(birth$W < parent_birth$W, birth$Phi < parent_birth$Phi)
      context$nested = context$nested + 1L
    }
    assign(".independent_birth_potentials", birth, envir = frame)
  }
  segments = rle(names_now != restriction)
  max_segment = if (any(segments$values)) max(segments$lengths[segments$values]) else 0L
  stopifnot(max_segment <= 5L,
    length(active) <= context$W0 + 1L,
    length(active) <= context$Phi0 + 1L,
    length(names_now) <= 6L * length(active) + 5L,
    length(names_now) <= 6L * context$W0 + 11L,
    length(names_now) <= 6L * context$Phi0 + 11L)
  if (length(names_now) > context$max_helpers) {
    context$max_helpers = length(names_now)
    context$deepest = names_now
  }
  context$max_apply = max(context$max_apply, length(active))
  context$max_segment = max(context$max_segment, max_segment)
  invisible(NULL)
}
observed_env = new.env(parent = source_env)
observed_env$.independent_depth_observer = .independent_depth_observer
observed_kernel = plain_kernel
body(observed_kernel) = marked_body
environment(observed_kernel) = observed_env

run_case = function(label, domains, clauses) {
  context$small = character()
  context$fiber_ids = list()
  if (!is.logical(clauses)) {
    occurring = unique(unlist(lapply(clauses, names), use.names = FALSE))
    groups = split(occurring, vapply(occurring, signature, "", clauses = clauses))
    context$small = unlist(groups[lengths(groups) <= 2L], use.names = FALSE)
    for (s in occurring) context$fiber_ids[[s]] = setNames(vapply(domains[[s]], function(value)
      paste(as.integer(vapply(clauses, function(clause) value %in% range_at(clause, s), FALSE)),
        collapse = ""), ""), domains[[s]])
  }
  context$W0 = fiber_weight(clauses, if (is.logical(clauses)) logical() else logical(length(clauses)))
  context$Phi0 = if (is.logical(clauses)) 0L else length(clauses) +
    fiber_weight(clauses, logical(length(clauses)), TRUE)
  context$observations = context$nested = context$max_helpers = context$max_apply = context$max_segment = 0L
  context$deepest = character()
  universe = list2env(domains, parent = emptyenv())
  ordinary = plain_kernel(clauses, universe)
  checked = observed_kernel(clauses, universe)
  stopifnot(identical(ordinary, checked))
  record = list(label = label, W0 = context$W0, Phi0 = context$Phi0,
    observations = context$observations, nested = context$nested,
    max_helpers = context$max_helpers, max_apply = context$max_apply,
    max_segment = context$max_segment, deepest = context$deepest)
  case_records[[length(case_records) + 1L]] <<- record
  invisible(record)
}

binary = c("0", "1")
ternary = c("a", "b", "c")
run_case("TRUE", list(x = binary), TRUE)
run_case("FALSE", list(x = binary), FALSE)
run_case("unit_merge", list(x = ternary), list(list(x = c("a", "b")), list(x = c("b", "c"))))
run_case("unit_contradiction", list(x = ternary), list(list(x = "a"), list(x = "b")))
run_case("unit_noop", list(x = ternary, y = binary),
  list(list(x = c("a", "b")), list(x = "a", y = "0")))
run_case("unit_subsumption", list(x = ternary, y = binary),
  list(list(x = "a"), list(x = c("a", "b"), y = "0")))
run_case("unit_strict_nonempty", list(x = ternary, y = binary),
  list(list(x = c("a", "b")), list(x = c("b", "c"), y = "0")))
run_case("unit_literal_removal", list(x = ternary, y = binary),
  list(list(x = "a"), list(x = "b", y = "0")))
run_case("virtual_HLA_only", list(x = binary, y = binary, z = binary),
  list(list(x = "0", y = "0"), list(y = "0", z = "0")))
run_case("deferred_union", list(x = c(ternary, "d"), y = c(ternary, "d")),
  list(list(x = c("a", "b"), y = c("a", "b")),
    list(x = c("b", "c"), y = c("b", "c")),
    list(x = c("b", "d"), y = c("b", "d"))))

for (n in c(3L, 7L, 13L)) for (reverse in c(FALSE, TRUE)) for (guarded in c(FALSE, TRUE)) {
  domains = setNames(rep(list(binary), n), paste0("x", seq_len(n)))
  links = lapply(seq_len(n - 1L), function(i) setNames(list("0", "1"), paste0("x", c(i, i + 1L))))
  clauses = c(list(list(x1 = "1")), if (reverse) rev(links) else links)
  if (guarded) {
    domains$guard = binary
    clauses = lapply(clauses, function(clause) c(clause, list(guard = "0")))
  }
  run_case(paste("chain", n, reverse, guarded), domains, clauses)
}

# With a fixed eight-clause unit chain plus one separate clause, growing its
# frozen group and concrete value multiplicity leaves Phi0 unchanged.
n = 8L
baseline_phi = NULL
for (q in c(3L, 5L, 12L, 64L)) for (copies in c(1L, 7L)) {
  domains = setNames(rep(list(binary), n), paste0("x", seq_len(n)))
  links = lapply(seq_len(n - 1L), function(i) setNames(list("0", "1"), paste0("x", c(i, i + 1L))))
  clauses = c(list(list(x1 = "1")), rev(links))
  group = paste0("f", seq_len(q))
  for (s in group) domains[[s]] = c(paste0("v", seq_len(copies)), "unused")
  clauses[[length(clauses) + 1L]] = setNames(rep(list(paste0("v", seq_len(copies))), q), group)
  result = run_case(paste("fixed_m_frozen", q, copies), domains, clauses)
  if (is.null(baseline_phi)) baseline_phi = result$Phi0
  stopifnot(result$Phi0 == baseline_phi)
}

# Nested comparison candidate queues exercise nonempty range changes,
# callbacks, unit merging, and repeated second-order trials in one invocation.
fixture = readRDS("attic/cnf_verify3/root/queued_comparison_control.rds")$case
run_case("historical_nested_comparison", fixture$domains, fixture$clauses)

stopifnot(all(helper_counts > 0L),
  "char_intersect -> char_union" %in% names(all_edges),
  any(vapply(case_records, function(record) record$Phi0 < record$W0, FALSE)),
  any(vapply(case_records, function(record) record$Phi0 > record$W0, FALSE)))
summary = list(R = R.version.string, sha256 = sha, cases = length(case_records),
  helper_entries = sum(helper_counts), nested_apply_entries = sum(vapply(case_records, `[[`, 0L, "nested")),
  max_helpers = max(vapply(case_records, `[[`, 0L, "max_helpers")),
  max_apply = max(vapply(case_records, `[[`, 0L, "max_apply")),
  max_apply_free_segment = max(vapply(case_records, `[[`, 0L, "max_segment")),
  helper_counts = helper_counts, observed_edges = all_edges)
saveRDS(list(summary = summary, cases = case_records), file.path(out_dir, "recursion_checks.rds"))
dput(summary, file = file.path(out_dir, "recursion_summary.R"))
print(summary)
cat("All actual-frame, dynamic-edge, dual-potential and paired-output checks passed.\n")
