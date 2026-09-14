source("attic/cnf_verify3/normalization_component_review/observer.R")
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(digest))
set.seed(904117L)
version = if (getRversion() < "4") "r36" else "r46"
totals = new.env(parent = emptyenv())
totals$normalization = c(cases = 0L, runs = 0L, exact_payloads = 0L, exact_traces = 0L,
  exact_hla = 0L, truth_rows = 0L, events = 0L, writes = 0L, pair_rows = 0L,
  unit_containment = 0L, nonunit_hla = 0L, unit_hla = 0L,
  repeated_virtual = 0L, named_virtual = 0L, cross_helpers = 0L,
  metadata_entries = 0L, cross_rows = 0L)
totals$components = c(cases = 0L, exhaustive_cases = 0L, directed_cases = 0L, random_cases = 0L,
  global_calls = 0L, isolated_calls = 0L, exact_payloads = 0L, exact_local_histories = 0L,
  false_prefixes = 0L, truth_rows = 0L, false_cases = 0L, fixed_points = 0L,
  maximum_productive_checks = 0L, multi_productive = 0L,
  events = 0L, writes = 0L, pair_rows = 0L, unit_containment = 0L,
  nonunit_hla = 0L, unit_hla = 0L, repeated_virtual = 0L, named_virtual = 0L,
  cross_helpers = 0L, metadata_entries = 0L, cross_rows = 0L)
totals$controls = list()
digests = list(normalization = list(), components = list())
records = list(normalization = list(), components = list())

add_counts = function(kind, delta) {
  counter = totals[[kind]]
  counter[names(delta)] = counter[names(delta)] + delta
  totals[[kind]] = counter
}

semantic_check = function(input, output, domains) {
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  stopifnot(nrow(assignments) <= 20000L)
  for (i in seq_len(nrow(assignments))) {
    assignment = as.list(assignments[i, , drop = FALSE])
    stopifnot(identical(truth(input, assignment), truth(output, assignment)))
  }
  nrow(assignments)
}

storage_variants = function(domains) {
  repeated = lapply(domains, function(x) rev(rep(x, length.out = 6L * length(x)))[
    rep(seq_len(6L * length(x)), rep(1:3, length.out = 6L * length(x)))])
  named = lapply(repeated, function(x) setNames(x, rep(c("", NA_character_, "same"), length.out = length(x))))
  matrices = lapply(repeated, function(x) matrix(x, nrow = 2L,
    dimnames = list(c("row", "row"), rep("col", length(x) %/% 2L))))
  arrays = lapply(repeated, function(x) array(x, c(2L, 1L, length(x) %/% 2L),
    dimnames = list(c("one", "two"), "", rep(NA_character_, length(x) %/% 2L))))
  metadata = lapply(named, function(x) structure(x, comment = "inert ordinary storage",
    review_metadata = list(a = 7L), class = "ReviewUnusedCharacterMarker"))
  list(canonical = domains, reverse = lapply(domains, rev), repeated = repeated,
    named = named, matrix = matrices, array = arrays, metadata = metadata)
}

read_fixture = function(path) {
  x = fromJSON(path, simplifyVector = FALSE)
  list(label = basename(path), domains = lapply(x$domains, unlist, use.names = FALSE),
    clauses = lapply(x$clauses, function(clause) lapply(clause, unlist, use.names = FALSE)))
}

fixture_paths = c("outside_skip_satisfiable_dense.json", "minimized_sse1.json",
  "minimized_first_order_phase_sse1.json", "minimized_sse2.json",
  "directed_oneend_shrink_min.json", "minimized_oneend_symbol_removal.json",
  "minimized_subsumption.json", "minimized_deferred_skip.json")
fixtures = lapply(file.path("attic/cnf_verify3/independent_solver", fixture_paths), read_fixture)

directed = list(
  list(label = "FALSE", domains = list(X = c("a", "b")), clauses = FALSE),
  list(label = "TRUE", domains = list(X = c("a", "b")), clauses = TRUE),
  list(label = "empty", domains = list(X = c("a", "b")), clauses = list()),
  list(label = "unit_merge", domains = list(X = c("a", "b", "c")),
    clauses = list(list(X = c("c", "b")), list(X = c("b", "a")))),
  list(label = "false_units", domains = list(X = c("a", "b", "c")),
    clauses = list(list(X = "c"), list(X = c("b", "a")))),
  list(label = "nonunit_hla", domains = list(X = c("a", "b", "c"), Y = c("a", "b", "c")),
    clauses = list(list(X = c("a", "b"), Y = c("a", "b")), list(X = c("b", "c"), Y = "a"))),
  list(label = "unit_hla", domains = list(X = c("a", "b", "c"), Y = c("a", "b", "c")),
    clauses = list(list(X = c("a", "b")), list(X = "a", Y = "a"), list(X = "b", Y = "b"))))

generate_case = function(index, symbols = sample.int(4L, 1L) + 1L) {
  domains = setNames(lapply(seq_len(symbols), function(i) c("", "b", "a", "c")[seq_len(sample.int(3L, 1L) + 1L)]),
    paste0("s", seq_len(symbols)))
  planted = vapply(domains, function(d) d[[sample.int(length(d), 1L)]], "")
  clauses = lapply(seq_len(sample.int(12L, 1L) + 1L), function(j) {
    width = if (index %% 3L == 0L) sample.int(symbols, 1L) else sample.int(symbols - 1L, 1L) + 1L
    selected = names(domains)[sample.int(symbols, width)]
    clause = setNames(lapply(selected, function(s) {
      d = domains[[s]]
      d[sample.int(length(d), sample.int(length(d) - 1L, 1L))]
    }), selected)
    if (index %% 2L == 0L && !truth(list(clause), as.list(planted))) {
      s = selected[[sample.int(width, 1L)]]
      clause[[s]] = planted[[s]]
    }
    clause
  })
  list(label = paste0("generated_", index), domains = domains, clauses = clauses)
}

random_n = as.integer(Sys.getenv("REVIEW_RANDOM", "120"))
normalization_cases = c(directed, fixtures, lapply(seq_len(random_n), generate_case))
for (ci in seq_along(normalization_cases)) {
  case = normalization_cases[[ci]]
  variants = storage_variants(case$domains)
  baseline = NULL
  for (kind in names(variants)) {
    result = run_observed(case$clauses, variants[[kind]])
    add_counts("normalization", c(runs = 1L, result$counts))
    if (is.null(baseline)) {
      baseline = result
      add_counts("normalization", c(truth_rows = semantic_check(case$clauses, result$output, case$domains)))
    } else {
      stopifnot(identical(result$output, baseline$output), identical(result$events, baseline$events),
        identical(result$hla, baseline$hla))
      add_counts("normalization", c(exact_payloads = 1L, exact_traces = 1L, exact_hla = length(result$hla)))
    }
  }
  add_counts("normalization", c(cases = 1L))
  records$normalization[[ci]] = list(case, baseline$output, baseline$events, baseline$hla)
  digests$normalization[[ci]] = digest(list(case, baseline$output, baseline$events, baseline$hla), algo = "sha256")
  if (ci %% 25L == 0L) cat("Normalization", ci, "inputs\n")
}

rename_component = function(case, group) {
  names(case$domains) = paste0(group, "_", names(case$domains))
  if (!is.logical(case$clauses)) case$clauses = lapply(case$clauses, function(clause) {
    names(clause) = paste0(group, "_", names(clause))
    clause
  })
  case
}

project = function(formula, symbols) {
  if (is.logical(formula)) return(formula)
  selected = logical(length(formula))
  for (i in seq_along(formula)) {
    belongs = names(formula[[i]]) %in% symbols
    stopifnot(!any(belongs) || all(belongs))
    selected[[i]] = any(belongs)
  }
  result = formula[selected]
  if (!length(result)) TRUE else result
}

mass = function(x) if (is.logical(x)) 0L else sum(vapply(x, function(c) sum(lengths(c)), 0L))
local_trace = function(result, group) {
  x = result$local[[group]]
  if (is.null(x)) list() else x
}

interleavings = function(a, b) {
  if (a == 0L) return(list(rep("B", b)))
  if (b == 0L) return(list(rep("A", a)))
  c(lapply(interleavings(a - 1L, b), function(x) c("A", x)),
    lapply(interleavings(a, b - 1L), function(x) c("B", x)))
}

check_components = function(components, order, kind) {
  group_names = names(components)
  domains = do.call(c, unname(lapply(components, `[[`, "domains")))
  # Group labels explicitly follow each clause through the supplied shuffle.
  group_positions = setNames(rep(0L, length(components)), group_names)
  whole = vector("list", length(order))
  for (i in seq_along(order)) {
    g = order[[i]]
    group_positions[[g]] = group_positions[[g]] + 1L
    whole[[i]] = components[[g]]$clauses[[group_positions[[g]]]]
  }
  local = lapply(components, `[[`, "clauses")
  original_local = local
  productive = setNames(rep(0L, length(components)), group_names)
  productive_whole = 0L
  trace_digests = list()
  pass_records = list()
  for (pass in seq_len(100L)) {
    if (is.logical(whole)) {
      owners = character()
      ids = integer()
    } else {
      owners = vapply(whole, function(clause) {
        candidates = group_names[vapply(components, function(c) names(clause)[[1L]] %in% names(c$domains), TRUE)]
        stopifnot(length(candidates) == 1L)
        candidates
      }, "")
      ids = ave(seq_along(owners), owners, FUN = seq_along)
      storage.mode(ids) = "integer"
    }
    global = run_observed(whole, domains, owners, ids)
    add_counts("components", c(global_calls = 1L, global$counts))
    isolated = setNames(lapply(group_names, function(g) {
      result = run_observed(local[[g]], domains,
        rep(g, length(local[[g]])), seq_along(local[[g]]))
      add_counts("components", c(isolated_calls = 1L, result$counts))
      result
    }), group_names)
    stopifnot(identical(global$output, FALSE) == any(vapply(isolated, function(x) identical(x$output, FALSE), TRUE)))
    trace_digests[[pass]] = digest(list(global$output, lapply(isolated, `[[`, "output"), global$local), algo = "sha256")
    pass_records[[pass]] = list(global$output, lapply(isolated, `[[`, "output"), global$local)
    if (identical(global$output, FALSE)) {
      for (g in group_names) {
        got = local_trace(global, g)
        expected = local_trace(isolated[[g]], g)
        stopifnot(length(got) <= length(expected), identical(got, expected[seq_along(got)]))
        add_counts("components", c(false_prefixes = 1L))
      }
      add_counts("components", c(false_cases = 1L))
      break
    }
    for (g in group_names) {
      payload_ok = identical(project(global$output, names(components[[g]]$domains)), isolated[[g]]$output)
      history_ok = identical(local_trace(global, g), local_trace(isolated[[g]], g))
      if (!payload_ok || !history_ok) {
        saveRDS(list(components = components, order = order, pass = pass, group = g,
          whole = whole, global = global, isolated = isolated, payload_ok = payload_ok, history_ok = history_ok),
          file.path(review_dir, paste0("mismatch_", version, ".rds")))
        stop("Component mismatch: payload=", payload_ok, "; history=", history_ok)
      }
      add_counts("components", c(exact_payloads = 1L, exact_local_histories = 1L))
      productive[[g]] = productive[[g]] + (mass(isolated[[g]]$output) < mass(local[[g]]))
    }
    did_change = mass(global$output) < mass(whole)
    productive_whole = productive_whole + did_change
    if (!did_change) {
      stopifnot(productive_whole == max(productive),
        identical(bare_output(review_plain(global$output, make_universe(domains))), global$output))
      add_counts("components", c(maximum_productive_checks = 1L, fixed_points = 1L,
        multi_productive = as.integer(productive_whole > 1L)))
      for (g in group_names) add_counts("components", c(truth_rows = semantic_check(
        original_local[[g]], isolated[[g]]$output, components[[g]]$domains)))
      break
    }
    whole = global$output
    local = lapply(isolated, `[[`, "output")
    stopifnot(pass < 100L)
  }
  add_counts("components", setNames(c(1L, 1L), c("cases", paste0(kind, "_cases"))))
  digests$components[[length(digests$components) + 1L]] <<- digest(trace_digests, algo = "sha256")
  records$components[[length(records$components) + 1L]] <<- pass_records
}

# Exhaust all ordered 0/1/2-clause full-width Boolean components, including
# duplicate clauses, and every order-preserving interleaving of every pair.
basic = list(domains = list(X = c("0", "1"), Y = c("0", "1")))
pool = list(list(X = "0", Y = "0"), list(X = "0", Y = "1"),
  list(X = "1", Y = "0"), list(X = "1", Y = "1"))
component_lists = c(list(list()), lapply(pool, function(x) list(x)),
  unlist(lapply(pool, function(x) lapply(pool, function(y) list(x, y))), recursive = FALSE))
for (ai in seq_along(component_lists)) {
  a = rename_component(c(basic, list(clauses = component_lists[[ai]])), "A")
  for (bi in seq_along(component_lists)) {
    b = rename_component(c(basic, list(clauses = component_lists[[bi]])), "B")
    for (order in interleavings(length(a$clauses), length(b$clauses))) check_components(list(A = a, B = b), order, "exhaustive")
  }
  cat("Exhaustive components", ai, "of", length(component_lists), "\n")
}

# Move unrelated work through every insertion boundary of the fixtures.
challengers = list(
  list(domains = list(X = c("a", "b", "c")), clauses = list(list(X = c("b", "a")))),
  list(domains = list(X = c("a", "b", "c"), Y = c("a", "b", "c")),
    clauses = list(list(X = "a", Y = "a"), list(X = "b", Y = "b"))),
  directed[[7L]], directed[[5L]])
for (fixture in fixtures) {
  a = rename_component(fixture, "A")
  for (challenger in challengers) {
    b = rename_component(challenger, "B")
    for (position in 0:length(a$clauses)) {
      order = c(rep("A", position), rep("B", length(b$clauses)), rep("A", length(a$clauses) - position))
      check_components(list(A = a, B = b), order, "directed")
    }
  }
}

for (i in seq_len(random_n * 2L)) {
  groups = LETTERS[seq_len(sample.int(2L, 1L) + 1L)]
  components = setNames(lapply(groups, function(g) rename_component(generate_case(i), g)), groups)
  order = rep(groups, vapply(components, function(c) length(c$clauses), 0L))
  order = order[sample.int(length(order))]
  check_components(components, order, "random")
  if (i %% 25L == 0L) cat("Generated components", i, "\n")
}

replace_ast = function(expr, target, replacement) {
  if (identical(expr, target)) return(replacement)
  if (is.call(expr)) for (i in seq_along(expr)[-1L]) {
    if (is.call(expr[[i]])) expr[[i]] = replace_ast(expr[[i]], target, replacement)
  }
  expr
}

# Output-preserving source branch with storage-sensitive behavior: a paired
# event observer must reject it even when an output-only test would pass.
branch_control = review_plain
body(branch_control) = replace_ast(body(branch_control), quote(x[!x %in% y]),
  quote({if (anyDuplicated(x) > 0L) invisible(NULL); x[!x %in% y]}))
control_case = directed[[6L]]
control_fun = instrument(branch_control)$fun
plain_control = run_observed(control_case$clauses, control_case$domains, fun = control_fun)
named_control = run_observed(control_case$clauses, storage_variants(control_case$domains)$named, fun = control_fun)
stopifnot(identical(plain_control$output, named_control$output), !identical(plain_control$events, named_control$events))
totals$controls$storage_branch_detected = TRUE

bad_complement = review_plain
body(bad_complement) = replace_ast(body(bad_complement), quote(x[!x %in% y]), quote(x[x %in% y]))
rejected = tryCatch({run_observed(control_case$clauses, control_case$domains,
  fun = instrument(bad_complement)$fun); FALSE}, error = conditionMessage)
stopifnot(is.character(rejected))
totals$controls$bad_complement_rejected = rejected

# The exact order comparator rejects unstable tie ordering controlled by
# unrelated clause count, while the ordinary truth-table oracle allows it.
bad_order = review_plain
body(bad_order) = replace_ast(body(bad_order), quote(entries[order(lengths(entries))]),
  quote(entries[order(lengths(entries), if (length(entries) > 2L) -seq_along(entries) else seq_along(entries))]))
a = rename_component(list(domains = list(X = c("a", "b", "c"), Y = c("a", "b", "c")),
  clauses = list(list(X = "a", Y = "a"), list(X = "b", Y = "b"))), "A")
b = rename_component(a, "B")
domains = c(a$domains, b$domains)
input = c(a$clauses, b$clauses)
bad_output = bare_output(bad_order(input, make_universe(domains)))
separate = bare_output(bad_order(a$clauses, make_universe(domains)))
stopifnot(!identical(project(bad_output, names(a$domains)), separate), semantic_check(input, bad_output, domains) == 81L)
totals$controls$unstable_order_detected = TRUE
totals$controls$unstable_order_truth_rows = 81L
stopifnot(!identical(truth(list(list(X = "a")), list(X = "b")), truth(list(), list(X = "b"))))
totals$controls$semantic_clause_deletion_detected = TRUE

result = list(R = R.version.string, source_sha256 = digest(file = "R/CnfFormula_simplify.R", algo = "sha256"),
  seed = 904117L, normalization = totals$normalization, components = totals$components,
  controls = totals$controls, sites = as.list(table(vapply(observed_source$sites, `[[`, "", "kind"))),
  fixture_paths = fixture_paths, digests = digests, records = records)
saveRDS(result, file.path(review_dir, paste0("results_", version, ".rds")))
json_result = result[!names(result) %in% c("digests", "records")]
json_result$normalization = as.list(json_result$normalization)
json_result$components = as.list(json_result$components)
write_json(json_result, file.path(review_dir, paste0("results_", version, ".json")),
  auto_unbox = TRUE, pretty = TRUE)
print(result[!names(result) %in% c("digests", "records")])
