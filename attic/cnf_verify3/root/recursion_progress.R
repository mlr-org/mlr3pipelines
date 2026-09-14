# Check a progress measure between nested local restriction calls.
suppressPackageStartupMessages({ library(checkmate); library(mlr3misc); library(jsonlite) })
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
plain_simplify = simplify_cnf
original_body = body(plain_simplify)
definitions = which(vapply(as.list(original_body), function(expr) {
  is.call(expr) && as.character(expr[[1L]]) %in% c("=", "<-") && length(expr) == 3L &&
    is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))
}, FALSE))
helper_names = vapply(definitions, function(index) as.character(original_body[[index]][[2L]]), "")
collect_calls = function(expr) {
  if (!is.call(expr)) return(character())
  head = if (is.symbol(expr[[1L]])) as.character(expr[[1L]]) else ""
  unique(c(if (head %in% helper_names) head else character(),
    unlist(lapply(as.list(expr)[-1L], collect_calls), use.names = FALSE)))
}
graph = setNames(lapply(definitions, function(index) collect_calls(original_body[[index]][[3L]][[3L]])), helper_names)
# The restricting union is a promise forced through char_intersect's y.
# Include this dynamic leaf edge, not only syntactic direct helper calls.
graph$char_intersect = union(graph$char_intersect, "char_union")
without_apply = setdiff(helper_names, "apply_domain_restriction")
paths = function(node, history = character()) {
  stopifnot(!node %in% history)
  children = intersect(graph[[node]], without_apply)
  if (!length(children)) return(list(c(history, node)))
  unlist(lapply(children, function(child) paths(child, c(history, node))), recursive = FALSE)
}
all_paths = unlist(lapply(without_apply, paths), recursive = FALSE)
longest = max(lengths(all_paths))
graph_result = list(helper_names = helper_names, edges = graph,
  deferred_argument_edge = c("char_intersect", "char_union"),
  excluded_for_acyclicity = "apply_domain_restriction", longest_acyclic_nodes = longest,
  longest_paths = all_paths[lengths(all_paths) == longest])
writeLines(toJSON(graph_result, auto_unbox = TRUE, pretty = TRUE),
  "attic/cnf_verify3/root/recursion_call_graph.json")

make_case = function(domains, clauses, label) list(domains = domains, clauses = clauses, label = label)
cases = list(readRDS("attic/cnf_verify3/root/queued_comparison_control.rds")$case)
cases[[1L]]$label = "nested comparison"
for (n in c(2L, 4L, 8L, 16L, 32L, 48L)) {
  domains = setNames(rep(list(c("0", "1")), n), paste0("X", seq_len(n)))
  edges = lapply(seq_len(n - 1L), function(i) setNames(list("0", "1"), paste0("X", c(i, i + 1L))))
  for (reverse in c(FALSE, TRUE)) {
    clauses = c(list(list(X1 = "1")), if (reverse) rev(edges) else edges)
    cases[[length(cases) + 1L]] = make_case(domains, clauses, paste("unit chain", n, reverse))
    clauses = lapply(clauses, function(clause) c(clause, list(G = "0")))
    cases[[length(cases) + 1L]] = make_case(c(domains, list(G = c("0", "1"))), clauses,
      paste("guarded chain", n, reverse))
  }
}
set.seed(9061450L)
for (trial in seq_len(600L)) {
  ns = sample(2:7, 1L)
  domains = setNames(lapply(seq_len(ns), function(i) paste0("v", seq_len(sample(2:6, 1L)))), paste0("S", seq_len(ns)))
  planted = lapply(domains, function(domain) sample(domain, 1L))
  clauses = lapply(seq_len(sample(3:24, 1L)), function(ci) {
    support = sample(names(domains), sample.int(ns, 1L))
    ranges = setNames(lapply(support, function(symbol) {
      domain = domains[[symbol]]
      sample(domain, sample.int(length(domain) - 1L, 1L))
    }), support)
    if (trial %% 2L == 0L && !any(vapply(support, function(symbol) planted[[symbol]] %in% ranges[[symbol]], FALSE))) {
      symbol = sample(support, 1L)
      ranges[[symbol]][[1L]] = planted[[symbol]]
    }
    ranges
  })
  cases[[length(cases) + 1L]] = make_case(domains, clauses, paste0("random:", trial))
}

results = list()
for (case in cases) {
  universe = CnfUniverse()
  for (symbol in names(case$domains)) CnfSymbol(universe, symbol, case$domains[[symbol]])
  initial = case$clauses[order(lengths(case$clauses))]
  cells = lapply(names(case$domains), function(symbol) {
    domain = case$domains[[symbol]]
    setNames(vapply(domain, function(value) {
      paste(as.integer(vapply(initial, function(clause) value %in% clause[[symbol]], FALSE)), collapse = "")
    }, ""), domain)
  })
  names(cells) = names(case$domains)
  weight = function(entries, eliminated) {
    sum(vapply(entries[!eliminated], function(clause) {
      sum(vapply(names(clause), function(symbol) length(unique(cells[[symbol]][clause[[symbol]]])), 0L))
    }, 0L))
  }
  initial_weight = weight(initial, logical(length(initial)))
  stack = list()
  maxima = list(helper_depth = 0L, active_apply = 0L, nested_apply_checks = 0L)
  deepest = character()
  observe_environment = new.env(parent = globalenv())
  observe_environment$.enter = function(name, state) {
    potential = if (name == "apply_domain_restriction") weight(state$entries, state$eliminated) else NA_integer_
    active_apply = which(vapply(stack, function(frame) frame$name == "apply_domain_restriction", FALSE))
    if (name == "apply_domain_restriction" && length(active_apply)) {
      stopifnot(potential < stack[[tail(active_apply, 1L)]]$potential)
      maxima$nested_apply_checks <<- maxima$nested_apply_checks + 1L
    }
    stack[[length(stack) + 1L]] <<- list(name = name, potential = potential)
    if (length(stack) > maxima$helper_depth) {
      maxima$helper_depth <<- length(stack)
      deepest <<- vapply(stack, function(frame) frame$name, "")
    }
    number_apply = length(active_apply) + as.integer(name == "apply_domain_restriction")
    maxima$active_apply <<- max(maxima$active_apply, number_apply)
    stopifnot(number_apply <= initial_weight + 1L,
      length(stack) <= (longest + 1L) * number_apply + longest)
    invisible(NULL)
  }
  observe_environment$.leave = function() { stack[[length(stack)]] <<- NULL; invisible(NULL) }
  observed_body = original_body
  for (j in seq_along(definitions)) {
    index = definitions[[j]]
    old = observed_body[[index]][[3L]][[3L]]
    observed_body[[index]][[3L]][[3L]] = substitute({
      .enter(NAME, parent.env(environment()))
      on.exit(.leave(), add = TRUE)
      OLD
    }, list(NAME = helper_names[[j]], OLD = old))
  }
  observed = plain_simplify
  body(observed) = observed_body
  environment(observed) = observe_environment
  ordinary = plain_simplify(case$clauses, universe)
  traced = observed(case$clauses, universe)
  stopifnot(identical(ordinary, traced), length(stack) == 0L)
  results[[length(results) + 1L]] = c(list(label = case$label, initial_fiber_weight = initial_weight), maxima,
    list(deepest_helper_path = deepest))
  if (length(results) %% 50L == 0L) cat("Completed", length(results), "cases\n")
}
suffix = if (getRversion() < "4.0.0") "r36" else "r46"
summary = list(R_version = R.version.string, cases = length(results),
  max_helper_depth = max(vapply(results, function(r) r$helper_depth, 0L)),
  max_active_apply = max(vapply(results, function(r) r$active_apply, 0L)),
  nested_apply_checks = sum(vapply(results, function(r) r$nested_apply_checks, 0L)),
  longest_acyclic_nodes = longest)
saveRDS(list(summary = summary, results = results), paste0("attic/cnf_verify3/root/recursion_progress_", suffix, ".rds"))
writeLines(toJSON(summary, auto_unbox = TRUE, pretty = TRUE),
  paste0("attic/cnf_verify3/root/recursion_progress_", suffix, ".json"))
print(summary)
