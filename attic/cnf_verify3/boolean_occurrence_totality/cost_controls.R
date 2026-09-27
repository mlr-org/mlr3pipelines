# Small occurrence-cost and deep-recursion controls, independent of the
# boundary observer. Source is wrapped in memory; no production edit occurs.
out_dir = "attic/cnf_verify3/boolean_occurrence_totality"
tag = if (getRversion() < "4") "r36" else "r46"
helper_names = c("char_intersect", "char_setdiff", "char_union", "return_entries",
  "register_unit", "apply_domain_restriction", "eliminate_symbol_from_clause",
  "on_updated_subset_relations", "on_update_range", "handle_sse_2nd_order_oneend",
  "handle_sse_2nd_order_twoend", "try_sse_2nd_order", "eliminate_clause_update_sr")
metric = new.env(parent = emptyenv())
enter = function(name, state) {
  if (length(metric$stack)) {
    at = length(metric$stack)
    metric$stack[[at]]$children = metric$stack[[at]]$children + 1L
  } else metric$roots = metric$roots + 1L
  w = sum(lengths(state$entries))
  if (name == "apply_domain_restriction") {
    for (ancestor in metric$stack) if (ancestor$name == name) {
      stopifnot(w < ancestor$weight)
      metric$strict_checks = metric$strict_checks + 1L
    }
  }
  metric$stack[[length(metric$stack) + 1L]] = list(name = name, children = 0L, weight = w)
  metric$depth = max(metric$depth, length(metric$stack))
  metric$calls = metric$calls + 1L
}
leave = function() {
  last = tail(metric$stack, 1L)[[1L]]
  previous = metric$children[[last$name]]
  metric$children[[last$name]] = max(c(previous, last$children))
  metric$stack = head(metric$stack, -1L)
}
wrap = function(x) {
  if (!is.call(x)) return(x)
  p = as.list(x)
  for (i in seq_along(p)[-1L]) {
    if (identical(p[i], list(quote(expr = )))) next
    p[i] = list(wrap(p[[i]]))
  }
  if (identical(p[[1L]], as.name("=")) && is.symbol(p[[2L]]) &&
    as.character(p[[2L]]) %in% helper_names && is.call(p[[3L]]) &&
    identical(p[[3L]][[1L]], as.name("function"))) {
    p[[3L]][[3L]] = substitute({
      enter(NAME, parent.env(environment()))
      on.exit(leave(), add = TRUE)
      BODY
    }, list(NAME = as.character(p[[2L]]), BODY = p[[3L]][[3L]]))
  }
  as.call(p)
}
original_env = new.env(parent = globalenv())
observed_env = new.env(parent = globalenv())
sys.source("R/CnfFormula_simplify.R", original_env)
eval(wrap(parse("R/CnfFormula_simplify.R")[[1L]]), observed_env)
cases = list()
run = function(words, label) {
  n = max(abs(unlist(words)))
  universe = list2env(setNames(rep(list(c("0", "1")), n), paste0("X", seq_len(n))), parent = emptyenv())
  entries = lapply(words, function(word) setNames(lapply(word,
    function(lit) if (lit > 0L) "1" else "0"), paste0("X", abs(word))))
  metric$stack = list()
  metric$roots = 0L
  metric$children = list()
  metric$depth = 0L
  metric$calls = 0L
  metric$strict_checks = 0L
  result = observed_env$simplify_cnf(entries, universe)
  stopifnot(identical(result, original_env$simplify_cnf(entries, universe)), !length(metric$stack))
  m = length(entries)
  w = sum(lengths(entries))
  stopifnot(metric$depth <= 13L * (w + 1L))
  cases[[label]] <<- list(words = words, clauses = m, occurrences = w,
    helper_roots = metric$roots, helper_calls = metric$calls,
    max_children = metric$children, max_helper_depth = metric$depth,
    strict_restriction_ancestor_checks = metric$strict_checks)
}
for (n in c(4L, 32L, 128L)) {
  label = paste0("fixed_two_clauses_", n)
  run(list(-1L, c(rep(1L, n), 2L)), label)
  stopifnot(cases[[label]]$helper_roots == n + 3L)
  label = paste0("fixed_three_clauses_", n)
  run(list(c(-1L, -3L), c(-1L, 3L), c(rep(1L, n), 2L)), label)
  stopifnot(cases[[label]]$max_children$register_unit == n + 1L)
}
for (n in c(8L, 24L, 48L)) {
  chain = lapply(seq_len(n), function(i) c(-i, -i, i + 1L))
  run(c(list(1L), rev(chain)), paste0("reverse_duplicate_unit_chain_", n))
}
stopifnot(cases$fixed_two_clauses_32$helper_roots > 4L * 2L^2L + 3L * 2L + 1L,
  cases$fixed_three_clauses_32$max_children$register_unit > 2L * 3L + 3L)
result = list(R = R.version.string, cases = cases,
  source_md5 = tools::md5sum("R/CnfFormula_simplify.R"))
saveRDS(result, file.path(out_dir, paste0("cost_controls_", tag, ".rds")))
for (label in names(cases)) {
  cat(label, "\n")
  print(cases[[label]][setdiff(names(cases[[label]]), "words")])
}
