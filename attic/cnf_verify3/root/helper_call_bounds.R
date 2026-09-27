# Count direct helper children and top-level helper roots independently of
# the earlier potential/depth observer. All observed definitions are private.
suppressPackageStartupMessages({ library(checkmate); library(mlr3misc); library(jsonlite) })
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
plain = simplify_cnf
top = body(plain)
definitions = which(vapply(as.list(top), function(expr) {
  is.call(expr) && identical(expr[[1L]], as.name("=")) && length(expr) == 3L &&
    is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))
}, FALSE))
helper_names = vapply(definitions, function(i) as.character(top[[i]][[2L]]), "")
stopifnot(length(helper_names) == 13L)

observe = function(clauses, universe, variant = "plain") {
  m = if (is.logical(clauses)) 0L else length(clauses)
  child_bound = 2 * m + 3
  root_bound = 4 * m * m + 3 * m + 1
  state = new.env(parent = globalenv())
  state$child_bound = child_bound
  state$root_bound = root_bound
  stack = integer()
  names_by_id = character()
  children = integer()
  roots = 0L
  max_depth = 0L
  source_body = top
  if (variant == "extra_children") {
    idx = definitions[[match("return_entries", helper_names)]]
    old = source_body[[idx]][[3L]][[3L]]
    source_body[[idx]][[3L]][[3L]] = substitute({
      for (.j in seq_len(child_bound + 1L)) char_setdiff(character(), character())
      OLD
    }, list(OLD = old))
  }
  state$.enter = function(name) {
    if (length(stack)) {
      parent = tail(stack, 1L)
      children[[parent]] <<- children[[parent]] + 1L
      if (children[[parent]] > child_bound) stop("Direct helper child bound exceeded")
    } else {
      roots <<- roots + 1L
      if (roots > root_bound) stop("Top-level helper root bound exceeded")
    }
    id = length(children) + 1L
    children[[id]] <<- 0L
    names_by_id[[id]] <<- name
    stack <<- c(stack, id)
    max_depth <<- max(max_depth, length(stack))
    invisible(NULL)
  }
  state$.leave = function() { stack <<- head(stack, -1L); invisible(NULL) }
  for (j in seq_along(definitions)) {
    idx = definitions[[j]]
    old = source_body[[idx]][[3L]][[3L]]
    source_body[[idx]][[3L]][[3L]] = substitute({
      .enter(NAME)
      on.exit(.leave(), add = TRUE)
      OLD
    }, list(NAME = helper_names[[j]], OLD = old))
  }
  if (variant == "extra_roots") {
    source_body = as.call(append(as.list(source_body), list(quote(
      for (.j in seq_len(root_bound + 1L)) char_union(character(), character())
    )), after = max(definitions)))
  }
  instrumented = plain
  body(instrumented) = source_body
  environment(instrumented) = state
  output = instrumented(clauses, universe)
  stopifnot(length(stack) == 0L, identical(output, plain(clauses, universe)))
  maxima = setNames(vapply(helper_names, function(name) {
    max(c(0L, children[names_by_id == name]))
  }, 0L), helper_names)
  list(clause_count = m, helper_calls = length(children), helper_roots = roots,
    max_helper_depth = max_depth, max_children = maxima,
    root_bound = root_bound, child_bound = child_bound)
}

cases = list(list(domains = list(X = c("0", "1")), clauses = TRUE),
  list(domains = list(X = c("0", "1")), clauses = FALSE),
  list(domains = list(X = c("0", "1")), clauses = list()),
  readRDS("attic/cnf_verify3/root/queued_comparison_control.rds")$case)
for (q in c(1L, 2L, 3L, 4L, 8L, 16L, 32L, 128L, 512L)) {
  domain = setNames(rep(list(c("a", "b", "c", "d")), q), paste0("F", seq_len(q)))
  clauses = lapply(c("b", "c", "d"), function(value) setNames(rep(list(c("a", value)), q), names(domain)))
  cases[[length(cases) + 1L]] = list(domains = domain, clauses = clauses, label = paste("frozen", q))
}
for (n in c(4L, 8L, 16L, 32L, 48L)) {
  domain = setNames(rep(list(c("0", "1")), n), paste0("X", seq_len(n)))
  edges = lapply(seq_len(n - 1L), function(i) setNames(list("0", "1"), paste0("X", c(i, i + 1L))))
  clauses = c(list(list(X1 = "1")), rev(edges))
  cases[[length(cases) + 1L]] = list(domains = domain, clauses = clauses, label = paste("unit", n))
  cases[[length(cases) + 1L]] = list(domains = c(domain, list(G = c("0", "1"))),
    clauses = lapply(clauses, function(clause) c(clause, list(G = "0"))), label = paste("guard", n))
}
set.seed(611029L)
for (trial in seq_len(1000L)) {
  n = sample(2:8, 1L)
  domain = setNames(lapply(seq_len(n), function(i) letters[seq_len(sample(2:6, 1L))]), paste0("S", seq_len(n)))
  clauses = lapply(seq_len(sample(1:18, 1L)), function(i) {
    symbols = sample(names(domain), sample.int(n, 1L))
    setNames(lapply(symbols, function(s) sample(domain[[s]], sample.int(length(domain[[s]]) - 1L, 1L))), symbols)
  })
  cases[[length(cases) + 1L]] = list(domains = domain, clauses = clauses, label = paste("random", trial))
}
results = lapply(cases, function(case) {
  universe = CnfUniverse()
  for (name in names(case$domains)) CnfSymbol(universe, name, case$domains[[name]])
  observe(case$clauses, universe)
})
# Output-preserving extra calls calibrate both distinct counting obligations.
universe = CnfUniverse()
CnfSymbol(universe, "X", c("0", "1"))
negative = vapply(c("extra_children", "extra_roots"), function(variant) {
  result = tryCatch(observe(list(list(X = "0")), universe, variant), error = function(e) conditionMessage(e))
  stopifnot(is.character(result), grepl("bound exceeded", result, fixed = TRUE))
  result
}, "")
summary = list(R_version = R.version.string, cases = length(results),
  helper_calls = sum(vapply(results, function(r) r$helper_calls, 0L)),
  helper_roots = sum(vapply(results, function(r) r$helper_roots, 0L)),
  max_helper_depth = max(vapply(results, function(r) r$max_helper_depth, 0L)),
  max_children = setNames(vapply(seq_along(helper_names), function(j) {
    max(vapply(results, function(r) r$max_children[[j]], 0L))
  }, 0L), helper_names), negative_controls = negative)
suffix = if (getRversion() < "4.0.0") "r36" else "r46"
saveRDS(list(summary = summary, results = results), paste0("attic/cnf_verify3/root/helper_call_bounds_", suffix, ".rds"))
writeLines(toJSON(summary, auto_unbox = TRUE, pretty = TRUE), paste0("attic/cnf_verify3/root/helper_call_bounds_", suffix, ".json"))
print(summary)
