# Independent reachability model for the pure Boolean implication fragment.
suppressPackageStartupMessages({ library(checkmate); library(mlr3misc); library(jsonlite) })
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
prefix = simplify_cnf
prefix_body = body(prefix)
boundary = which(vapply(as.list(prefix_body), function(expr) {
  is.call(expr) && identical(expr[[1L]], as.name("=")) && identical(expr[[2L]], as.name("remaining_entries"))
}, FALSE))
stopifnot(length(boundary) == 1L)
body(prefix) = as.call(c(as.list(prefix_body)[seq_len(boundary - 1L)],
  list(quote(return_entries(entries[!eliminated])))))
all_edges = function(n) {
  pairs = as.matrix(expand.grid(seq_len(n), seq_len(n)))
  pairs[pairs[, 1L] != pairs[, 2L], , drop = FALSE]
}
edge_keys = function(edges) if (nrow(edges)) paste(edges[, 1L], edges[, 2L], sep = ":") else character()
closure = function(n, edges) {
  result = diag(TRUE, n)
  if (nrow(edges)) result[edges] = TRUE
  for (k in seq_len(n)) result = result | outer(result[, k], result[k, ], "&")
  result
}
greedy = function(n, edges) {
  edges = edges[!duplicated(edge_keys(edges)), , drop = FALSE]
  active = rep(TRUE, nrow(edges))
  for (i in seq_len(nrow(edges))) {
    other = active
    other[[i]] = FALSE
    reach = closure(n, edges[other, , drop = FALSE])
    if (reach[edges[i, 1L], edges[i, 2L]]) active[[i]] = FALSE
  }
  edges[active, , drop = FALSE]
}
as_edges = function(formula) {
  if (is.logical(formula)) {
    stopifnot(isTRUE(formula))
    return(matrix(integer(), ncol = 2L))
  }
  rows = lapply(c(formula), function(clause) {
    stopifnot(length(clause) == 2L, all(lengths(clause) == 1L))
    vals = unlist(clause, use.names = FALSE)
    stopifnot(setequal(vals, c("0", "1")))
    as.integer(sub("^X", "", names(clause)[match(c("0", "1"), vals)]))
  })
  do.call(rbind, rows)
}
truth = function(n, edges) {
  grid = expand.grid(rep(list(c(FALSE, TRUE)), n))
  result = rep(TRUE, nrow(grid))
  for (i in seq_len(nrow(edges))) result = result & (!grid[[edges[i, 1L]]] | grid[[edges[i, 2L]]])
  result
}
stats = list(cases = 0L, assignment_rows = 0L, dag_cases = 0L, duplicate_cases = 0L,
  clause_essentiality_checks = 0L)
complete_three_outputs = list()
check_case = function(n, edges, label) {
  universe = CnfUniverse()
  symbols = lapply(seq_len(n), function(i) CnfSymbol(universe, paste0("X", i), c("0", "1")))
  raw = lapply(seq_len(nrow(edges)), function(i) setNames(list("0", "1"), paste0("X", edges[i, ])))
  clauses = lapply(seq_len(nrow(edges)), function(i) {
    CnfClause(list(CnfAtom(symbols[[edges[i, 1L]]], "0"), CnfAtom(symbols[[edges[i, 2L]]], "1")))
  })
  formula = CnfFormula(clauses)
  actual = as_edges(formula)
  expected = greedy(n, edges)
  stopifnot(identical(unname(actual), unname(expected)),
    identical(edge_keys(as_edges(prefix(raw, universe))), edge_keys(edges[!duplicated(edge_keys(edges)), , drop = FALSE])),
    identical(truth(n, actual), truth(n, edges)),
    identical(closure(n, actual), closure(n, edges)),
    identical(edge_keys(as_edges(CnfFormula(as.list(formula)))), edge_keys(actual)))
  if (nrow(actual)) for (i in seq_len(nrow(actual))) {
    other = actual[-i, , drop = FALSE]
    stopifnot(!identical(truth(n, other), truth(n, actual)))
    stats$clause_essentiality_checks <<- stats$clause_essentiality_checks + 1L
  }
  reach = closure(n, edges)
  dag = !any(reach & t(reach) & row(reach) != col(reach))
  stats$cases <<- stats$cases + 1L
  stats$dag_cases <<- stats$dag_cases + as.integer(dag)
  stats$duplicate_cases <<- stats$duplicate_cases + as.integer(anyDuplicated(edge_keys(edges)) != 0L)
  stats$assignment_rows <<- stats$assignment_rows + 2L^n
  key = paste(sort(edge_keys(actual)), collapse = ",")
  if (n == 3L && nrow(edges) == 6L && !anyDuplicated(edge_keys(edges))) {
    if (is.null(complete_three_outputs[[key]])) complete_three_outputs[[key]] <<- list(
      input = unname(edges), output = unname(actual), label = label, occurrences = 0L)
    complete_three_outputs[[key]]$occurrences <<- complete_three_outputs[[key]]$occurrences + 1L
  }
  if (stats$cases %% 1000L == 0L) cat("Completed", stats$cases, "graph cases\n")
  invisible(list(key = key, dag = dag))
}
edges3 = all_edges(3L)
visit = function(indices = integer()) {
  check_case(3L, edges3[indices, , drop = FALSE], paste0("three:", paste(indices, collapse = ",")))
  for (i in setdiff(seq_len(nrow(edges3)), indices)) visit(c(indices, i))
}
visit()
stopifnot(stats$cases == 1957L)
edges4 = all_edges(4L)
set.seed(9061510L)
for (mask in 0:4095) {
  indices = which(bitwAnd(mask, bitwShiftL(1L, 0:11)) != 0L)
  orders = list(indices, rev(indices), indices[sample.int(length(indices))])
  outputs = lapply(orders, function(order) check_case(4L, edges4[order, , drop = FALSE], paste0("four:", mask)))
  if (outputs[[1L]]$dag) stopifnot(length(unique(vapply(outputs, function(x) x$key, ""))) == 1L)
}
family_results = list()
for (n in c(3L, 4L, 5L, 6L, 8L)) {
  complete = all_edges(n)
  cycle = cbind(seq_len(n), c(seq.int(2L, n), 1L))
  star = rbind(cbind(1L, seq.int(2L, n)), cbind(seq.int(2L, n), 1L))
  for (wanted in list(cycle, cycle[, 2:1, drop = FALSE], star)) {
    other = complete[!edge_keys(complete) %in% edge_keys(wanted), , drop = FALSE]
    output = check_case(n, rbind(other, wanted), paste0("chosen normal form:", n))
    stopifnot(identical(output$key, paste(sort(edge_keys(wanted)), collapse = ",")))
    family_results[[length(family_results) + 1L]] = list(symbols = n, surviving_edges = nrow(wanted), key = output$key)
  }
}
check_case(3L, edges3[c(1L, 1L, 2L, 3L, 1L, 4L, 5L, 6L, 2L), ], "duplicates")
stopifnot(length(complete_three_outputs) == 5L,
  identical(unname(sort(vapply(complete_three_outputs, function(x) nrow(x$output), 0L))), c(3L, 3L, 4L, 4L, 4L)))
result = list(R_version = R.version.string, stats = stats,
  complete_three_outputs = complete_three_outputs, family_results = family_results)
suffix = if (getRversion() < "4.0.0") "r36" else "r46"
saveRDS(result, paste0("attic/cnf_verify3/root/pure_implication_graph_", suffix, ".rds"))
writeLines(toJSON(result, auto_unbox = TRUE, pretty = TRUE),
  paste0("attic/cnf_verify3/root/pure_implication_graph_", suffix, ".json"))
print(stats)
