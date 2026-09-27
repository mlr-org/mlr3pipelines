# Synthetic callback-boundary controls using the exact production helper body.
# These are not claims that the manually assembled states are reachable inputs.
source("R/CnfFormula_simplify.R")
statements = as.list(body(simplify_cnf))[-1L]
definition = function(name) {
  matches = Filter(function(statement) {
    is.call(statement) && identical(statement[[1L]], as.name("=")) &&
      identical(statement[[2L]], as.name(name))
  }, statements)
  stopifnot(length(matches) == 1L)
  matches[[1L]]
}
queue_loop = Filter(function(statement) {
  is.call(statement) && identical(statement[[1L]], as.name("for")) &&
    identical(statement[[2L]], as.name("sse_idx"))
}, statements)
stopifnot(length(queue_loop) == 1L)

state = new.env(parent = globalenv())
state$available = 1:2
state$eliminated = c(FALSE, FALSE)
state$is_unit = c(FALSE, FALSE)
state$entries = list(
  list(s = c("1", "4"), r = "1"),
  list(s = c("1", "2", "4"), r = c("1", "2")))
state$is_not_subset_of = list(
  matrix(FALSE, 2L, 2L, dimnames = list(NULL, c("s", "r"))),
  matrix(FALSE, 2L, 2L, dimnames = list(NULL, c("s", "r"))))
state$not_subset_count = matrix(NA_integer_, 2L, 2L)
state$not_subset_count[1L, 2L] = 0L
state$second_order_enabled = FALSE
state$second_order_enabled_matrix = matrix(TRUE, 2L, 2L)
state$calls = list()
eval(quote({
  eliminate_clause_update_sr = function(clause_idx) {
    calls[[length(calls) + 1L]] <<- list(kind = "delete", target = clause_idx)
    eliminated[[clause_idx]] <<- TRUE
  }
  apply_domain_restriction = function(clause_idx, symbol, restringent, is_unit_propagation) {
    calls[[length(calls) + 1L]] <<- list(kind = "restrict", target = clause_idx,
      symbol = symbol, range = restringent, is_unit_propagation = is_unit_propagation)
    entries[[clause_idx]][[symbol]] <<- intersect(entries[[clause_idx]][[symbol]], restringent)
    FALSE
  }
  handle_sse_2nd_order_oneend = function(meta_idx, meta_idx_other, symbol) {
    calls[[length(calls) + 1L]] <<- list(kind = "oneend", source = unname(meta_idx),
      target = unname(meta_idx_other), symbol = symbol,
      flag = second_order_enabled_matrix[meta_idx, meta_idx_other])
    FALSE
  }
  handle_sse_2nd_order_twoend = function(meta_idx, meta_idx_other, symbol) {
    calls[[length(calls) + 1L]] <<- list(kind = "twoend", source = unname(meta_idx),
      target = unname(meta_idx_other), symbol = symbol,
      flag = second_order_enabled_matrix[meta_idx, meta_idx_other])
    FALSE
  }
}), state)
eval(definition("on_updated_subset_relations"), state)

# A zero-count visit is pending; target shrink changes its current count to one.
# Its current target range still contains a value outside the donor's range.
state$entries[[2L]]$s = c("1", "2")
state$is_not_subset_of[[1L]][2L, "s"] = TRUE
state$not_subset_count[1L, 2L] = 1L
before = state$entries
stopifnot(identical(state$on_updated_subset_relations(1L, 2L, FALSE), FALSE))
stopifnot(length(state$calls) == 1L,
  identical(state$calls[[1L]], list(kind = "restrict", target = 2L,
    symbol = "s", range = c("1", "4"), is_unit_propagation = FALSE)),
  identical(state$entries[[2L]]$s, "1"), !any(state$eliminated))
grid = expand.grid(s = as.character(1:4), r = as.character(1:3), stringsAsFactors = FALSE)
truth = function(clauses) {
  Reduce(`&`, lapply(clauses, function(clause) {
    Reduce(`|`, lapply(names(clause), function(symbol) grid[[symbol]] %in% clause[[symbol]]))
  }))
}
stopifnot(identical(truth(before), truth(state$entries)))
cat("PASS: a pending zero-to-one ordinary visit reads count one and restricts the current pivot.\n")

# A pair queued as twoend now has its stable final oneend role. Its ordinary
# oneend route can defer; the exact manual queue enables it before dispatch.
state$calls = list()
state$second_order_enabled = TRUE
state$second_order_enabled_matrix[1L, 2L] = FALSE
stopifnot(identical(state$on_updated_subset_relations(1L, 2L, TRUE), FALSE),
  length(state$calls) == 0L)
eval(definition("sse_to_trigger"), state)
eval(queue_loop[[1L]], state)
stopifnot(length(state$calls) == 1L,
  identical(state$calls[[1L]], list(kind = "oneend", source = 1L,
    target = 2L, symbol = "s", flag = TRUE)),
  state$second_order_enabled_matrix[1L, 2L])
cat("PASS: a queued pair now at count one is enabled before the current oneend handler.\n")

# The ordinary count-two branch precedes the flag test altogether.
state$calls = list()
state$not_subset_count[1L, 2L] = 2L
state$is_not_subset_of[[1L]][2L, "r"] = TRUE
state$second_order_enabled_matrix[1L, 2L] = FALSE
stopifnot(identical(state$on_updated_subset_relations(1L, 2L, FALSE), FALSE),
  length(state$calls) == 1L,
  identical(state$calls[[1L]], list(kind = "twoend", source = 1L,
    target = 2L, symbol = NULL, flag = FALSE)))
cat("PASS: an ordinary count-two callback dispatches even while the manual flag is FALSE.\n")

# The second_order_only flag never prevents the zero-count deletion branch.
state$calls = list()
state$not_subset_count[1L, 2L] = 0L
state$is_not_subset_of[[1L]][2L, ] = FALSE
stopifnot(is.null(state$on_updated_subset_relations(1L, 2L, TRUE)),
  identical(state$calls, list(list(kind = "delete", target = 2L))),
  state$eliminated[[2L]])
cat("PASS: a zero-count manual visit still deletes the target.\n")
