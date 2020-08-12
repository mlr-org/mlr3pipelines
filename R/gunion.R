#' @title Disjoint Union of Graphs
#'
#' @description
#' Takes an arbitrary amount of [`Graph`]s or [`PipeOp`]s (or objects that can be automatically
#' converted into [`Graph`]s or [`PipeOp`]s, see [`as_graph()`] and [`as_pipeop()`]) as inputs and joins
#' them in a new [`Graph`].
#'
#' The [`PipeOp`]s of the input [`Graph`]s are not joined with new edges across
#' [`Graph`]s, so if `length(graphs) > 1` the resulting [`Graph`] will be disconnected.
#'
#' @param graphs `list` of ([`Graph`] | [`PipeOp`])\cr
#'   List of elements with one of the types defined above, which are the
#'   [`Graph`]s to be joined. The list can be named, in which case the
#'   IDs of the elements are prefixed with the names, separated by a dot (`.`).
#' @return [`Graph`] the resulting [`Graph`].
#'
#' @family Graph operators
#' @export
gunion = function(graphs) {
  assert_list(graphs)
  graphs = Filter(function(x) length(x$pipeops), map(graphs, as_graph))


  g = Graph$new()
  if (length(graphs)) {
    g$pipeops = unlist(map(graphs, "pipeops"), recursive = FALSE)
    assert_names(names(g$pipeops), type = "unique", .var.name = "ids of pipe operators")
    g$pipeops = map(g$pipeops, function(x) x$clone(deep = TRUE))
    imap(g$pipeops, function(p, i) p$id = i)
    g$edges = rbindlist(imap(graphs, function(g, i) {
      edges = copy(g$edges)
      if (is.character(i) && nchar(i)) {
        edges[, c("src_id", "dst_id") := lapply(.SD, function(x) sprintf("%s.%s", i, x)),
          .SDcols = c("src_id", "dst_id")]
      }
      edges
    }), use.names = TRUE)
  }
  g
}
