#' @title Disjoint Union of Graphs
#'
#' @description
#' Takes an arbitrary amount of [`Graph`]s or [`PipeOp`]s (or objects that can be automatically
#' converted into [`Graph`]s or [`PipeOp`]s, see [`as_graph()`] and [`as_pipeop()`]) as inputs and joins
#' them in a new [`Graph`].
#'
#' The [`PipeOp`]s of the input [`Graph`]s are not joined with new edges across
#' [`Graph`]s, so if `length(graphs) > 1`, the resulting [`Graph`] will be disconnected.
#'
#' This operation always creates deep copies of its input arguments, so they cannot be modified by reference afterwards.
#' To access individual [`PipeOp`]s after composition, use the resulting [`Graph`]'s `$pipeops` list.
#'
#' @param graphs `list` of ([`Graph`] | [`PipeOp`])\cr
#'   List of elements which are the
#'   [`Graph`]s to be joined. Elements must be convertible to [`Graph`] or [`PipeOp`] using [`as_graph()`] and [`as_pipeop()`].
#'   The list can be named, in which case the
#'   IDs of the elements are prefixed with the names, separated by a dot (`.`).
#' @param in_place (`logical(1)`)\cr
#'   Whether to try to avoid cloning the first element of `graphs`, similar to the difference
#'   of [`%>>>%`] over [`%>>%`]. This can only be avoided if `graphs[[1]]` is already a [`Graph`].
#' @return [`Graph`] the resulting [`Graph`].
#'
#' @family Graph operators
#' @export
gunion = function(graphs, in_place = FALSE) {
  assert_list(graphs)
  if (length(graphs) == 0) return Graph$new()
  do_clone = c(!in_place, rep(TRUE, length(graphs) - 1))
  graphs = pmap(list(x = graphs, clone = do_clone), as_graph)
  graphs = Filter(function(x) length(x$pipeops), graphs)

  g = if (in_place) graphs[[1]] else Graph$new()

  g$pipeops = unlist(map(graphs, "pipeops"), recursive = FALSE)
  assert_names(names(g$pipeops), type = "unique", .var.name = "ids of pipe operators of graphs")
  imap(g$pipeops, function(p, i) p$id = i)
  g$edges = rbindlist(imap(graphs, function(g, i) {
    edges = g$edges
    if (is.character(i) && nchar(i)) {
      edges[, c("src_id", "dst_id") := lapply(.SD, function(x) sprintf("%s.%s", i, x)),
        .SDcols = c("src_id", "dst_id")]
    }
    edges
  }), use.names = TRUE)
}
