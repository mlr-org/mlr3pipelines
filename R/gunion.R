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
#' @param graphs `list` of ([`Graph`] | [`PipeOp`] | `NULL` | `...`)\cr
#'   List of elements which are the
#'   [`Graph`]s to be joined. Elements must be convertible to [`Graph`] or [`PipeOp`] using [`as_graph()`] and [`as_pipeop()`].
#'   `NULL` values automatically get converted to [`PipeOpNOP`] with a random ID of the format `nop_********`.
#'   The list can be named, in which case the
#'   IDs of the elements are prefixed with the names, separated by a dot (`.`).
#' @param in_place (`logical(1)` | `logical`)\cr
#'   Whether to try to avoid cloning the first element of `graphs`, similar to the difference
#'   of [`%>>!%`] over [`%>>%`]. This can only be avoided if `graphs[[1]]` is already a [`Graph`].\cr
#'   Unlike [`chain_graphs()`], `gunion()` does all checks *before* mutating `graphs[[1]]`, so it will not leave `graphs[[1]]`
#'   in an incompletely modified state when it fails.\cr
#'   `in_place` may also be of length `graph`, in which case it determines for each element of `graphs` whether it is cloned.
#'   This is for internal usage and is not recommended.
#' @return [`Graph`] the resulting [`Graph`].
#'
#' @family Graph operators
#' @export
gunion = function(graphs, in_place = FALSE) {
  assert_list(graphs)
  assert(check_flag(in_place), check_logical(in_place, any.missing = FALSE, len = length(graphs)))
  if (length(graphs) == 0L) return(Graph$new())
  graphs = map_if(graphs, is.null, function(x) po("nop", id = paste0("nop_", paste(sample(c(letters, 0:9), 8L, TRUE), collapse = ""))))
  do_clone = if (length(in_place) == length(graphs)) !in_place else c(!in_place, rep(TRUE, length(graphs) - 1L))
  graphs = structure(pmap(list(x = graphs, clone = do_clone), as_graph), names = names(graphs))
  graphs = Filter(function(x) length(x$pipeops), graphs)

  if (length(graphs) == 0L) return(Graph$new())

  if (in_place[[1L]]) {
    g = graphs[[1L]]
    g$.__enclos_env__$private$.param_set = NULL  # clear param_set cache
  } else {
    g = Graph$new()
  }

  new_pipeops = unlist(map(graphs, "pipeops"), recursive = FALSE)
  assert_names(names(new_pipeops), type = "unique", .var.name = "ids of pipe operators of graphs")
  g$pipeops = new_pipeops  # do this *after* the check so we don't have a modified graphs[[1]] when there are duplicates.
  if (!is.null(names(graphs))) imap(g$pipeops, function(p, i) p$id = i)
  if (is.null(names(graphs))) {
    g$edges = rbindlist(map(graphs, "edges"))
  } else {
    g$edges = rbindlist(imap(graphs, function(g, i) {
      edges = g$edges
      if (is.character(i) && nchar(i)) {
        edges[, c("src_id", "dst_id") := lapply(.SD, function(x) sprintf("%s.%s", i, x)),
          .SDcols = c("src_id", "dst_id")]
      }
      edges
    }), use.names = TRUE)
  }
  g
}
