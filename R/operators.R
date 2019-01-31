#' @title PipeOp Composition Operator
#'
#' @description
#' This operator \dQuote{pipes} data from the source `g1` into the sink `g2`. Both source and sink can either be
#' a [`Graph`] or a [`PipeOp`]. The number of output channels of `g1` (as given by `g1$output`) must equal the
#' number of input channels of `g2` (as given by `g2$input`). Connections between channels are created in the
#' order in which they occur in `g1` and `g2`, respectively: `g1`'s output channel 1 is connected to `g2`'s input
#' channel 1, channel 2 to 2 etc.
#'
#' This operator always created deep copies of its input arguments, so they cannot be modified by reference.
#' To access individual `PipeOp`s after composition, use the resulting `Graph`'s `$pipeops` list.
#'
#'
#' @param g1 ([`Graph`] | [`PipeOp`]) \cr
#'   `Graph` / `PipeOp` to put in front of `g2`.
#' @param g2 ([`Graph`] | [`PipeOp`]) \cr
#'   `Graph` / `PipeOp` to put after  `g1`.
#'
#' @examples
#' o1 = PipeOpScale$new()
#' o2 = PipeOpPCA$new()
#'
#' # The following two are equivalent:
#' result1 = o1 %>>% o2
#'
#' result2 = Graph$new()$
#'   add_pipeop(o1$copy(deep = TRUE))$
#'   add_pipeop(o2$copy(deep = TRUE))$
#'   add_edge(o1$id, o2$id)
#' @family mlr3pipelines backend related
#' @export
`%>>%` = function(g1, g2) {
  g1 = ensure_graph(g1)
  g2 = ensure_graph(g2)
  g1out = g1$output
  g2in = g2$input
  if (nrow(g1out) != nrow(g2in)) {
    stopf("Graphs / PipeOps to be connected have mismatching number of inputs / outputs.")
  }
  g = gunion(list(g1, g2))

  # build edges from free output channels of g1 and free input channels of g2
  new_edges = cbind(g1out[, list(src_id = get("op.id"), src_channel = get("channel.name"))],
    g2in[, list(dst_id = get("op.id"), dst_channel = get("channel.name"))])
  g$edges = rbind(g$edges, new_edges)
  g
}
