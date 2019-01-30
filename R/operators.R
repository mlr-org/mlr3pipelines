########## Basic operator %>>% for 1-to-1, 1-to-n, n-to-1 ##########


# FIXME: getter for length of channels
# FIXME: lhs and rhs must be the number of pipeops,  and for each pair the number of channels must be equal

# we nneed to DOCUMENT semantics of the >> op, wrt to what happens with multiple rhs / lhs channels

# we need to simply document a few cases where >> works. it does not need to in totally arbitrary cases, but robustly in the cases it
# asserts

#' @title PipeOp Composition Operator
#'
#' @description
#' This operator \dQuote{pipes} data from the source `g1` into the sink `g2`. Both source and sink can either be
#' a [Graph] or a [PipeOp]. The number of output channels of `g1` must equal the number of input channels
#' of the `g2`.
#'
#' @param g1 ([`Graph`] | [`PipeOp`]) \cr
#'   [Graph] / [PipeOp] to put in front of `g2`.
#' @param g2 ([`Graph`] | [`PipeOp`]) \cr
#'   [Graph] / [PipeOp] to put after  `g1`.
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
