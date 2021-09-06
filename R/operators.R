#' @title PipeOp Composition Operator
#'
#' @description
#' This operator "pipes" data from the source `g1` into the sink `g2`. Both source and sink can either be
#' a [`Graph`] or a [`PipeOp`] (or an object that can be automatically converted into a [`Graph`] or [`PipeOp`], see [`as_graph()`] and [`as_pipeop()`]).
#'
#' `%>>%` tries to automatically match output channels of `g1` to input channels of `g2`; this is only possible if either
#' * the number of output channels of `g1` (as given by `g1$output`) is equal to the
#' number of input channels of `g2` (as given by `g2$input`), or
#' * `g1` has only one output channel (i.e. `g1$output` has one line), or
#' * `g2` has only one input channel, which is a *vararg* channel (i.e. `g2$input` has one line, with `name` entry `"..."`).
#'
#' Connections between channels are created in the
#' order in which they occur in `g1` and `g2`, respectively: `g1`'s output channel 1 is connected to `g2`'s input
#' channel 1, channel 2 to 2 etc.
#'
#' This operator always created deep copies of its input arguments, so they cannot be modified by reference afterwards.
#' To access individual [`PipeOp`]s after composition, use the resulting [`Graph`]'s `$pipeops` list.
#'
#' Both arguments of `%>>%` are automatically converted to [`Graph`]s using [`as_graph()`]; this means that objects on either side may be objects
#' that can be automatically converted to [`PipeOp`]s (such as [`Learner`][mlr3::Learner]s or [`Filter`][mlr3filters::Filter]s), or that can
#' be converted to [`Graph`]s. This means, in particular, `list`s of [`Graph`]s, [`PipeOp`]s or objects convertible to that, because
#' [`as_graph()`] automatically applies [`gunion()`] to `list`s. See examples.
#'
#' Note that if `g1` is `NULL`, `g2` converted to a [`Graph`] will be returned.
#' Analogously, if `g2` is `NULL`, `g1` converted to a [`Graph`] will be returned.
#'
#' @param g1 ([`Graph`] | [`PipeOp`] | [`Learner`][mlr3::Learner] | [`Filter`][mlr3filters::Filter] | `list` | `...`) \cr
#'   [`Graph`] / [`PipeOp`] / object-convertible-to-[`PipeOp`] to put in front of `g2`.
#' @param g2 ([`Graph`] | [`PipeOp`] | [`Learner`][mlr3::Learner] | [`Filter`][mlr3filters::Filter] | `list` | `...`) \cr
#'   [`Graph`] / [`PipeOp`] / object-convertible-to-[`PipeOp`] to put after  `g1`.
#'
#' @return [`Graph`]: the constructed [`Graph`].
#' @family Graph operators
#' @export
#' @examples
#' o1 = PipeOpScale$new()
#' o2 = PipeOpPCA$new()
#' o3 = PipeOpFeatureUnion$new(2)
#'
#' # The following two are equivalent:
#' pipe1 = o1 %>>% o2
#'
#' pipe2 = Graph$new()$
#'   add_pipeop(o1$clone(deep = TRUE))$
#'   add_pipeop(o2$clone(deep = TRUE))$
#'   add_edge(o1$id, o2$id)
#'
#' # Note automatical gunion() of lists.
#' # The following three are equivalent:
#' graph1 = list(o1, o2) %>>% o3
#'
#' graph2 = gunion(list(o1, o2)) %>>% o3
#'
#' graph3 = Graph$new()$
#'   add_pipeop(o1$clone(deep = TRUE))$
#'   add_pipeop(o2$clone(deep = TRUE))$
#'   add_pipeop(o3$clone(deep = TRUE))$
#'   add_edge(o1$id, o3$id, dst_channel = 1)$
#'   add_edge(o2$id, o3$id, dst_channel = 2)
`%>>%` = function(g1, g2) {
  # neutral elements handling
  if (is.null(g1)) {
    return(as_graph(g2))
  }
  if (is.null(g2)) {
    return(as_graph(g1))
  }

  g1 = as_graph(g1)
  g2 = as_graph(g2)
  g1out = g1$output
  g2in = g2$input
  if (nrow(g1out) != 1 && nrow(g1out) != nrow(g2in) && !(nrow(g2in) == 1 && g2in$channel.name == "...")) {
    stopf("Graphs / PipeOps to be connected have mismatching number of inputs / outputs.")
  }
  g = gunion(list(g1, g2))


  # check that types agree
  for (row in seq_len(max(nrow(g1out), nrow(g2in)))) {
    outrow = min(nrow(g1out), row)
    inrow = min(nrow(g2in), row)
    if (!are_types_compatible(strip_multiplicity_type(g1out$train[outrow]), strip_multiplicity_type(g2in$train[inrow]))) {
      stopf("Output type of PipeOp %s during training (%s) incompatible with input type of PipeOp %s (%s)",
        g1out$op.id[outrow], g1out$train[outrow], g2in$op.id[inrow], g2in$train[inrow])
    }
    if (!are_types_compatible(strip_multiplicity_type(g1out$predict[outrow]), strip_multiplicity_type(g2in$predict[inrow]))) {
      stopf("Output type of PipeOp %s during prediction (%s) incompatible with input type of PipeOp %s (%s)",
        g1out$op.id[outrow], g1out$predict[outrow], g2in$op.id[inrow], g2in$predict[inrow])
    }
  }

  # build edges from free output channels of g1 and free input channels of g2
  new_edges = cbind(g1out[, list(src_id = get("op.id"), src_channel = get("channel.name"))],
    g2in[, list(dst_id = get("op.id"), dst_channel = get("channel.name"))])
  g$edges = rbind(g$edges, new_edges)
  g
}

strip_multiplicity_type = function(type) {
  gsub("^\\[*|\\]*$", "", type)
}
