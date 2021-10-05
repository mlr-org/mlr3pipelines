#' @title PipeOp Composition Operator
#'
#' @description
#' These operators creates a connection that "pipes" data from the source `g1` into the sink `g2`.
#' Both source and sink can either be
#' a [`Graph`] or a [`PipeOp`] (or an object that can be automatically converted into a [`Graph`] or [`PipeOp`], see [`as_graph()`] and [`as_pipeop()`]).
#'
#' `%>>%` and `%>>!%` try to automatically match output channels of `g1` to input channels of `g2`; this is only possible if either
#' * the number of output channels of `g1` (as given by `g1$output`) is equal to the
#' number of input channels of `g2` (as given by `g2$input`), or
#' * `g1` has only one output channel (i.e. `g1$output` has one line), or
#' * `g2` has only one input channel, which is a *vararg* channel (i.e. `g2$input` has one line, with `name` entry `"..."`).
#'
#' Connections between channels are created in the
#' order in which they occur in `g1` and `g2`, respectively: `g1`'s output channel 1 is connected to `g2`'s input
#' channel 1, channel 2 to 2 etc.
#'
#' `%>>%` always creates deep copies of its input arguments, so they cannot be modified by reference afterwards.
#' To access individual [`PipeOp`]s after composition, use the resulting [`Graph`]'s `$pipeops` list.
#' `%>>!%`, on the other hand, tries to avoid cloning its first argument: If it is a [`Graph`], then this [`Graph`]
#' will be modified in-place.
#'
#' When `%>>!%` fails, then it leaves `g1` in an incompletely modified state. It is therefore usually recommended to use
#' `%>>%`, since the very marginal gain of performance from
#' using `%>>!%` often does not outweigh the risk of either modifying objects by-reference that should not be modified or getting
#' graphs that are in an incompletely modified state. However,
#' when creating long [`Graph`]s, chaining with `%>>!%` instead of `%>>%` can give noticeable performance benefits
#' because `%>>%` makes a number of `clone()`-calls that is quadratic in chain length, `%>>!%` only linear.
#'
#' `concat_graphs(g1, g2, in_place = FALSE)` is equivalent to `g1 %>>% g2`. `concat_graphs(g1, g2, in_place = TRUE)` is equivalent to `g1 %>>!% g2`.
#'
#' Both arguments of `%>>%` are automatically converted to [`Graph`]s using [`as_graph()`]; this means that objects on either side may be objects
#' that can be automatically converted to [`PipeOp`]s (such as [`Learner`][mlr3::Learner]s or [`Filter`][mlr3filters::Filter]s), or that can
#' be converted to [`Graph`]s. This means, in particular, `list`s of [`Graph`]s, [`PipeOp`]s or objects convertible to that, because
#' [`as_graph()`] automatically applies [`gunion()`] to `list`s. See examples. If the first argument of `%>>!%` is not a [`Graph`], then
#' it is cloned just as when `%>>%` is used; `%>>!%` only avoids `clone()` if the first argument is a [`Graph`].
#'
#' Note that if `g1` is `NULL`, `g2` converted to a [`Graph`] will be returned.
#' Analogously, if `g2` is `NULL`, `g1` converted to a [`Graph`] will be returned.
#'
#' @section Chaining Graphs:
#' `concat_graphs` can also be called with the `glist` argument, which takes an arbitrary amount of [`Graph`]s or [`PipeOp`]s (or objects that can be automatically
#' converted into [`Graph`]s or [`PipeOp`]s) and joins them in a serial [`Graph`], as if connecting them using [`%>>%`].
#'
#' Care is taken to avoid unnecessarily cloning of components. A call of
#' `chain_graphs(list(g1, g2, g3, g4, ...), in_place = FALSE)` is equivalent to
#' `g1 %>>% g2 %>>!% g3 %>>!% g4 %>>!% ...`.
#' A call of `chain_graphs(list(g1, g2, g3, g4, ...), in_place = FALSE)`
#' is equivalent to `g1 %>>!% g2 %>>!% g3 %>>!% g4 %>>!% ...` (differing in the
#' first operator being `%>>!%` as well).
#'
#' `concat_graphs(glist = <list>)` (implicitly with `in_place = FALSE`) is a safe way of generating large linear pipelines quickly, while
#' still avoiding to change any of its inputs by reference, and avoiding the risk of ending up with broken objects.
#'
#' @param g1 ([`Graph`] | [`PipeOp`] | [`Learner`][mlr3::Learner] | [`Filter`][mlr3filters::Filter] | `list` | `...`) \cr
#'   [`Graph`] / [`PipeOp`] / object-convertible-to-[`PipeOp`] to put in front of `g2`.
#' @param g2 ([`Graph`] | [`PipeOp`] | [`Learner`][mlr3::Learner] | [`Filter`][mlr3filters::Filter] | `list` | `...`) \cr
#'   [`Graph`] / [`PipeOp`] / object-convertible-to-[`PipeOp`] to put after  `g1`.
#' @param glist `list` of ([`Graph`] | [`PipeOp`] | [`Learner`][mlr3::Learner] | [`Filter`][mlr3filters::Filter] | `list` | `...`)\cr
#'   List of elements which are the [`Graph`]s to be joined. Elements must be convertible to [`Graph`] or [`PipeOp`] using [`as_graph()`] and [`as_pipeop()`].
#'   `NULL` is the neutral element of [`%>>%`] and skipped. When this is given, `g1` and `g2` must not be given.
#' @param in_place (`logical(1)`)\cr
#'   When `g1` and `g2` are given: Whether to try to avoid cloning `g1`. If `g1` is not a [`Graph`], then it is cloned regardless.\n
#'   When `glist` is given instead:
#'   Whether to try to avoid cloning the first element of `glist`, similar to the difference
#'   of `%>>!%` over `%>>%`. This can only be avoided if `glist[[1]]` is already a [`Graph`].
#'   Beware that, when `in_place` is `TRUE` and if `concat_graphs()` fails because of id collisions, then `glist[[1]]` will possibly be in an incompletely
#'   modified state.
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
#'   add_pipeop(o1)$
#'   add_pipeop(o2)$
#'   add_edge(o1$id, o2$id)
#'
#' # Note automatical gunion() of lists.
#' # The following three are equivalent:
#' graph1 = list(o1, o2) %>>% o3
#'
#' graph2 = gunion(list(o1, o2)) %>>% o3
#'
#' graph3 = Graph$new()$
#'   add_pipeop(o1)$
#'   add_pipeop(o2)$
#'   add_pipeop(o3)$
#'   add_edge(o1$id, o3$id, dst_channel = 1)$
#'   add_edge(o2$id, o3$id, dst_channel = 2)
#'
#' pipe1 %>>!% o3  # modify pipe1 in-place
#'
#' pipe1  # contains o1, o2, and o3 now.
#'
#' o1 %>>!% o2
#'
#' o1  # not changed, becuase not a Graph.
#'
#' concat_graphs(glist = list(o1, o2, o3))
concat_graphs = function(g1, g2, glist, in_place = FALSE) {
  if (!missing(glist)) {
    if (!missing(g1) || !missing(g2)) stop("When glist is given, g1 and g2 must not be given")
    return(chain_graphs(graphs = glist, in_place = in_place))
  }
  assert_flag(in_place)
  # neutral elements handling
  if (is.null(g1)) return(if (!is.null(g2)) as_graph(g2, clone = TRUE))
  if (is.null(g2)) return(as_graph(g1, clone = !in_place))

  # one idea would be to not clone here, and let `gunion()` decide whether to clone. However,
  # that would lead to `PipeOp`s being cloned twice, so we clone here explicitly and tell gunion to do things in-place.
  g1 = as_graph(g1, clone = !in_place)
  g2 = as_graph(g2, clone = TRUE)
  g1out = g1$output
  g2in = g2$input
  if (nrow(g1out) != 1 && nrow(g1out) != nrow(g2in) && !(nrow(g2in) == 1 && g2in$channel.name == "...")) {
    stopf("Graphs / PipeOps to be connected have mismatching number of inputs / outputs.")
  }


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

  g = gunion(list(g1, g2), in_place = c(TRUE, TRUE))  # at this point graphs are already cloned.
  g$edges = rbind(g$edges, new_edges)
  g
}

#' @rdname concat_graphs
#' @export
`%>>%` = function(g1, g2) {
  concat_graphs(g1, g2, in_place = FALSE)
}

#' @rdname concat_graphs
#' @export
`%>>!%` = function(g1, g2) {
  concat_graphs(g1, g2, in_place = TRUE)
}

strip_multiplicity_type = function(type) {
  gsub("^\\[*|\\]*$", "", type)
}

#' @title Chain a Series of Graphs
#'
#' @description
#' Takes an arbitrary amount of [`Graph`]s or [`PipeOp`]s (or objects that can be automatically
#' converted into [`Graph`]s or [`PipeOp`]s, see [`as_graph()`] and [`as_pipeop()`]) as inputs and joins
#' them in a serial [`Graph`], as if connecting them using [`%>>%`].
#'
#' Care is taken to avoid unnecessarily cloning of components. A call of
#' `chain_graphs(list(g1, g2, g3, g4, ...), in_place = FALSE)` is equivalent to
#' `g1 %>>% g2 %>>!% g3 %>>!% g4 %>>!% ...`.
#' A call of `chain_graphs(list(g1, g2, g3, g4, ...), in_place = FALSE)`
#' is equivalent to `g1 %>>!% g2 %>>!% g3 %>>!% g4 %>>!% ...` (differing in the
#' first operator being `%>>!%` as well).
#'
#' @param graphs `list` of ([`Graph`] | [`PipeOp`] | `NULL` | `...`)\cr
#'   List of elements which are the
#'   [`Graph`]s to be joined. Elements must be convertible to [`Graph`] or [`PipeOp`] using [`as_graph()`] and [`as_pipeop()`].
#'   `NULL` is the neutral element of [`%>>%`] and skipped.
#' @param in_place (`logical(1)`)\cr
#'   Whether to try to avoid cloning the first element of `graphs`, similar to the difference
#'   of [`%>>!%`] over [`%>>%`]. This can only be avoided if `graphs[[1]]` is already a [`Graph`].
#'   Beware that, if `chain_graphs()` fails because of id collisions, then `graphs[[1]]` will possibly be in an incompletely
#'   modified state when `in_place` is `TRUE`.
#' @return [`Graph`] the resulting [`Graph`], or `NULL` if there are no non-null values in `graphs`.
#' @export
chain_graphs = function(graphs, in_place = FALSE) {
  assert_list(graphs)
  graphs = discard(graphs, is.null)
  if (!length(graphs)) return(NULL)
  if (!in_place) {
    # all except the first graph get cloned, so if we are in_place,
    # we only need to take care to clone it. We convert it to a Graph,
    # so `%>>!%` will not clone it again.
    graphs[[1]] = as_graph(graphs[[1]], clone = TRUE)
  }
  Reduce(`%>>!%`, graphs)
}
