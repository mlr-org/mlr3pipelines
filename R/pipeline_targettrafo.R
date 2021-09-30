#' @include mlr_graphs.R

#' @title Transform and Re-Transform the Target Variable
#' @name mlr_graphs_targettrafo
#' @description
#' Wraps a [`Graph`] that transforms a target during training and inverts the transformation
#' during prediction. This is done as follows:
#' * Specify a transformation and inversion function using any subclass of [`PipeOpTargetTrafo`], defaults to
#'   [`PipeOpTargetMutate`], afterwards apply `graph`.
#' * At the very end, during prediction the transformation is inverted using [`PipeOpTargetInvert`].
#' * To set a transformation and inversion function for [`PipeOpTargetMutate`] see the
#'   parameters `trafo` and `inverter` of the `param_set` of the resulting [`Graph`].
#' * Note that the input `graph` is not explicitly checked to actually return a
#'   [`Prediction`][mlr3::Prediction] during prediction.
#'
#' @param graph [`PipeOpLearner`] | [`Graph`] \cr
#'   A [`PipeOpLearner`] or [`Graph`] to wrap between a transformation and re-transformation of the target variable.
#' @param trafo_pipeop [`PipeOp`] \cr
#'   A [`PipeOp`] that is a subclass of [`PipeOpTargetTrafo`]. Default is [`PipeOpTargetMutate`].
#' @param id_prefix `character(1)` \cr
#'   Optional id prefix to prepend to [`PipeOpTargetInvert`] ID. The resulting ID will be `"[id_prefix]targetinvert"`. Default is `""`.
#'
#' @return [`Graph`]
#' @export
#' @examples
#' library("mlr3")
#'
#' tt = pipeline_targettrafo(PipeOpLearner$new(LearnerRegrRpart$new()))
#' tt$param_set$values$targetmutate.trafo = function(x) log(x, base = 2)
#' tt$param_set$values$targetmutate.inverter = function(x) list(response = 2 ^ x$response)
#'
#' # gives the same as
#' g = Graph$new()
#' g$add_pipeop(PipeOpTargetMutate$new(param_vals = list(
#'   trafo = function(x) log(x, base = 2),
#'   inverter = function(x) list(response = 2 ^ x$response))
#'   )
#' )
#' g$add_pipeop(LearnerRegrRpart$new())
#' g$add_pipeop(PipeOpTargetInvert$new())
#' g$add_edge(src_id = "targetmutate", dst_id = "targetinvert",
#'   src_channel = 1, dst_channel = 1)
#' g$add_edge(src_id = "targetmutate", dst_id = "regr.rpart",
#'   src_channel = 2, dst_channel = 1)
#' g$add_edge(src_id = "regr.rpart", dst_id = "targetinvert",
#'   src_channel = 1, dst_channel = 2)
pipeline_targettrafo = function(graph, trafo_pipeop = PipeOpTargetMutate$new(), id_prefix = "") {
  graph = as_graph(graph)
  if (graph$pipeops[[graph$input$op.id]]$innum != 1L) {
    stopf("First PipeOp of graph should accept a single task as input.")
  }
  assert_r6(trafo_pipeop, classes = "PipeOpTargetTrafo")
  assert_string(id_prefix)

  input_id = graph$input$op.id
  output_id = graph$output$op.id
  trafo_pipeop_id = trafo_pipeop$id
  target_invert_id = paste0(id_prefix, "targetinvert")

  graph$add_pipeop(trafo_pipeop)
  graph$add_pipeop(PipeOpTargetInvert$new(target_invert_id))

  graph$add_edge(src_id = trafo_pipeop_id, dst_id = target_invert_id, src_channel = 1L, dst_channel = 1L)
  graph$add_edge(src_id = trafo_pipeop_id, dst_id = input_id, src_channel = 2L, dst_channel = 1L)
  graph$add_edge(src_id = output_id, dst_id = target_invert_id, src_channel = 1L, dst_channel = 2L)

  graph
}

mlr_graphs$add("targettrafo", pipeline_targettrafo)
