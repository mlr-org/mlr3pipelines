#' @include mlr_graphs.R

#' @title Create A Graph to Perform "One vs. Rest" classification.
#'
#' @description
#' Create a new [`Graph`] for a [classification Task][mlr3::TaskClassif] to
#' perform "One vs. Rest" classification.
#'
#' All input arguments are cloned and have no references in common with the returned [`Graph`].
#'
#' @param graph [`Graph`] \cr
#'   Graph being wrapped between [`PipeOpOVRSplit`] and [`PipeOpOVRUnite`].
#'   The Graph should return `NULL` during training and a
#'   [classification Prediction][mlr3::PredictionClassif] during prediction.
#' @return [`Graph`]
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("wine")
#'
#' learner = lrn("classif.rpart")
#' learner$predict_type = "prob"
#'
#' # Simple OVR
#' g1 = pipeline_ovr(learner)
#' g1$train(task)
#' g1$predict(task)
#'
#' # Bagged Learners
#' gr = po("replicate", reps = 3) %>>%
#'   po("subsample") %>>%
#'   learner %>>%
#'   po("classifavg", collect_multiplicity = TRUE)
#' g2 = pipeline_ovr(gr)
#' g2$train(task)
#' g2$predict(task)
#'
#' # Bagging outside OVR
#' g3 = po("replicate", reps = 3) %>>%
#'   pipeline_ovr(po("subsample") %>>% learner) %>>%
#'   po("classifavg", collect_multiplicity = TRUE)
#' g3$train(task)
#' g3$predict(task)
pipeline_ovr = function(graph) {
  graph = PipeOpOVRSplit$new() %>>>% graph
  if (graph$output$train != "NULL" || graph$output$predict != "PredictionClassif") {
    stopf("Graph should return 'NULL' during training and a 'PredictionClassif' during prediction.")
  }
  graph %>>>% PipeOpOVRUnite$new()
}

mlr_graphs$add("ovr", pipeline_ovr)
