#' @include mlr_graphs.R

#' @title Create a bagging learner
#' @name mlr_graphs_bagging
#' @description
#' Creates a [`Graph`] that performs bagging for a supplied graph.
#' This is done as follows:
#' * `Subsample` the data in each step using [`PipeOpSubsample`], afterwards apply `graph`
#' * Replicate this step `iterations` times (in parallel via [multiplicities][Multiplicity])
#' * Average outputs of replicated `graph`s predictions using the `averager`
#'   (note that setting `collect_multipliciy = TRUE` is required)
#'
#' All input arguments are cloned and have no references in common with the returned [`Graph`].
#'
#' @param graph [`PipeOp`] | [`Graph`] \cr
#'   A [`PipeOpLearner`] or [`Graph`] to create a robustifying pipeline for.
#'   Outputs from the replicated `graph`s are connected with the `averager`.
#' @param iterations `integer(1)` \cr
#'   Number of bagging iterations. Defaults to 10.
#' @param frac `numeric(1)` \cr
#'   Percentage of rows to keep during subsampling. See [`PipeOpSubsample`] for
#'   more information. Defaults to 0.7.
#' @param averager [`PipeOp`] | [`Graph`] \cr
#'   A [`PipeOp`] or [`Graph`] that averages the predictions from the
#'   replicated and subsampled graph's.
#'   In the simplest case, `po("classifavg")` and `po("regravg")` can be used
#'   in order to perform simple averaging of classification and regression
#'   predictions respectively.
#'   If `NULL` (default), no averager is added to the end of the graph.
#'   Note that setting `collect_multipliciy = TRUE` during construction of the averager is required.
#' @return [`Graph`]
#' @export
#' @examples
#' \donttest{
#' library(mlr3)
#' lrn_po = po("learner", lrn("regr.rpart"))
#' task = mlr_tasks$get("boston_housing")
#' gr = pipeline_bagging(lrn_po, 3, averager = po("regravg", collect_multiplicity = TRUE))
#' resample(task, GraphLearner$new(gr), rsmp("holdout"))
#' }
pipeline_bagging = function(graph, iterations = 10, frac = 0.7, averager = NULL) {
  g = as_graph(graph)
  assert_count(iterations)
  assert_number(frac, lower = 0, upper = 1)
  if (!is.null(averager)) {
    if (NROW(averager$input) != 1L || !(grepl("\\[*\\]", x = averager$input$train) && grepl("\\[*\\]", x = averager$input$predict))) {
      stop("'averager' must collect multiplicities.")
    }
    averager = as_graph(averager, clone = TRUE)
  }

  po("replicate", param_vals = list(reps = iterations)) %>>!%
    po("subsample", param_vals = list(frac = frac)) %>>!%
    g %>>!%
    averager
}

mlr_graphs$add("bagging", pipeline_bagging)

