#' @include mlr_graphs.R

#' @title Create a bagging learner
#' @name mlr_graphs_bag
#' @description
#' Creates a [`Graph`] that performs bagging for a supplied graph.
#' This is done as follows:
#' * `Subsample` the data in each step using [`PipeOpSubsample`], afterwards apply `graph`
#' * Replicate this step `iterations` times (in parallel via [multiplicities][Multiplicity])
#' * Average outputs of replicated `graph`s predictions using the `averager`
#'   (note that setting `collect_multiplicity = TRUE` is required)
#'
#' All input arguments are cloned and have no references in common with the returned [`Graph`].
#'
#' @param graph [`PipeOp`] | [`Graph`] \cr
#'   A [`PipeOpLearner`] or [`Graph`] to create a bagging pipeline for.
#'   Outputs from the replicated `graph`s are connected with the `averager`.
#' @param iterations `integer(1)` \cr
#'   Number of bagging iterations. Defaults to 10.
#' @param frac `numeric(1)` \cr
#'   Percentage of rows to keep during subsampling. See [`PipeOpSubsample`] for
#'   more information. Defaults to 1.
#' @param averager [`PipeOp`] | [`Graph`] \cr
#'   A [`PipeOp`] or [`Graph`] that averages the predictions from the
#'   replicated and subsampled graph's.
#'   In the simplest case, `po("classifavg")` and `po("regravg")` can be used
#'   in order to perform simple averaging of classification and regression
#'   predictions respectively.
#'   If `NULL` (default), no averager is added to the end of the graph.
#'   Note that setting `collect_multiplicity = TRUE` during construction of the averager is required.
#' @param replace `logical(1)` \cr
#'   Whether to sample with replacement.
#'   Default `TRUE`.
#' @return [`Graph`]
#' @export
#' @examplesIf requireNamespace("rpart")
#' \donttest{
#' library(mlr3)
#' lrn_po = po("learner", lrn("regr.rpart"))
#' task = mlr_tasks$get("boston_housing")
#' gr = pipeline_bag(lrn_po, 3, averager = po("regravg", collect_multiplicity = TRUE))
#' resample(task, GraphLearner$new(gr), rsmp("holdout"))$aggregate()
#'
#' # The original bagging method uses bootstrapping by sampling with replacement.
#' gr = ppl("bag", lrn_po,
#'   averager = po("regravg", collect_multiplicity = TRUE))
#' resample(task, GraphLearner$new(gr), rsmp("holdout"))$aggregate()
#' }
pipeline_bag = function(graph, iterations = 10, frac = 1, averager = NULL, replace = TRUE) {
  g = as_graph(graph)
  assert_flag(replace)
  assert_count(iterations)
  assert_number(frac, lower = 0, upper = 1)
  if (!is.null(averager)) {
    if (NROW(averager$input) != 1L || !(grepl("\\[*\\]", x = averager$input$train) && grepl("\\[*\\]", x = averager$input$predict))) {
      stop("'averager' must collect multiplicities.")
    }
    averager = as_graph(averager, clone = TRUE)
  }

  po("replicate", param_vals = list(reps = iterations)) %>>!%
    po("subsample", param_vals = list(frac = frac, replace = replace)) %>>!%
    g %>>!%
    averager
}

mlr_graphs$add("bag", pipeline_bag)
