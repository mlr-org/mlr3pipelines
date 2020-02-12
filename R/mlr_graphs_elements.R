#' @include mlr_graphs.R

#' @title Robustify a learner
#'
#' @description
#' Creates a [`Graph`] that can be used to robustify any subsequent learner.
#' Performs the following steps:
#' * Drops empty factor levels using [`PipeOpFixFactors`]
#' * Imputes `numeric` features using [`PipeOpImputeHist`] and [`PipeOpMissInd`]
#' * Imputes `factor` features using [`PipeOpImputeNewlvl`]
#' * Encodes `factors` using `one-hot-encoding`. Factors with a cardinality > max_cardinality` are
#'   collapsed using [`PipeOpCollapseFactors`].
#' * If `scaling`, numeric features are scaled to mean 0 and standard deviation 1.
#'
#' The graph is built conservatively, i.e. the function always tries to assure everything works.
#' If a learner is provided, some steps can be left out, i.e. if the learner can deal with
#' factor variables, no encoding is performed.
#'
#' @param task [`Task`] \cr
#'   A task to create a robustifying pipeline for.
#' @param learner [`Learner`] \cr
#'   A learner to create a robustifying pipeline for. Optional, if omitted,
#'   a more conservative pipeline is built.
#' @param scaling [`logical`] \cr
#'   Should the data be scaled? Defaults to true.
#' @param max_cardinality [`integer`] \cr
#'   Maximum number of factor levels allowed. See above. Default: 1000.
#' @return Returns a [`Graph`]
#' @export
#' @examples
#' library(mlr3)
#' lrn = lrn("regr.rpart")
#' task = mlr_tasks$get("boston_housing")
#' gr = robustify_pipeline(task, lrn) %>>% po("learner", lrn)
#' resample(task,GraphLearner$new(gr), rsmp("holdout"))
robustify_pipeline = function(task, learner = NULL, scaling = TRUE, max_cardinality = 1000) {
  assert_task(task)
  if (!is.null(learner)) assert_learner(learner)
  assert_logical(scaling)
  assert_count(max_cardinality)

  cols_by_type = function(types) {
    assert_character(types)
    task$feature_types[get("type") %in% types,]
  }
  pos = list()

  if ("factor" %in% learner$feature_types %??% c() & nrow(cols_by_type("factor")) > 0)
    pos = c(pos, po("fixfactors", param_vals = list(affect_columns = is.factor)))

  if (!("missing" %in% (learner$properties %??% c()))) {
    # Impute numerics
    if (sum(task$missings(cols_by_type(c("numeric", "integer"))$id)) > 0)
      pos = c(pos, po("imputehist", param_vals = list(affect_columns = is.numeric)), po("missind"))

    # Impute factors
    if (sum(task$missings(cols_by_type(c("factor", "character"))$id)) > 0)
      pos = c(pos, po("imputenewlvl", param_vals = list(affect_columns = is.factor)))
  }

  if (!("factor" %in% learner$feature_types %??% c())) {
    # Collapse factors over 1000 levels
    if (any(map_lgl(task$levels(cols_by_type("factor")$id), function(x) length(x) > max_cardinality)))
      pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
    # Encode factors
    if (nrow(cols_by_type("factor")) > 0)
      pos = c(pos, po("encode"))
  }

  if (scaling) pos = c(pos, po("scale"))
  if (length(pos) == 0) return(po("nop"))
  else Reduce(`%>>%`, pos)
}

mlr_graphs$add("robustify_pipeline", robustify_pipeline)


#' @title Create a bagging learner
#'
#' @description
#' Creates a [`Graph`] that performs bagging on the supplied graph.
#'
#' @param graph [`PipeOp`]|[`Graph`] \cr
#'   A learner to create a robustifying pipeline for. Optional, if omitted,
#'   a more conservative pipeline is built.
#' @param iterations [`integer`] \cr
#'   Number of bagging iterations. Defaults to 10.
#' @param averager [`PipeOp`]|[`Graph`] \cr
#'   A [`PipeOp`] or [`Graph`] that averages the predictions from the
#'   replicated and subsampled graph's.
#'   In the simplest case, `po("classifavg")` and `po("regravg")` can be used
#'   in order to perform simple averaging of classification and regression
#'   predictions respectively.`
#' @return Returns a [`Graph`]
#' @export
#' @examples
#' library(mlr3)
#' lrn_po = po("learner", lrn("regr.rpart"))
#' task = mlr_tasks$get("boston_housing")
#' gr = bagging_pipeline(lrn_po, 3, averager = po("regravg"))
#' resample(task, GraphLearner$new(gr), rsmp("holdout"))
bagging_pipeline = function(graph, iterations = 10, averager = NULL) {
  assert_true(inherits(averager, "PipeOp") | inherits(averager, "Graph"))
  assert_count(iterations)

  subs = po("subsample", param_vals = list(frac = 0.7)) %>>% graph
  subs_repls = greplicate(subs, iterations)

  if (!is.null(averager)) {
    assert_true(inherits(averager, "PipeOp") | inherits(averager, "Graph"))
    subs_repls %>>% averager
  } else {
    return(subs_repls)
  }

}

mlr_graphs$add("bagging_pipeline", bagging_pipeline)
