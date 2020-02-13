#' @include mlr_graphs.R

#' @title Robustify a learner
#' @name mlr_graphs_robustify
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
#'   Optional, if omitted, the full pipeline is created.
#' @param learner [`Learner`] \cr
#'   A learner to create a robustifying pipeline for. Optional, if omitted,
#'   a more conservative pipeline is built.
#' @param impute_missings [`logical`] | [`NULL`] \cr
#'   Should missing values be imputed? Defaults to `NULL`, i.e imputes if the task has
#'   missing values and the learner can not handle them.
#' @param factors_to_numeric [`logical`] | [`NULL`] \cr
#'   Should factors be encoded? Defaults to `NULL`, i.e encodes if the task has factors
#'   and the learner can not handle factors.
#' @param scaling [`logical`] \cr
#'   Should the data be scaled? Defaults to true.
#' @param max_cardinality [`integer`] \cr
#'   Maximum number of factor levels allowed. See above. Default: 1000.
#' @return [`Graph`]
#' @export
#' @examples
#' library(mlr3)
#' lrn = lrn("regr.rpart")
#' task = mlr_tasks$get("boston_housing")
#' gr = robustify_pipeline(task, lrn) %>>% po("learner", lrn)
#' resample(task,GraphLearner$new(gr), rsmp("holdout"))
robustify_pipeline = function(task = NULL, learner = NULL, impute_missings = NULL,
  factors_to_numeric = NULL, scaling = TRUE, max_cardinality = 1000) {

  cols_by_type = function(types) {
    task$feature_types[get("type") %in% types,]
  }
  if (!is.null(task)) assert_task(task)
  if (!is.null(learner)) assert_learner(learner)
  if (is.null(impute_missings)) impute_missings = (is.null(task) || any(task$missings())) && (is.null(learner) || !"missings" %in% learner$properties)
  else assert_flag(impute_missings)
  if (is.null(factors_to_numeric)) factors_to_numeric = (is.null(task) || nrow(cols_by_type("factor")) > 0) && (is.null(learner) || !"factors" %in% learner$properties)
  else assert_flag(impute_missings)
  assert_flag(scaling)
  assert_count(max_cardinality)


  if (!is.null(task)) {
    pos = list()
    if ("factor" %in% learner$feature_types && nrow(cols_by_type("factor")) > 0)
      pos = c(pos, po("fixfactors", param_vals = list(affect_columns = is.factor)))

    if (impute_missings) {
      # Impute numerics
      if (nrow(cols_by_type(c("numeric", "integer"))) > 0)
        pos = c(pos, po("copy", 2) %>>%
          gunion(list(
            po("imputehist", param_vals = list(affect_columns = is.numeric)),
            po("missind"))) %>>%
           po("featureunion"))
      # Impute factors
      if (nrow(cols_by_type(c("factor", "ordered"))) > 0)
        pos = c(pos, po("imputenewlvl", param_vals = list(affect_columns = is.factor)))
    }

    if (factors_to_numeric) {
      # Collapse factors over 1000 levels
      if (any(map_lgl(task$levels(cols_by_type("factor")$id), function(x) length(x) > max_cardinality)))
        pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
      # Encode factors
      pos = c(pos, po("encode"))
    }
  } else {
    pos = list(po("fixfactors", param_vals = list(affect_columns = is.factor)))
    if (impute_missings) {
      pos = c(pos,
        po("copy", 2) %>>%
          gunion(list(
            po("imputehist", param_vals = list(affect_columns = is.numeric)),
            po("missind"))) %>>%
           po("featureunion"),
        po("imputenewlvl", param_vals = list(affect_columns = is.factor)))
    }
    pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
    if (factors_to_numeric) pos = c(pos, po("encode"))
  }

  if (scaling) pos = c(pos, po("scale"))

  if (length(pos) == 0) as_graph(po("nop"))
  else Reduce(`%>>%`, pos)
}

mlr_graphs$add("robustify", robustify_pipeline)


#' @title Create a bagging learner
#' @name mlr_graphs_bagging
#' @description
#' Creates a [`Graph`] that performs bagging on the supplied graph.
#'
#' @param graph [`PipeOp`] | [`Graph`] \cr
#'   A learner to create a robustifying pipeline for. Optional, if omitted,
#'   a more conservative pipeline is built.
#' @param iterations [`integer`] \cr
#'   Number of bagging iterations. Defaults to 10.
#' @param averager [`PipeOp`] | [`Graph`] \cr
#'   A [`PipeOp`] or [`Graph`] that averages the predictions from the
#'   replicated and subsampled graph's.
#'   In the simplest case, `po("classifavg")` and `po("regravg")` can be used
#'   in order to perform simple averaging of classification and regression
#'   predictions respectively.`
#' @return [`Graph`]
#' @export
#' @examples
#' library(mlr3)
#' lrn_po = po("learner", lrn("regr.rpart"))
#' task = mlr_tasks$get("boston_housing")
#' gr = bagging_pipeline(lrn_po, 3, averager = po("regravg"))
#' resample(task, GraphLearner$new(gr), rsmp("holdout"))
bagging_pipeline = function(graph, iterations = 10, averager = NULL) {
  assert_count(iterations)

  subs = po("subsample", param_vals = list(frac = 0.7)) %>>% as_graph(graph)
  subs_repls = greplicate(subs, iterations)

  if (!is.null(averager)) subs_repls %>>% as_graph(averager)
  else subs_repls
}

mlr_graphs$add("bagging", bagging_pipeline)
