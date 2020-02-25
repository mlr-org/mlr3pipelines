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
#'   Should the data be scaled? Defaults to `FALSE`.
#' @param max_cardinality [`integer`] \cr
#'   Maximum number of factor levels allowed. See above. Default: 1000.
#' @return [`Graph`]
#' @export
#' @examples
#' library(mlr3)
#' lrn = lrn("regr.rpart")
#' task = mlr_tasks$get("boston_housing")
#' gr = robustify_pipeline(task, lrn) %>>% po("learner", lrn)
#' resample(task, GraphLearner$new(gr), rsmp("holdout"))
robustify_pipeline = function(task = NULL, learner = NULL, impute_missings = NULL,
  factors_to_numeric = NULL, scaling = FALSE, max_cardinality = 1000) {

  cols_by_type = function(types) {
    if (is.null(task)) return(data.table("id" = character(), "type" = character()))
    task$feature_types[get("type") %in% types,]
  }
  if (!is.null(task)) assert_task(task)
  if (!is.null(learner)) assert_learner(learner)
  if (is.null(impute_missings)) impute_missings = (is.null(task) || any(task$missings())) && (is.null(learner) || !"missings" %in% learner$properties)
  assert_flag(impute_missings)
  if (is.null(factors_to_numeric)) factors_to_numeric = (is.null(task) || nrow(cols_by_type("factor")) > 0) && (is.null(learner) || !"factors" %in% learner$properties)
  assert_flag(impute_missings)
  assert_flag(scaling)
  assert_count(max_cardinality)

  # If given a task, only treat actually existing column types
  pos = list()

  # FIXME: Improve this when text-processors are available. See #332 and friends
  if (is.null(task) || (nrow(cols_by_type(c("character"))) > 0 && "character" %nin% lrn$feature_types))
    pos = c(pos, po("colapply", id = "char_to_fct", param_vals = list(affect_columns = selector_type("character"), applicator = function(x) as.factor(x))))

  # FIXME: Uncomment once #308 is merged
  # if (is.null(task) || (nrow(cols_by_type(c("POSIXct"))) > 0 && "POSIXct" %nin% lrn$feature_types))
  #   pos = c(pos, po("datefeatures", param_vals = list(affect_columns = selector_type("POSIXct"))))

  if (impute_missings) {
    # Impute numerics
    if (is.null(task) || nrow(cols_by_type(c("numeric", "integer"))) > 0)
      pos = c(pos,
        gunion(list(
          po("imputehist"),
          po("missind", param_vals = list(affect_columns = selector_type(c("numeric", "integer")))))) %>>%
        po("featureunion"))
    # Impute factors
    if (is.null(task) || nrow(cols_by_type(c("factor", "ordered", "character"))) > 0)
      pos = c(pos, po("imputenewlvl"))
  }

  # Fix extra factor levels
  if (is.null(task) || nrow(cols_by_type(c("factor", "ordered", "character"))) > 0)
    pos = c(pos, po("fixfactors"))

  # Ensure all factor levels are encoded during predict.
  if (impute_missings && (is.null(task) || nrow(cols_by_type(c("factor", "ordered"))) > 0))
    pos = c(pos, po("imputesample", affect_columns = selector_type(c("factor", "ordered"))))

  # Collapse factors over 1000 levels
  # FIXME: Can be improved after #330 is solved
  if (is.null(task)) {
    pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
  } else {
    if (any(map_lgl(task$levels(cols_by_type("factor")$id), function(x) length(x) > max_cardinality)))
      pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
  }

  if (factors_to_numeric) pos = c(pos, po("encode"))

  pos = c(pos, po("removeconstants"))
  if (scaling) pos = c(pos, po("scale"))

  as_graph(Reduce(`%>>%`, pos))
}

mlr_graphs$add("robustify", robustify_pipeline)


#' @title Create a bagging learner
#' @name mlr_graphs_bagging
#' @description
#' Creates a [`Graph`] that performs bagging for a supplied graph.
#' This is done as follows:
#' * `Subsample` the data in each step using [`PipeOpSubsample`]
#' * Replicate this step `iterations` times (in parallel)
#' * Average resulting predictions using the `averager`.
#'
#' @param graph [`PipeOp`] | [`Graph`] \cr
#'   A learner to create a robustifying pipeline for. Optional, if omitted,
#'   a more conservative pipeline is built.
#' @param iterations [`integer`] \cr
#'   Number of bagging iterations. Defaults to 10.
#' @param frac [`numeric`] \cr
#'   Percentage of rows to keep during subsampling. See [`PipeOpSubsample`] for
#'   more information. Defaults to 0.7.
#' @param averager [`PipeOp`] | [`Graph`] \cr
#'   A [`PipeOp`] or [`Graph`] that averages the predictions from the
#'   replicated and subsampled graph's.
#'   In the simplest case, `po("classifavg")` and `po("regravg")` can be used
#'   in order to perform simple averaging of classification and regression
#'   predictions respectively.`
#'   If `NULL` (default), no averager is added to the end of the graph.
#' @return [`Graph`]
#' @export
#' @examples
#' library(mlr3)
#' lrn_po = po("learner", lrn("regr.rpart"))
#' task = mlr_tasks$get("boston_housing")
#' gr = bagging_pipeline(lrn_po, 3, averager = po("regravg"))
#' resample(task, GraphLearner$new(gr), rsmp("holdout"))
bagging_pipeline = function(graph, iterations = 10, frac = 0.7, averager = NULL) {
  assert_count(iterations)
  assert_number(frac, lower = 0, upper = 1)
  graph = as_graph(graph)
  if (!is.null(averager)) {
    averager = as_graph(averager)
    assert_true(graph$output$train == averager$input$train | averager$input$train == "NULL")
    assert_true(graph$output$predict == averager$input$predict)
  }

  subs = po("subsample", param_vals = list(frac = frac)) %>>% graph
  subs_repls = greplicate(subs, iterations)

  if (!is.null(averager)) subs_repls %>>% averager else subs_repls
}

mlr_graphs$add("bagging", bagging_pipeline)
