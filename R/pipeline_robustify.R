#' @include mlr_graphs.R

#' @title Robustify a learner
#' @name mlr_graphs_robustify
#' @description
#' Creates a [`Graph`] that can be used to robustify any subsequent learner.
#' Performs the following steps:
#' * Drops empty factor levels using [`PipeOpFixFactors`]
#' * Imputes `numeric` features using [`PipeOpImputeHist`] and [`PipeOpMissInd`]
#' * Imputes `factor` features using [`PipeOpImputeOOR`]
#' * Encodes `factors` using `one-hot-encoding`. Factors with a cardinality > max_cardinality are
#'   collapsed using [`PipeOpCollapseFactors`]
#'
#' The graph is built conservatively, i.e. the function always tries to assure everything works.
#' If a learner is provided, some steps can be left out, i.e. if the learner can deal with
#' factor variables, no encoding is performed.
#'
#' All input arguments are cloned and have no references in common with the returned [`Graph`].
#'
#' @param task [`Task`] \cr
#'   A [`Task`][mlr3::Task] to create a robustifying pipeline for.
#'   Optional, if omitted, the full pipeline is created.
#' @param learner [`Learner`][mlr3::Learner] \cr
#'   A learner to create a robustifying pipeline for. Optional, if omitted,
#'   a more conservative pipeline is built.
#' @param impute_missings `logical(1)` | `NULL` \cr
#'   Should missing values be imputed? Defaults to `NULL`, i.e. imputes if the task has
#'   missing values and the learner can not handle them.
#' @param factors_to_numeric `logical(1)` | `NULL` \cr
#'   Should factors be encoded? Defaults to `NULL`, i.e. encodes if the task has factors
#'   and the learner can not handle factors.
#' @param max_cardinality `integer(1)` \cr
#'   Maximum number of factor levels allowed. See above. Default: 1000.
#' @return [`Graph`]
#' @export
#' @examples
#' \donttest{
#' library(mlr3)
#' lrn = lrn("regr.rpart")
#' task = mlr_tasks$get("boston_housing")
#' gr = pipeline_robustify(task, lrn) %>>% po("learner", lrn)
#' resample(task, GraphLearner$new(gr), rsmp("holdout"))
#' }
pipeline_robustify = function(task = NULL, learner = NULL, impute_missings = NULL,
  factors_to_numeric = NULL, max_cardinality = 1000) {

  has_type_feats = function(types, if_null = TRUE) {
    if (is.null(task)) if_null else any(types %in% task$feature_types$type)
  }
  if (!is.null(task)) assert_task(task)
  if (!is.null(learner)) assert_learner(learner)
  if (is.null(impute_missings)) impute_missings = is.null(task) || (any(task$missings()) && (is.null(learner) || "missings" %nin% learner$properties))
  assert_flag(impute_missings)
  if (is.null(factors_to_numeric)) factors_to_numeric = is.null(task) || (has_type_feats("factor") && (is.null(learner) || "factor" %nin% learner$feature_types))
  assert_flag(impute_missings)
  assert_count(max_cardinality)

  # If given a task, only treat actually existing column types
  pos = list()

  # FIXME: Improve this when text-processors are available. See #332 and friends
  if (has_type_feats("character") && "character" %nin% learner$feature_types) {
    pos = c(pos, po("colapply", id = "char_to_fct", param_vals = list(affect_columns = selector_type("character"), applicator = function(x) as.factor(x))))
  }

  # Date processing
   if (has_type_feats("POSIXct") && ("POSIXct" %nin% learner$feature_types)) {
     pos = c(pos, po("datefeatures", param_vals = list(affect_columns = selector_type("POSIXct"))))
   }

  if (impute_missings) {
    # Impute numerics
    if (has_type_feats(c("numeric", "integer"))) {
      type = if (is.null(learner)) {
        "factor"  # default to factor as in PipeOpMissInd anyways
      } else {
        types = c("factor", "integer", "logical", "numeric")
        type_candidates = intersect(types, learner$feature_types)
        if (length(type_candidates) == 0L) {
          stopf("Learner %s does not support any of the feature types needed for missing indicator columns: %s.",
            learner$id, paste0(types, collapse = ", "))
        } else {
          type_candidates[[1]]  # just take the first supported type
        }
      }
      pos = c(pos,
        gunion(list(
          po("imputehist"),
          po("missind", param_vals = list(affect_columns = selector_type(c("numeric", "integer")), type = type)))) %>>%
        po("featureunion"))
    }
    # Impute factors
    if (has_type_feats(c("factor", "ordered", "character"))) {
      pos = c(pos, po("imputeoor"))
    }
  }

  # Fix extra factor levels
  if (has_type_feats(c("factor", "ordered"))) {
    pos = c(pos, po("fixfactors"))
  }

  # Ensure all factor levels are encoded during predict
  if (impute_missings && has_type_feats(c("factor", "ordered", "character"))) {
    pos = c(pos, po("imputesample", affect_columns = selector_type(c("factor", "ordered", "character"))))
  }

  # Collapse factors over 1000 levels
  # FIXME: Can be improved after #330 is solved
  if (is.null(task)) {
    pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
  } else {
    if (any(map_lgl(task$levels(task$feature_types$id[task$feature_types$type == "factor"]), function(x) length(x) > max_cardinality))) {
      pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
    }
  }

  if (factors_to_numeric) pos = c(pos, po("encode"))
  pos = c(pos, po("removeconstants"))
  chain_graphs(pos, in_place = TRUE)
}

mlr_graphs$add("robustify", pipeline_robustify)
