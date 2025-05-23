#' @title Add Missing Indicator Columns
#'
#' @usage NULL
#' @name mlr_pipeops_missind
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Add missing indicator columns ("dummy columns") to the [`Task`][mlr3::Task].
#' Drops original features; should probably be used in combination with [`PipeOpFeatureUnion`] and imputation [`PipeOp`]s (see examples).
#'
#' Note the `affect_columns` is initialized with `selector_invert(selector_type(c("factor", "ordered", "character")))`, since missing
#' values in factorial columns are often indicated by out-of-range imputation ([`PipeOpImputeOOR`]).
#'
#' @section Construction:
#' ```
#' PipeOpMissInd$new(id = "missind", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, defaulting to `"missind"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section State:
#' `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `indicand_cols` :: `character`\cr
#'   Names of columns for which indicator columns are added. If the `which` parameter is `"all"`, this is just the names of all features,
#'   otherwise it is the names of all features that had missing values during training.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOpTaskPreproc`], as well as:
#' * `which` :: `character(1)`\cr
#'   Determines for which features the indicator columns are added. Can either be `"missing_train"` (default), adding indicator columns
#'   for each feature that actually has missing values, or `"all"`, adding indicator columns for all features.
#' * `type` :: `character(1)`\cr
#'   Determines the type of the newly created columns. Can be one of `"factor"` (default), `"integer"`, `"logical"`, `"numeric"`.
#'
#' @section Internals:
#' This [`PipeOp`] should cover most cases where "dummy columns" or "missing indicators" are desired. Some edge cases:
#' * If imputation
#'   for factorial features is performed and only numeric features should gain missing indicators, the `affect_columns` parameter
#'   can be set to `selector_type("numeric")`.
#' * If missing indicators should only be added for features that have more than a fraction of `x` missing values, the
#'   [`PipeOpRemoveConstants`] can be used with `affect_columns = selector_grep("^missing_")` and `ratio = x`.
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [`PipeOpTaskPreprocSimple`]([`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#' \dontshow{data.table::setDTthreads(1)}
#'
#' task = tsk("pima")$select(c("insulin", "triceps"))
#' sum(complete.cases(task$data()))
#' task$missings()
#' tail(task$data())
#'
#' po = po("missind")
#' new_task = po$train(list(task))[[1]]
#'
#' tail(new_task$data())
#'
#' # proper imputation + missing indicators
#'
#' impgraph = list(
#'   po("imputesample"),
#'   po("missind")
#' ) %>>% po("featureunion")
#'
#' tail(impgraph$train(task)[[1]]$data())
PipeOpMissInd = R6Class("PipeOpMissInd",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "missind", param_vals = list()) {
      ps = ps(
        which = p_fct(levels = c("missing_train", "all"), tags = c("train", "required")),
        type = p_fct(levels = c("factor", "integer", "logical", "numeric"), tags = c("train", "predict", "required"))
      )
      ps$values = list(which = "missing_train", type = "factor")
      super$initialize(id, ps, param_vals = param_vals, tags = "missings")
      if ("affect_columns" %nin% names(param_vals)) {
        # can't put this in `ps$values` because it is a PipeOpTaskPreproc param
        self$param_set$values$affect_columns = selector_invert(selector_type(c("factor", "ordered", "character")))
      }
    }
  ),
  private = list(

    .get_state = function(task) {
      if (self$param_set$values$which == "all") {
        # 'which' is just all feature names
        indicand_cols = task$feature_names
      } else {
        # 'which' is the feature names of all features that have missing values
        indicand_cols = task$feature_names[map_lgl(task$data(cols = task$feature_names),
          function(x) anyMissing(x))]
      }
      list(indicand_cols = indicand_cols)
    },

    .transform = function(task) {
      if (!length(self$state$indicand_cols)) {
        # need to handle this as special case because cbind for empty tasks is broken
        return(task$select(character(0)))
      }
      data_dummy = as.data.table(is.na(task$data(cols = self$state$indicand_cols)))
      data_dummy = switch(self$param_set$values$type,
        factor = data_dummy[, lapply(.SD, function(x) factor(ifelse(x, "missing", "present"), levels = c("missing", "present")))],
        integer = data_dummy[, lapply(.SD, as.integer)],
        logical = data_dummy,
        numeric = data_dummy[, lapply(.SD, as.numeric)],
        stop("Invalid value of 'type' parameter"))
      colnames(data_dummy) = paste0("missing_", colnames(data_dummy))
      task$select(character(0))$cbind(data_dummy)
    }
  )
)

mlr_pipeops$add("missind", PipeOpMissInd)
