#' @title PipeOpImputeOutOfRange
#'
#' @usage NULL
#' @name mlr_pipeops_imputeoor
#' @format [`R6Class`] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute numerical features by constant values shifted below the minimum or above the maximum by
#' using \eqn{min(x) - offset - multiplier * diff(range(x))} or
#' \eqn{max(x) + offset + multiplier * diff(range(x))}.
#'
#' This type of imputation is especially sensible in the context of tree-based methods, see also
#' Ding & Simonoff (2010).
#'
#' @section Construction:
#' ```
#' PipeOpImputeOutOfRange$new(id = "imputeoor", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"imputeoor"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpImpute`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features missing values
#' imputed as described above.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpImpute`].
#'
#' The `$state$model` is a named `list` of `numeric(1)` indicating the constant value used for
#' imputation.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`], as well as:
#' * `min` :: `logical(1)` \cr
#'   Should the features be shifted below the minimum? Default is TRUE. If FALSE they are shifted
#'   above the maximum. See also the description above.
#' * `offset` :: `numeric(1)` \cr
#'   Numerical non-negative offset as used in the description above. Default is 1.
#' * `multiplier` :: `numeric(1)` \cr
#'   Numerical non-negative multiplier as used in the description above. Default is 1.
#'
#' @section Internals:
#' Uses the `min`, `max`, `diff` and `range` functions. Features that are entirely `NA` are imputed
#' as `0`.
#'
#' For details in the context of tree-based methods see:\cr
#' Ding, Y. and Simonoff, J. S. 2010.\cr
#' An Investigation of Missing Data Methods for Classification Trees Applied to Binary Response
#' Data.\cr
#' Journal of Machine Learning Research. 11, 131-170.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("pima")
#' task$missings()
#'
#' po = po("imputeoor")
#' new_task = po$train(list(task = task))[[1]]
#' new_task$missings()
#'
#' po$state$model
#' @family PipeOps
#' @family Imputation PipeOps
#' @include PipeOpImpute.R
#' @export
PipeOpImputeOutOfRange = R6Class("PipeOpImputeOutOfRange",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "imputeoor", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamLgl$new("min", default = TRUE, tags = c("train", "predict")),
        ParamDbl$new("offset", lower = 0, default = 1, tags = c("train", "predict")),
        ParamDbl$new("multiplier", lower = 0, default = 1, tags = c("train", "predict")))
      )
      ps$values = list(min = TRUE, offset = 1, multiplier = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals)
    }
  ),
  private = list(
    .select_cols = function(task) task$feature_types[get("type") %in% c("numeric", "integer"), get("id")],

    .train_imputer = function(feature, type, context) {
      oor = if(self$param_set$values$min) {
        min(feature, na.rm = TRUE) - self$param_set$values$offset - self$param_set$values$multiplier * diff(range(feature, na.rm = TRUE))
      } else {
        max(feature, na.rm = TRUE) + self$param_set$values$offset + self$param_set$values$multiplier * diff(range(feature, na.rm = TRUE))
      }

      if (is.nan(oor)) {
        oor = 0
      }

      if (type == "integer") {
        oor = as.integer(round(oor))
      }
      oor
    },

    .impute = function(feature, type, model, context) {
      feature[is.na(feature)] = model
      feature
    }
  )
)

mlr_pipeops$add("imputeoor", PipeOpImputeOutOfRange)
