#' @title PipeOpImputeNewlvl
#'
#' @usage NULL
#' @name mlr_pipeops_imputenewlvl
#' @format [`R6Class`] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute factorial features by adding a new level `".MISSING"`.\cr
#'
#' Impute numerical features by constant values shifted below the minimum or above the maximum by
#' using \eqn{min(x) - offset - multiplier * diff(range(x))} or
#' \eqn{max(x) + offset + multiplier * diff(range(x))}.\cr
#'
#' This type of imputation is especially sensible in the context of tree-based methods, see also
#' Ding & Simonoff (2010).
#'
#' @section Construction:
#' ```
#' PipeOpImputeNewlvl$new(id = "imputenewlvl", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"imputenewlvl"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpImpute`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected features having missing values
#' imputed as described above.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpImpute`].
#'
#' The `$state$model` contains either `".MISSING"` used for `character` and `factor` (also
#' `ordered`) features or `numeric(1)` indicating the constant value used for imputation of
#' `integer` and `numeric` features.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`], as well as:
#' * `min` :: `logical(1)` \cr
#'   Should `integer` and `numeric` features be shifted below the minimum? Default is TRUE. If FALSE
#'   they are shifted above the maximum. See also the description above.
#' * `offset` :: `numeric(1)` \cr
#'   Numerical non-negative offset as used in the description above for `integer` and `numeric`
#'   features. Default is 1.
#' * `multiplier` :: `numeric(1)` \cr
#'   Numerical non-negative multiplier as used in the description above for `integer` and `numeric`
#'   features. Default is 1.
#'
#' @section Internals:
#' Adds an explicit new `level()` to `factor` and `ordered` features, but not to `character`
#' features. For `integer` and `numeric` features uses the `min`, `max`, `diff` and `range` functions.
#' `integer` and `numeric` features that are entirely `NA` are imputed as `0`.

#'
#' @section Methods:
#' Only methods inherited from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#' set.seed(2409)
#' data = tsk("pima")$data()
#' data$y = factor(c(NA, sample(letters, size = 766, replace = TRUE), NA))
#' data$z = ordered(c(NA, sample(1:10, size = 767, replace = TRUE)))
#' task = TaskClassif$new("task", backend = data, target = "diabetes")
#' task$missings()
#' po = po("imputenewlvl")
#' new_task = po$train(list(task = task))[[1]]
#' new_task$missings()
#' new_task$data()
#' @family PipeOps
#' @family Imputation PipeOps
#' @include PipeOpImpute.R
#' @export
PipeOpImputeNewlvl = R6Class("PipeOpImputeNewlvl",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "imputenewlvl", param_vals = list()) {
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

    # this is one of the few imputers that handles 'character' features!
    .select_cols = function(task) task$feature_types[get("type") %in% c("character", "factor", "integer", "numeric", "ordered"), get("id")],

    .train_imputer = function(feature, type, context) {
      if (type %in% c("factor", "ordered", "character")) {
        return(".MISSING")  # early exit
      }

      # for integer or numeric do
      oor = if(self$param_set$values$min) {
        min(feature, na.rm = TRUE) - self$param_set$values$offset - self$param_set$values$multiplier * diff(range(feature, na.rm = TRUE))
      } else {
        max(feature, na.rm = TRUE) + self$param_set$values$offset + self$param_set$values$multiplier * diff(range(feature, na.rm = TRUE))
      }

      # if the feature only consists of NA/NaN values, oor will be NaN if offset and multiplier are 0;
      # oor will be Inf if offset and multiplier are larger than 0
      if (is.nan(oor) || is.infinite(oor)) {
        oor = 0
      }

      if (type == "integer") {
        oor = as.integer(round(oor))
      }

      oor
    },

    .impute = function(feature, type, model, context) {
      if (type %in% c("factor", "ordered")) {
        levels(feature) = c(levels(feature), ".MISSING")
      }
      feature[is.na(feature)] = model
      feature
    }
  )
)

mlr_pipeops$add("imputenewlvl", PipeOpImputeNewlvl)
