#' @title PipeOpImputeConstant
#'
#' @usage NULL
#' @name mlr_pipeops_imputeconstant
#' @format [`R6Class`] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute features by a constant value. To select specific features use the `affect_columns`
#' parameter inherited from [`PipeOpImpute`].
#'
#' @section Construction:
#' ```
#' PipeOpImputeConstant$new(id = "imputeconstant", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"imputeconstant"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpImpute`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected features missing values imputed by
#' the value of the `constant` parameter.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpImpute`].
#'
#' The `$state$model` contains only `NULL` elements.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`], as well as:
#' * `constant` :: `atomic(1)`\cr
#'   The constant value that should be used for the imputation, atomic vector of length 1. The
#'   atomic mode must match the type of the features that will be selected by the `affect_columns`
#'   parameter and this will be checked during imputation. Default is `".MISSING"`.
#' * `check_levels` :: `logical(1)`\cr
#'   Should be checked whether the `constant` value is a valid level of factorial features (i.e., it
#'   already is a level)? Raises an error if unsuccesful. This check is only performed for factorial
#'   features (i.e., `factor`, `ordered`; skipped for `character`). Default is `TRUE`.
#'
#' @section Internals:
#' Adds an explicit new level to `factor` and `ordered` features, but not to `character` features if
#' `check_levels` is `FALSE`.
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
#' # impute missing values of the numeric feature "glucose" by the constant value -999
#' po = po("imputeconstant", param_vals = list(
#'   constant = -999, affect_columns = selector_name("glucose"))
#' )
#' new_task = po$train(list(task = task))[[1]]
#' new_task$missings()
#' new_task$data(cols = "glucose")[[1]]
#' @family PipeOps
#' @family Imputation PipeOps
#' @include PipeOpImpute.R
#' @export
PipeOpImputeConstant = R6Class("PipeOpImputeConstant",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "imputeconstant", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("constant", tags = c("train", "predict", "required"), custom_check = check_scalar),
        ParamLgl$new("check_levels", default = TRUE, tags = c("train", "predict", "required"))
      ))
      ps$values = list(constant = ".MISSING", check_levels = TRUE)
      super$initialize(id, param_set = ps, param_vals = param_vals)
    }
  ),
  private = list(

    .select_cols = function(task) task$feature_names,

    .train_imputer = function(feature, type, context) {
      NULL
    },

    .impute = function(feature, type, model, context) {
      constant = self$param_set$values$constant
      switch(type,
        "logical"   = assert_flag(constant),
        "integer"   = assert_int(constant),
        "numeric"   = assert_number(constant),
        "character" = assert_string(constant),
        "factor"    = assert_string_or_factor(constant),
        "ordered"   = assert_string_or_factor(constant),
        "POSIXct"   = assert_posixct(constant, any.missing = FALSE, len = 1L)
      )

      if (type == "integer") {
        constant = as.integer(constant)
      }

      if (type %in% c("ordered", "factor")) {
        constant = as.factor(constant)
        if (self$param_set$values$check_levels) {
          assert_subset(levels(constant), choices = levels(feature))
        }
        levels(feature) = c(levels(feature), levels(constant)[constant])
      }

      feature[is.na(feature)] = constant
      feature
    }
  )
)

mlr_pipeops$add("imputeconstant", PipeOpImputeConstant)

check_string_or_factor = function(x) {
  if (test_string(x) || test_factor(x, len = 1L)) TRUE else sprintf("Must be of type 'string' or 'factor'")
}

assert_string_or_factor = makeAssertionFunction(check_string_or_factor)
