#' @title Impute Features by a Constant
#'
#' @usage NULL
#' @name mlr_pipeops_imputeconstant
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute features by a constant value.
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
#' The `$state$model` contains the value of the `constant` parameter that is used for imputation.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`], as well as:
#' * `constant` :: `atomic(1)`\cr
#'   The constant value that should be used for the imputation, atomic vector of length `1`. The atomic mode must match
#'   the type of the features that will be selected by the `affect_columns` parameter and this will be checked during
#'   imputation. This is a required hyperparameter and needs to be set by the user.
#' * `check_levels` :: `logical(1)`\cr
#'   Should be checked whether the `constant` value is a valid level of factorial features (i.e., it already is a
#'   level)? Raises an error if unsuccessful. This check is only performed for factorial features (i.e., `factor`,
#'   `ordered`; skipped for `character`). Initialized to `TRUE`.\cr
#'   Note that empty factor levels can be a problem for many [`Learners`][mlr3::Learner]. Thus, [`PipeOpImputeOOR`] is
#'   the preferred choice for creating new levels, since it is designed to impute out-of-range values and offers a more
#'   explicit control for handling potentially problematic behavior.
#'
#' @section Internals:
#' The constructor is called with `empty_level_control` set to `"always"`, to allow the creation of a new empty level
#' for `factor` and `ordered` (but not `character`) features during training, if `constant` is not an already existing
#' level and `check_levels` is set to `FALSE`. This has no impact if `check_levels` is `TRUE`, since in that case an
#' error would be raised before imputation.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
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
#'
#' @family PipeOps
#' @family Imputation PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpImpute.R
#' @export
PipeOpImputeConstant = R6Class("PipeOpImputeConstant",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "imputeconstant", param_vals = list()) {
      ps = ps(
        constant = p_uty(tags = c("train", "required"), custom_check = check_scalar),
        check_levels = p_lgl(init = TRUE, tags = c("train", "required"))
      )
      super$initialize(id, param_set = ps, param_vals = param_vals, empty_level_control = "always",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered", "POSIXct", "Date"))
    }
  ),
  private = list(
    .train_imputer = function(feature, type, context) {
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
      if (type %in% c("ordered", "factor") && self$param_set$values$check_levels) {
        if (!isTRUE(check_choice(as.character(constant), levels(feature)))) {
          stopf("Constant '%s' is not an existing level of feature with levels {%s}, but hyperparameter 'check_levels' is TRUE.",
                constant, str_collapse(levels(feature), quote = "'"))
        }
      }
      if (type == "integer") {
        constant = as.integer(constant)
      }
      constant
    },

    .train_nullmodel = function(feature, type, context) private$.train_imputer(feature, type, context)
  )
)

mlr_pipeops$add("imputeconstant", PipeOpImputeConstant)

assert_string_or_factor = function(x) assert(check_string(x), check_factor(x, len = 1, any.missing = FALSE))
