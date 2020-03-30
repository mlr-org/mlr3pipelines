#' @title PipeOpPredPostproc
#' 
#' @format [`R6Class`] object inheriting from [`PipeOpPredPostproc`]/[`PipeOp`].
#'
#' @description
#' Abstract base class for handling most 'postprocessing' operations on predictions.
#' These are operations that have exactly one prediction object as input and one
#' prediction object as output.
#'
#' Users must implement `$train()` and `$predict()`, which have a [`Prediction`][mlr3::Prediction]
#' input and should return that [`Prediction`][mlr3::Prediction]. 
#' The [`Prediction`][mlr3::Prediction] should, if possible, be manipulated in-place,
#' and should not be cloned.
#'
#' @section Construction:
#' ```
#' * `PipeOpPredPostproc$new(id = "predpostproc", param_set = ParamSet$new())` \cr
#'   (`character(1)`, `ParamSet`, `logical(1)`) -> `self` \cr
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. Default: "predpostproc".
#' * `param_set` :: [`ParamSet`][paradox::ParamSet] | `list` of `expression`\cr
#'   Parameter space description. This should be created by the subclass and given to `super$initialize()`.
#'   If this is a [`ParamSet`][paradox::ParamSet], it is used as the [`PipeOp`]'s [`ParamSet`][paradox::ParamSet]
#'   directly. Otherwise it must be a `list` of expressions e.g. created by `alist()` that evaluate to [`ParamSet`][paradox::ParamSet]s.
#'   These [`ParamSet`][paradox::ParamSet] are combined using a [`ParamSetCollection`][paradox::ParamSetCollection].
#'   Default: ParamSet$new()
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Inputs are of type [`Task`][mlr3::Task] during train and predict.
#' Outputs are `NULL` during train and [`Prediction`][mlr3::Prediction] during predict.
#'
#' @section State:
#' The `$state` is a named `list` with elements
#' * `thresholds` :: `character` learned thresholds
#'
#' @section Parameters:
#'  Parameters inherited by [`PipeOp`].
#' 
#' @section Fields:
#' Fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#' 
#' @usage NULL
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpPredPostproc = R6Class("PipeOpPredPostproc",
  inherit = PipeOp,

  public = list(
    threshold = NULL,
    measure = NULL,
    initialize = function(id = "predpostproc", param_set = ParamSet$new(), param_vals = list(), packages = character(0)) {
      super$initialize(id, param_set = param_set, param_vals = param_vals, packages = packages,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction"),
        tags = "predpostproc"
      )
    },
    train = function(input) {
      self$state = list()
      list(NULL)
    },
    predict = function(input) stop("abstract")
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("predpostproc", PipeOpPredPostproc)


