#' @title PipeOpSelect
#'
#' @usage NULL
#' @name mlr_pipeops_select
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Removes features from [`Task`][mlr3::Task] depending on a [`Selector`] function:
#' The `selector` parameter gives the features to keep.
#' See [`Selector`] for selectors that are provided and how to write custom [`Selector`]s.
#'
#' @section Construction:
#' ```
#' PipeOpSelect$new(id = "select", param_vals = list())
#' ```
#" * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"select"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with features removed that were not selected by the [`Selector`]/`function` in `selector`.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `selection` :: `character`\cr
#'   A vector of all feature names that are kept (i.e. not dropped) in the [`Task`][mlr3::Task]. Initialized to [`selector_all()`]
#'
#' @section Parameters:
#' * `selector` :: `function` | [`Selector`] \cr
#'   [`Selector`] function, takes a `Task` as argument and returns a `character`
#'   of features to keep.\cr
#'   See [`Selector`] for example functions. Defaults to `selector_all()`.
#'
#' @section Internals:
#' Uses `task$select()`.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' pos = mlr_pipeops$get("select")
#'
#' pos$param_set$values$selector = selector_all()
#' pos$train(list("boston_housing"))[[1]]$feature_names
#'
#' pos$param_set$values$selector = selector_type("factor")
#' pos$train(list("boston_housing"))[[1]]$feature_names
#'
#' pos$param_set$values$selector = selector_invert(selector_type("factor"))
#' pos$train(list("boston_housing"))[[1]]$feature_names
#'
#' pos$param_set$values$selector = selector_grep("^Petal")
#' pos$train(list("iris"))[[1]]$feature_names
#'
#' @family PipeOps
#' @family Selectors
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpSelect = R6Class("PipeOpSelect",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "select", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("selector", custom_check = check_function, tags = c("train", "required"))
      ))
      ps$values = list(selector = selector_all())
      super$initialize(id, ps, param_vals = param_vals)
    },

    get_state = function(task) {
      selection = self$param_set$values$selector(task)
      assert_subset(selection, task$feature_names)
      list(selection = selection)
    },

    transform = function(task) {
      task$select(self$state$selection)
    }
  )
)

mlr_pipeops$add("select", PipeOpSelect)
