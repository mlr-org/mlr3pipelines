#' @title PipeOpSelect
#'
#' @name mlr_pipeop_select
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`].
#'
#' @description
#' Removes features from `Task` depending on a selector function. If `invert`
#' is `FALSE` (default) then `selector` gives the features to keep, otherwise it gives
#' the features to drop. See [`Selector`] for selectors that are provided.
#'
#' @section Parameter Set:
#' * `selector` :: `function` \cr
#'   Selector function, takes a `Task` as argument and returns a `character`
#'   of features to keep. See [`Selector`] for example functions. Defaults to
#'   `selector_all()`.
#' * `invert`    :: `logical(1)` \cr
#'   Invert selection. If this is `TRUE`, the features selected by
#'   `selector` are *removed* while the ones not selected remain.
#' @family PipeOps
#' @family Selectors
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpSelect = R6Class("PipeOpSelect",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "select", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("selector", custom_check = check_function, tags = "required"),
        ParamLgl$new("invert", tags = "required")
      ))
      ps$values = list(selector = selector_all(), invert = FALSE)
      super$initialize(id, ps, param_vals = param_vals)
    },

    get_state = function(task) {
      selection = self$param_set$values$selector(task)
      assert_subset(selection, task$feature_names)
      if (self$param_set$values$invert) {
        selection = setdiff(task$feature_names, selection)
      }
      list(selection = selection) # we don't use 'scores', but maybe the user cares.
    },

    transform = function(task) {
      task$select(self$state$selection)
    }
  )
)

mlr_pipeops$add("select", PipeOpSelect)
