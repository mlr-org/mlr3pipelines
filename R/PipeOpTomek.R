#' @title Tomek Down-Sampling
#'
#' @usage NULL
#' @name mlr_pipeops_tomek
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Generates a cleaner data set by removing all majority-minority Tomek links.
#'
#' The algorithm down-samples the data by removing all pairs of observations that form a Tomek link,
#' i.e. a pair of observations that are nearest neighbors and belong to different classes.
#' It can only be applied to [classification tasks][mlr3::TaskClassif] with numeric or integer features that have no missing values.
#' Supports multiclass classification.
#'
#' See [`themis::tomek`] for details.
#'
#' @section Construction:
#' ```
#' PipeOpTOmek$new(id = "tomek", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"tomek"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output during training is the input [`Task`][mlr3::Task] with removed rows for pairs of observations that form a Tomek link.
#' The output during prediction is the unchanged input.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @references
#' `r format_bib("tomek1976")`
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' \dontshow{ if (requireNamespace("themis")) \{ }
#' library("mlr3")
#'
#' # Create example task
#' task = tsk("iris")
#' task$head()
#' table(task$data(cols = "Species"))
#'
#' # Down-sample data
#' pop = po("tomek")
#' tomek_result = pop$train(list(task))[[1]]$data()
#' nrow(tomek_result)
#' table(tomek_result$Species)
#' \dontshow{ \} }
PipeOpTomek = R6Class("PipeOpTomek",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "tomek", param_vals = list()) {
      super$initialize(id, param_set = ps(), param_vals = param_vals, packages = "themis", can_subset_cols = FALSE,
                       task_type = "TaskClassif", tags = "imbalanced data")
    }
  ),
  private = list(

    .train_task = function(task) {
      cols = task$feature_names

      # Return task unchanged, if no feature columns exist
      if (!length(cols)) {
        return(task)
      }
      # Only numeric and integer features allowed
      if (!all(task$feature_types$type %in% c("numeric", "integer"))) {
        stop("Tomek does only accept numeric and integer features. Use PipeOpSelect to select the appropriate features.")
      }

      # Down-sample data
      dt = setDT(invoke(themis::tomek, df = task$data(), var = task$target_names))

      keep = as.integer(row.names(dt))
      task$filter(keep)
    }
  )
)

mlr_pipeops$add("tomek", PipeOpTomek)
