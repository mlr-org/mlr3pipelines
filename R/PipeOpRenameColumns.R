#' @title PipeOpRenameColumns
#'
#' @usage NULL
#' @name mlr_pipeops_renamecolumns
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOp`].
#'
#' @description
#' Renames the columns of a [`Task`][mlr3::Task] both during training and prediction.
#' Uses the `$rename()` mutator of the [`Task`][mlr3::Task].
#'
#' @section Construction:
#' ```
#' PipeOpRenameColumns$new(id = "renamecolumns", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"renamecolumns"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreprocSimple`].
#'
#' The output is the input [`Task`][mlr3::Task] with the old column names changed to the new ones.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreprocSimple`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreprocSimple`], as well as:
#' * `renaming` :: named `character`\cr
#'   Named character vector. The names of the vector specify the old column names that should be
#'   changed to the new column names as given by the elements of the vector.
#'
#' @section Internals:
#' Uses the `$rename()` mutator of the [`Task`][mlr3::Task] to set the new column names.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("renamecolumns", param_vals = list(renaming = c("Species" = "S", "Petal.Length" = "PL")))
#' pop$train(list(task))
PipeOpRenameColumns = R6Class("PipeOpRenameColumns",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "renamecolumns", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("renaming", default = character(), tags = c("train", "predict"), custom_check = function(x) check_names(x, type = "strict"))
      ))
      ps$values = list(renaming = character())
      super$initialize(id, ps, param_vals = param_vals, can_subset_cols = FALSE)
    }
  ),
  private = list(
    .transform = function(task) {
      if (length(self$param_set$values$renaming) == 0L) {
        return(task)  # early exit
      }

      task$rename(old = names(self$param_set$values$renaming), new = self$param_set$values$renaming)
      task
    }
  )
)

mlr_pipeops$add("renamecolumns", PipeOpRenameColumns)
