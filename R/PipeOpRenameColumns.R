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
#' * `old` :: `character`\cr
#'   Character vector specifying the old column names that should be changed. Default `character()`, i.e., no column names are changed.
#' * `new` :: `character`\cr
#'   Character vector of the new column names with the positions matching the old column names. Default `character()`.
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
#' pop = po("renamecolumns", param_vals = list(old = c("Species", "Petal.Length"), new = c("S", "PL")))
#' pop$train(list(task))
PipeOpRenameColumns = R6Class("PipeOpRenameColumns",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "renamecolumns", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("old", default = character(), tags = c("train", "predict"), custom_check = function(x) check_names(x, type = "strict")),
        ParamUty$new("new", default = character(), tags = c("train", "predict"), custom_check = function(x) check_names(x, type = "strict"))
      ))
      ps$values = list(old = character(), new = character())
      super$initialize(id, ps, param_vals = param_vals, can_subset_cols = FALSE)
    }
  ),
  private = list(
    .transform = function(task) {
      old = self$param_set$values$old
      assert_subset(old, choices = unique(unlist(task$col_roles, use.names = FALSE)))
      new = self$param_set$values$new
      n = length(new)

      if (n != length(old)) {
        stopf("The length of new must match the length of old.")
      }

      if (n == 0L) {
        return(task)  # early exit
      }

      task$rename(old = old, new = new)
      task
    }
  )
)

mlr_pipeops$add("renamecolumns", PipeOpRenameColumns)
