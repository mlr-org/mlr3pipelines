#' @title PipeOpColRoles
#'
#' @usage NULL
#' @name mlr_pipeops_colroles
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOp`].
#'
#' @description
#' ColRoless a [`Task`][mlr3::Task] to use a fraction of the rows.
#'
#' Sampling happens only during training phase. Subsampling a [`Task`][mlr3::Task] may be
#' beneficial for training time at possibly (depending on original [`Task`][mlr3::Task] size)
#' negligible cost of predictive performance.
#'
#' @section Construction:
#' ```
#' PipeOpColRoles$new(id = "colroles", param_vals = list())
#' ```
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"colroles"`
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreprocSimple`].
#'
#' The output during training is the input [`Task`][mlr3::Task] with added or removed rows according to the sampling.
#' The output during prediction is the unchanged input.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreprocSimple`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreprocSimple`]; however, the `affect_columns` parameter is *not* present. Further parameters are:
#' * `frac` :: `numeric(1)`\cr
#'   Fraction of rows in the [`Task`][mlr3::Task] to keep. May only be greater than 1 if `replace` is `TRUE`. Initialized to `(1 - exp(-1)) == 0.6321`.
#' * `stratify` :: `logical(1)`\cr
#'   Should the colroless be stratified by target? Initialized to `FALSE`. May only be `TRUE` for [`TaskClassif`][mlr3::TaskClassif] input.
#' * `replace` :: `logical(1)`\cr
#'   Sample with replacement? Initialized to `FALSE`.
#'
#' @section Internals:
#' Uses `task$filter()` to remove rows. If `replace` is `TRUE` and identical rows are added, then the `task$row_roles$use` can *not* be used
#' to duplicate rows because of \[inaudible\]; instead the `task$rbind()` function is used, and
#' a new [`data.table`] is attached that contains all rows that are being duplicated exactly as many times as they are being added.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' pos = mlr_pipeops$get("colroles", param_vals = list(frac = 0.7, stratify = TRUE))
#'
#' pos$train(list(tsk("iris")))
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpColRoles = R6Class("PipeOpColRoles",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "colroles", param_vals = list()) {
      ps = ParamSet$new(params = list(
        # vector of column names
        ParamUty$new("column", tags = c("train", "predict")),
        # list of length length(column) with a vector of roles
        ParamUty$new("new_role", tags = c("train", "predict"))
      ))
      #ps$values = list(frac = 1 - exp(-1), stratify = FALSE, replace = FALSE)
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE)
    },

    #get_state = function(task) {
    #  list(column = self$param_set$values$column, new_role = self$param_set$values$new_role)
    #},

    transform = function(task) {
      col_roles = unlist(task$col_roles)
      indices = rep.int(seq_along(task$col_roles), times = unlist(map(task$col_roles, length)))
      for(i in seq_along(self$param_set$values$column)) {
        column = self$param_set$values$column[i]
        old_role = indices[match(column, col_roles)]
        for(role in old_role) {
          task$col_roles[[role]] = setdiff(task$col_roles[[role]], column)
        }
        if (length(self$param_set$values$new_role[[i]])) {
          new_role = match(self$param_set$values$new_role[[i]], names(task$col_roles))
          for(role in new_role) {
            task$col_roles[[role]] = union(task$col_roles[[role]], column)
          }
        }
      }
      
      task
    }
  )
)

mlr_pipeops$add("colroles", PipeOpColRoles)
