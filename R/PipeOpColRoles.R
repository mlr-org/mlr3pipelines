#' @title PipeOpColRoles
#'
#' @usage NULL
#' @name mlr_pipeops_colroles
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Changes the columns roles of the input [`Task`][mlr3::Task] according to `new_role`.
#'
#' @section Construction:
#' ```
#' PipeOpColRoles$new(id = "colroles", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"colroles"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with transformed column roles according to `new_role`.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `new_role`  :: `list` \cr
#'   Named list of new column roles. The names must match the column names of the input task that
#'   will later be trained on. Each entry of the list must contain a character vector with possible
#'   values of `feature`, `name`, `order`, `group`, `stratum`,  and `weight`. If the value is given
#'   as `character()`, the column will be dropped from the input task. Changing the role of a column
#'   results in this column loosing its previous role(s). Setting a new target variable or changing
#'   the role of an existing target variable is not supported. 
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("boston_housing")
#' pop = po("colroles", param_vals = list(
#'   new_role = list(cmedv = "order")
#' ))
#'
#' pop$train(list(task))
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpColRoles = R6Class("PipeOpColRoles",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "colroles", param_vals = list()) {
      ps = ParamSet$new(params = list(
        # named list, each entry with a vector of roles
        ParamUty$new("new_role", tags = c("train", "predict"), custom_check = function(x) {
          first_check = check_list(x, types = "character", any.missing = FALSE, min.len = 1L, names = "named")
          if (is.character(first_check)) {
            return(first_check)
          }
          # for changing the target use PipeOpNewTarget
          # a value of "character()" will lead to this column being dropped
          check_subset(unlist(x), c("feature", "name", "order", "group", "stratum", "weight"))
        })
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE)
    },

    transform = function(task) {
      # names of "new_role" must be a subset of the column names of the task
      assert_subset(names(self$param_set$values$new_role), choices = task$col_info$id)
      # changing the role of a target is not supported
      if (any(task$col_roles$target %in% names(self$param_set$values$new_role))) {
        stop("Cannot change the role of a target.")
      }
      col_roles = unlist(task$col_roles)
      indices = rep.int(seq_along(task$col_roles), times = unlist(map(task$col_roles, length)))
      for(i in seq_along(self$param_set$values$new_role)) {
        column = names(self$param_set$values$new_role)[i]
        old_role = indices[match(column, col_roles)]
        # first remove any old role
        for(role in old_role) {
          task$col_roles[[role]] = setdiff(task$col_roles[[role]], column)
        }
        # then add the new role(s)
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
