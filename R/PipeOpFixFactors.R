#' @title Fix Factor Levels
#'
#' @usage NULL
#' @name mlr_pipeops_fixfactors
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Fixes factors of type `factor`, `ordered`: Makes sure the factor levels
#' during prediction are the same as during training; possibly dropping empty
#' training factor levels before.
#'
#' Note this may introduce *missing values* during prediction if unseen factor levels are found.
#'
#' @section Construction:
#' ```
#' PipeOpFixFactors$new(id = "fixfactors", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"fixfactors"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected `factor` and `ordered` feature levels fixed.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `levels` :: named `list` of `character`\cr
#'   List of factor levels of each affected `factor` or `ordered` feature that will be fixed.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `droplevels`  :: `logical(1)` \cr
#'   Whether to drop empty factor levels of the training task. Default `TRUE`
#'
#' @section Internals:
#' Changes factor levels of columns and attaches them with a new `data.table` backend and the virtual `cbind()` backend.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
PipeOpFixFactors = R6Class("PipeOpFixFactors",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "fixfactors", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamLgl$new("droplevels", tags = c("train", "predict"))
      ))
      ps$values = list(droplevels = TRUE)
      super$initialize(id, param_set = ps, param_vals = param_vals, tags = "robustify", feature_types = c("factor", "ordered"))
    }
  ),
  private = list(
    .get_state = function(task) {
      # get the levels of the training task
      dt = task$data(cols = private$.select_cols(task))
      if (self$param_set$values$droplevels) {
        dt = droplevels(dt)
      }
      list(levels = lapply(dt, function(x) levels(x))) # explicitly access the "levels" function
    },

    .transform = function(task) {
      dt = task$data(cols = names(self$state$levels))

      # check which levels are actually different during training and prediction
      needs_adjustment = as.logical(imap(self$state$levels, function(lvx, id) {
        !identical(lvx, levels(dt[[id]]))
      }))

      if (!any(needs_adjustment)) {
        return(task)
      }

      changed_cols = as.data.table(imap(self$state$levels[needs_adjustment], function(lvx, id) {
        x = dt[[id]]
        if (is.ordered(x)) {
          ordered(x, levels = lvx)
        } else {
          factor(x, levels = lvx)
        }
      }))
      task$select(setdiff(task$feature_names, colnames(changed_cols)))$cbind(changed_cols)
    }
  )
)

mlr_pipeops$add("fixfactors", PipeOpFixFactors)

# FIXME: from mlr3; should probably go to mlr3misc
ujoin = function(x, y, key) {
  cn = setdiff(intersect(names(x), names(y)), key)
  expr = parse(text = paste0("`:=`(", paste0(sprintf("%1$s=i.%1$s",
    cn), collapse = ","), ")"))
  x[y, eval(expr), on = key]
}
