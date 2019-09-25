#' @title PipeOpFixFactors
#'
#' @usage NULL
#' @name mlr_pipeops_fixfactors
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Fixes factors of type `factor`, `ordered`: Removes empty factor levels and/or makes sure the factor levels
#' during prediction are the same as during training.
#'
#' Note this may introduce *missing values* during prediction if unseen factor levels are found.
#'
#' @section Construction:
#' ```
#' PipeOpFixFactors$new(id = "encode", param_vals = list())
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
#' * `droplevels``  :: `logical(1)` \cr
#'   Whether to drop empty factor levels. Default `TRUE`
#' * `fix_predict_levels` :: `logical(1)` \cr
#'   Whether to make sure the same factor levels are present during prediction as during training.
#'   This may introduce missing values during training if unseen factor levels are found. Default `TRUE`.
#'
#' @section Internals:
#' Uses the [`mlr3::Task`] functionality of handling factor levels.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
PipeOpFixFactors = R6Class("PipeOpFixFactors",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "fixfactors", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamLgl$new("droplevels", tags = c("train", "predict")),
        ParamLgl$new("fix_predict_levels", tags = c("train", "predict"))
      ))
      ps$values = list(droplevels = TRUE, fix_predict_levels = TRUE)
      super$initialize(id, param_set = ps, param_vals = param_vals)
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("factor", "ordered"), get("id")]
    },

    get_state = function(task) {
      if (self$param_set$values$droplevels) {
        lvls = task$backend$distinct(rows = task$row_ids, cols = task$feature_names)
      } else {
        lvls = task$levels()
      }
      list(levels = lvls)
    },

    transform = function(task) {
      vals = self$param_set$values
      if (!vals$droplevels && !fix_predict_levels) {
        # nothing changes --> don't need to clone
        return(task)
      }
      task = task$clone(deep = TRUE)
      task$col_info = ujoin(task$col_info, enframe(self$state$levels, "id", "levels"), key = "id")
      task
    }
  )
)

mlr_pipeops$add("fixfactors", PipeOpFixFactors)

# FIXME: from mlr3; should probably go to mlr3misc
ujoin = function (x, y, key) {
  cn = setdiff(intersect(names(x), names(y)), key)
  expr = parse(text = paste0("`:=`(", paste0(sprintf("%1$s=i.%1$s",
    cn), collapse = ","), ")"))
  x[y, eval(expr), on = key]
}
