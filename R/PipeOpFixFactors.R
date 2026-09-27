#' @title Fix Factor Levels
#'
#' @usage NULL
#' @name mlr_pipeops_fixfactors
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
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
#'
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
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' # Reduced task with no entries for the installment_rate < 20 is defined
#' task = tsk("german_credit")
#' rows = task$row_ids[task$data()[, installment_rate != "< 20"]]
#' reduced_task = task$clone(deep = TRUE)$filter(rows)
#' levels(reduced_task$data()$installment_rate)
#'
#' # PipeOp is trained on the reduced task
#' po = po("fixfactors")
#' processed_task = preproc(reduced_task, po)
#' levels(processed_task$data()$installment_rate)
#' summary(processed_task$data()$installment_rate)
#'
#' predicted_task = preproc(task, po, predict = TRUE)
#'
#' # Predictions are made on the task without any missing data
#' levels(predicted_task$data()$installment_rate)
#' summary(predicted_task$data()$installment_rate)
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpFixFactors = R6Class("PipeOpFixFactors",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "fixfactors", param_vals = list()) {
      ps = ps(
        droplevels = p_lgl(tags = c("train", "predict"))
      )
      ps$values = list(droplevels = TRUE)
      super$initialize(id, param_set = ps, param_vals = param_vals, tags = "robustify", feature_types = c("factor", "ordered"))
    }
  ),
  private = list(
    .get_state = function(task) {
      # get the levels of the training task
      dt = task$data(cols = private$.select_cols(task))
      if (self$param_set$values$droplevels && nrow(dt)) {  # nrow(dt): workaround for https://github.com/Rdatatable/data.table/issues/5184
        dt = droplevels(dt)
      }
      list(levels = lapply(dt, function(x) levels(x)))  # explicitly access the "levels" function
    },

    .transform = function(task) {
      dt = task$data(cols = names(self$state$levels))

      # check which levels are actually different during training and prediction
      needs_adjustment = imap_lgl(self$state$levels, function(lvx, id) {
        !identical(lvx, levels(dt[[id]]))
      })

      if (!any(needs_adjustment)) {
        return(task)
      }

      changed_cols = imap_dtc(self$state$levels[needs_adjustment], function(lvx, id) {
        x = dt[[id]]
        if (is.ordered(x)) {
          ordered(x, levels = lvx)
        } else {
          factor(x, levels = lvx)
        }
      })
      task$select(setdiff(task$feature_names, colnames(changed_cols)))$cbind(changed_cols)
    }
  )
)

mlr_pipeops$add("fixfactors", PipeOpFixFactors)
