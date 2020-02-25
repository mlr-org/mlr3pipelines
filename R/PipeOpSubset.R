#' @title PipeOpSubset
#'
#' @usage NULL
#' @name mlr_pipeops_subset
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' subsets a [`Task`][mlr3::Task] to use a fraction of the rows.
#'
#' Sampling happens only during training phase. Subsetting a [`Task`][mlr3::Task] may be
#' beneficial for training time at possibly (depending on original [`Task`][mlr3::Task] size)
#' negligible cost of predictive performance.
#'
#' @section Construction:
#' ```
#' PipeOpSubset$new(id = "subset", param_vals = list())
#' ```
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"subset"`
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output during training is the input [`Task`][mlr3::Task] with added or removed rows according to the sampling.
#' The output during prediction is the unchanged input.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`]; however, the `affect_columns` parameter is *not* present. Further parameters are:
#' * `condition` :: `formula`\cr
#'   Expression that evaluate to a boolean vector of length `task$nrows`, in the form of `formula`.
#'   This expression may reference other features, as well as variables visible at the creation of the `formula` (see examples).
#'   Initialized to `list()`.
#' @section Internals:
#' Uses `task$filter()` to remove rows. If `replace` is `TRUE` and identical rows are added, then the `task$row_roles$use` can *not* be used
#' to duplicate rows because of \[inaudible\]; instead the `task$rbind()` function is used, and
#' a new [`data.table`] is attached that contains all rows that are being duplicated exactly as many times as they are being added.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' pos = mlr_pipeops$get("subset", param_vals = list("condition" = ~ Species != "Versicolor"))
#'
#' pos$train(list(tsk("iris")))
#'
#' ä
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpSubset = R6Class("PipeOpSubset",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "subset", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("condition", custom_check = check_formula, tags = c("train", "predict", "required"))
      ))
      ps$values = list(condition = ~TRUE)
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE)
    },

    train_task = function(task) {
      taskdata = task$data()
      frm = self$param_set$values$condition
      split = assert_logical(eval(frm[[2]], envir = taskdata, enclos = environment(frm)))
      self$state = list()
      task_filter_ex(task, task$row_ids[split])
    },

    predict_task = identity
  )
)

mlr_pipeops$add("subset", PipeOpSubset)
