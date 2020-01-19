#' @title PipeOpFilterRows
#'
#' @usage NULL
#' @name mlr_pipeops_filterrows
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' WORK IN PROGRESS (more like a toy example). Filters a task based on row
#' indices supplied as integer parameters. The number of outputs therefore
#' directly depends on the number of parameter vectors supplied. Row
#' indices must be unique and in union be the same as all row indices of the
#' data of the task. Prediction is not clear, yet.
#'
#' @section Construction:
#' ```
#' PipeOpFilterRows$new(id = "filterrows", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"copy"`.
#' * `param_vals` :: named `list`\cr
#'   List of row indices (integer) that are used for filtering the task.
#'
#' @section Input and Output Channels:
#' [`PipeOpFilterRows`] has one input channel named `"input"`, taking a
#' [`Task`][mlr3::Task], or a subclass of [`Task`][mlr3::Task] if the
#' `task_type` construction argument is given as such; both during training and
#' prediction.
#'
#' [`PipeOpFilterRows`] has multiple output channels depending on the length of
#' the `param_vals` list, named `"output1"`, `"output2"`, ... All output
#' channels produce the object given as input (`"*"`).
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' [`PipeOpFilterRows`] takes a list of parameters that should consists of
#' integer vectors that resemble row indices of the data of the task which are
#' used to filter the task.
#'
#' @section Internals:
#' Highly experimental.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @examples
#' # Simple binary split
#' library("mlr3")
#' task = tsk("iris")
#' grp1 = which(task$data(cols = "Sepal.Length") < 5.1)
#' grp2 = setdiff(task$row_ids, grp1)
#' po1 = mlr_pipeops$get("filterrows", param_vals = list(grp1 = grp1, grp2 = grp2))
#' po1$train(list(task))
#' @family PipeOps
#' @family Placeholder Pipeops
#' @include PipeOp.R
#' @export
PipeOpFilterRows = R6Class("PipeOpFilterRows",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "filterrows", param_vals = list(), task_type = "Task") {
      #stopifnot(!is.null(param_vals)) do we need this?
      nms = names(param_vals)
      n_param = seq_along(param_vals)
      ps = ParamSet$new(params = lapply(n_param, function(id) {
        ParamUty$new(nms[id], tags = "train", custom_check = function(x) check_integer(x, lower = 1L, any.missing = FALSE, min.len = 1L, unique = TRUE))
      }))
      super$initialize(id, param_set = ps, param_vals = param_vals,
        input = data.table(name = "input", train = task_type, predict = "*"),
        output = data.table(name = rep_suffix("output", n = ps$length), train = task_type, predict = "*"))
    },

    train_internal = function(inputs) {
      self$state = list()
      intask = inputs[[1L]]$clone(deep = TRUE)
      self$train_task(intask)
    },

    train_task = function(task) {
      rows = self$param_set$get_values(tags = "train")
      stopifnot(identical(sort(unname(unlist(rows))), sort(task$row_ids)))
      lapply(rows, function(id) {
        task$clone(deep = TRUE)$filter(id)
      })
    },

    # FIXME: not sure what to predict, yet
    predict_internal = function(inputs) {
      rep_len(inputs, self$outnum)
    }
  )
)

mlr_pipeops$add("filterrows", PipeOpFilterRows)
