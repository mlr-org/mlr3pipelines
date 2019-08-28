#' @title PipeOpChunk
#'
#' @usage NULL
#' @name mlr_pipeops_chunk
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Chunks its input into `outnum` chunks.
#' Creates `outnum` [`Task`][mlr3::Task]s during training, and
#' simply passes on the input during `outnum` times during prediction.
#'
#' @section Construction:
#' ```
#' PipeOpChunk$new(outnum, id = "chunk", param_vals = list())
#' ```
#'
#' * `outnum` :: `numeric(1)`\cr
#'   Number of output channels, and therefore number of chunks created.
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"chunk"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output:
#' [`PipeOpChunk`] has one input channel named `"input"`, taking a [`Task`][mlr3::Task] both during training and prediction.
#'
#' [`PipeOpChunk`] has multiple output channels depending on the `options` construction argument, named `"output1"`, `"output2"`, ...
#' All output channels produce (respectively disjoint, random) subsets of the input [`Task`][mlr3::Task] during training, and
#' pass on the original [`Task`][mlr3::Task] during prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' * `shuffle` :: `logical(1)` \cr
#'   Should the data be shuffled before chunking? Initialized to `TRUE`.
#'
#' @section Internals:
#' Uses the [`mlr3misc::chunk_vector()`] function.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @include PipeOp.R
#' @export
#' @examples
#' library(mlr3)
#'
#' task = tsk("wine")
#' opc = mlr_pipeops$get("chunk", 2)
#'
#' # watch the row number: 89 during training (task is chunked)...
#' opc$train(list(task))
#'
#' # ... 178 during predict (task is copied)
#' opc$predict(list(task))
PipeOpChunk = R6Class("PipeOpChunk",
  inherit = PipeOp,
  public = list(
    initialize = function(outnum, id = "chunk", param_vals = list()) {
      outnum = assert_int(outnum, lower = 1L)
      ps = ParamSet$new(params = list(
        ParamLgl$new("shuffle", tags = "train")
      ))
      ps$values = list(shuffle = TRUE)
      super$initialize(id,
        param_set = ps, param_vals = param_vals,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = rep_suffix("output", outnum), train = "Task", predict = "Task")
      )
    },

    train_internal = function(inputs) {
      self$state = list()
      task = inputs[[1L]]

      row_ids = chunk_vector(task$row_ids, n_chunks = self$outnum, shuffle = self$param_set$values$shuffle)

      # Subset data, clone task and overwrite data in it.
      map(row_ids, function(x) {
        task$clone(deep = TRUE)$filter(x)
      })
    },

    predict_internal = function(inputs) {
      rep(inputs, self$outnum)
    }
  )
)

mlr_pipeops$add("chunk", PipeOpChunk, list("N"))
