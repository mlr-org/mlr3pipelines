#' @title PipeOpChunk
#'
#' @name mlr_pipeop_chunk
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Chunks its input into `outnum` chunks.
#' Returns a list of [`Task`]s during training, and
#' simply passes on the input during prediction.
#'
#' @section Methods:
#' * `PipeOpChunk$new(outnum, id = "chunk")` \cr
#'   (`integer(1)`, `character(1)`) -> `self` \cr
#'   Constructor. `outnum` gives the number of output
#'   channels / chunks that are created.
#'
#' @section Parameter Set:
#' * `shuffle` :: `logical(1)` \cr
#'   Should the data be shuffled before chunking? Default `TRUE`
#'
#' @family PipeOps
#' @include PipeOp.R
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("wine")
#' op = PipeOpChunk$new(3)
#' op$train(list(task = task))
PipeOpChunk = R6Class("PipeOpChunk",
  inherit = PipeOp,
  public = list(
    initialize = function(outnum, id = "chunk", param_vals = list()) {
      outnum = assert_int(outnum, lower = 1L)
      ps = ParamSet$new(params = list(
        ParamLgl$new("shuffle", default = TRUE)
      ))
      ps$values = list(shuffle = TRUE)
      super$initialize(id,
        param_set = ps, param_vals = param_vals,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = rep_suffix("output", outnum), train = "Task", predict = "Task")
      )
    },

    train = function(inputs) {
      self$state = list()
      task = inputs[[1L]]

      row_ids = chunk_vector(task$row_ids, n_chunks = self$outnum, shuffle = isTRUE(self$param_set$values$shuffle))

      # Subset data, clone task and overwrite data in it.
      map(row_ids, function(x) {
        task$clone(deep = TRUE)$filter(x)
      })
    },

    predict = function(inputs) {
      rep(inputs, self$outnum)
    }
  )
)

register_pipeop("chunk", PipeOpChunk, list("N"))
