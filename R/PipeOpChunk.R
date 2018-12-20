#' @title PipeOpChunk
#' @format [R6Class] PipeOpChunk
#'
#' @description
#'   Chunks its input into `outnum` chunks.
#' @section Usage:
#' Inherits from [PipeOpChunk]
#' * `f = PipeOpChunk$new(outnum, id)` \cr
#'     `integer(1)`, `character(1)` -> [PipeOpCopy]
#' @section Details:
#' * `outnum`: `integer(1)` Number of times the input is copied.
#' * `shuffle`: `logical(1)` Should the data be shuffled before chunking?
#' * `stratify`: `logical(1)` Should the subsamples be stratified.
#' @name PipeOpChunk
#' @family PipeOp, PipeOpBroadcast, PipeOpChunk
#' @export
PipeOpChunk = R6::R6Class("PipeOpChunk",
  inherit = PipeOp,
  public = list(
    initialize = function(outnum, id = "chunk") {
      ps = ParamSet$new(params = list(
        ParamLgl$new("shuffle", default = TRUE),
        ParamLgl$new("stratify", default = FALSE)
      ))
      super$initialize(id, ps)
      private$.intype = list("Task")
      private$.outtype = rep(list("Task"), outnum)
      private$.outnum = outnum
    },
    train = function(input) {
      assert_list(inputs, len = 1L, type = "Task")
      self$state = list()

      # Get feature dt from task
      task = inputs[[1L]]
      colns = task$backend$colnames

      # FIXME: Implement stratification?
      idx = chunk(task$row_ids, n.chunks = self$param_vals$outnum, shuffle = self$param_vals$shuffle)
      
      # Subset data, clone task and overwrite data in it.
      tsklst = map(idx, function(x) {
        task$clone()$filter(x)
      })
      return(tsklst)
    },
    predict = function(input) {
      return(input)
    }
  ),
  private = list(
    .outnum = NULL
  ),
  active = list(
    outnum = function() private$.outnum
  )
)