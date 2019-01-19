#' @title PipeOpChunk
#'
#' @name PipeOpChunk
#' @format [R6Class] PipeOpChunk
#'
#' @description
#' Chunks its input into `outnum` chunks.
#' Returns a list of [mlr3::Task].
#' During predict simply passes on the input.
#' @section Usage:
#' Inherits from [PipeOpChunk]
#' * `f = PipeOpChunk$new(outnum, id)` \cr
#'     `integer(1)`, `character(1)` -> [PipeOpCopy]
#' @section Details:
#' * `outnum`: `integer(1)` Number of times the input is copied.
#' * `shuffle`: `logical(1)` Should the data be shuffled before chunking?
#' * `stratify`: `logical(1)` Should the subsamples be stratified.
#' @family PipeOp
#' @family PipeOpBroadcast
#' @examples
#' op = PipeOpChunk$new(5)
NULL

#' @include PipeOp.R
#' @export
PipeOpChunk = R6Class("PipeOpChunk",
  inherit = PipeOp,
  public = list(
    initialize = function(outnum, id = "chunk") {
      outnum = assert_int(outnum, lower = 1)
      ps = ParamSet$new(params = list(
        ParamLgl$new("shuffle", default = TRUE),
        ParamLgl$new("stratify", default = FALSE)
      ))
      super$initialize(id,
        param_set = ps,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = rep_suffix("output", outnum), train = "Task", predict = "Task")
      )
      self$param_vals = list(shuffle = TRUE, stratify = FALSE)
    },
    train = function(inputs) {
      self$state = list()

      task = inputs[[1L]]
      colns = task$backend$colnames

      # FIXME: Implement stratification?
      idx = task$row_ids[[1L]]
      idx = split(idx, chunk(idx, n_chunks = self$outnum, shuffle = self$param_vals$shuffle))

      # Subset data, clone task and overwrite data in it.
      map(idx, function(x) {
        task$clone(deep = TRUE)$filter(x)
      })
    },

    predict = function(inputs) {
      return(rep(inputs, self$outnum))
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpChunk", PipeOpChunk)
