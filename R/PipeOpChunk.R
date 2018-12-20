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
#' @family PipeOp, PipeOpBroadcast, PipeOpDT, PipeOpChunk
#' @export
PipeOpChunk = R6::R6Class("PipeOpChunk",
  inherit = PipeOpDT,
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
    train_dt = function(dt) {
      self$state = list()
      # FIXME: Implement stratification
      idx = chunk(rownames(dt), n.chunks = self$param_vals$outnum, shuffle = self$param_vals$shuffle)
      map(idx, function(x) subset(dt, rownames(dt) %in% x))
    },
    predict_dt = function(dt) {
      return(dt)
    }
  ),
  private = list(
    .outnum = NULL
  ),
  active = list(
    outnum = function() private$.outnum
  )
)