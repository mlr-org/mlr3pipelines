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
#' @section Parameter Set:
#' * `shuffle` :: `logical(1)` \cr
#'   Should the data be shuffled before chunking? Default `TRUE`
#' * `stratify` :: `logical(1)` \cr
#'   Should the subsamples be stratified. Default `FALSE`.
#' @family PipeOps
#' @examples
#' op = PipeOpChunk$new(5)
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
      idx = task$row_ids
      idx = split(idx, chunk(idx, n_chunks = self$outnum, shuffle = self$param_vals$shuffle))

      # Subset data, clone task and overwrite data in it.
      map(idx, function(x) {
        task$clone(deep = TRUE)$filter(x)
      })
    },

    predict = function(inputs) {
      rep(inputs, self$outnum)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpChunk", PipeOpChunk)
