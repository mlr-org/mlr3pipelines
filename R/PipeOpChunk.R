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
    initialize = function(outnum, id = "chunk", param_vals = list()) {
      outnum = assert_int(outnum, lower = 1L)
      ps = ParamSet$new(params = list(
        ParamLgl$new("shuffle", default = TRUE),
        ParamLgl$new("stratify", default = FALSE)
      ))
      ps$values = list(shuffle = TRUE, stratify = FALSE)
      super$initialize(id,
        param_set = ps, param_vals = param_vals,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = rep_suffix("output", outnum), train = "Task", predict = "Task")
      )
    },
    train = function(inputs) {
      self$state = list()

      task = inputs[[1L]]
      colns = task$backend$colnames

      if (self$param_set$values$stratify) {
        row_ids = task$row_ids
        stratify = task$target_names
        tmp = cbind(task$data(rows = row_ids, cols = stratify), ..row_id = row_ids)[, list(..N = .N, ..row_id = list(.SD$..row_id)), by = stratify]
        row_ids = split(row_ids, unlist(map(tmp$..row_id, chunk, n_chunks = self$outnum, shuffle = self$param_set$values$shuffle)))
      } else {
        # FIXME: Implement stratification?
        row_ids = task$row_ids
        row_ids = split(row_ids, chunk(row_ids, n_chunks = self$outnum, shuffle = self$param_set$values$shuffle))
      }

      # Subset data, clone task and overwrite data in it.
      map(row_ids, function(x) {
        task$clone(deep = TRUE)$filter(x)
      })
    },

    predict = function(inputs) {
      rep(inputs, self$outnum)
    })
)
