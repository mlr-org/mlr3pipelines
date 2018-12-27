#' @title PipeOpSparsePCA
#' @format [R6Class] PipeOpSparsePCA
#'
#' @description
#'   Extracts principle components from a sparse matrix.
#'   See [irlbae::prcomp_irlba] for details  and parameters.
#' @section Usage:
#' Inherits from [PipeOp]
#' * `f = pipeOpSparsePCA$new(id)` \cr
#'     `character(1)` -> [PipeOpSparsePCA]
#' @name PipeOpSparsePCA
#' @family PipeOp, PipeOpPCA
#' @export
PipeOpSparsePCA = R6Class("PipeOpSparsePCA",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "sparsePca") {
      ps = ParamSet$new(params = list(
        ParamLgl$new("center", default = TRUE),
        ParamLgl$new("scale", default = TRUE),
        ParamInt$new("n", default = 3L, lower = 1, upper = Inf)
      ))
      super$initialize(id, ps)
      self$packages = "irlba"
      private$.intype = list("any")
      private$.outtype = list("any")

    },

    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      task = inputs[[1L]]
      fn = task$feature_names

      # Get sparse matrix
      d = task$data()
      spmat = task$backend$data(paste0("row_", seq_len(nrow(d))), task$feature_names, format = "sparse")
      sc = irlba::prcomp_irlba(spmat, center = self$param_vals$center, scale = self$param_vals$scale, n = self$param_vals$n)

      self$state = sc

      d[, fn] = NULL
      d[, colnames(sc$x)] = as.data.table(sc$x)
      d[, task$backend$primary_key] = task$backend$data(paste0("row_", seq_len(nrow(d))), task$backend$primary_key)

      db = DataBackendDataTable$new(d, primary_key = task$backend$primary_key)
      tn = task$target_names
      list(TaskRegr$new(id = task$id, backend = db, target = tn))
    },

    predict = function() {
      assert_list(self$inputs, len = 1L, type = "Task")
      task = self$inputs[[1L]]
      fn = task$feature_names

      # Get sparse matrix
      d = task$data()
      spmat = task$backend$data(paste0("row_", seq_len(nrow(d))), task$feature_names, format = "sparse")
      dt = as.data.table(predict(self$state, spmat))
      assert_true(nrow(dt) == nrow(d))

      # Drop old features, add new features
      d[, (fn) := NULL]
      d[, (colnames(dt)) := dt]

      list(TaskClassif$new(id = task$id, backend = as_data_backend(d), target = task$target_names))
    }
  )
)