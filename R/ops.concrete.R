#' @title PipeOpNULL
#' @format [R6Class] PipeOpNULL
#' 
#' @description
#'   Simply pushes the input forward unchanged.
#'   Can be usefull for example to keep the original task in conjunction with
#'   `gunion()` to keep a copy of the original data.
#' 
#' @section Usage:
#' * `f = pipeOpNULL$new(id)` \cr
#'     `character(1)` -> [PipeOpNULL]
#' @name pipeOpNULL
#' @family pipeOp
#' @examples
#' # Do PCA on input data, but also keep a copy of the original input.
#' op1 = PipeOpNULL$new()
#' op2 = PipeOpPCA$new()
#' g = gunion(op1, op2) %>>% pipeOpFeatureUnion()
#' @export
PipeOpNULL = R6Class("PipeOpNULL",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "OpNULL") {
      super$initialize(id)
      private$.intype = list("any")
      private$.outtype = list("any")
    },

    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      self$state = list()
      inputs
    },

    predict = function(inputs) {
      return(inputs)
    }
  )
)

#' @title PipeOpDT
#' @format [R6Class] PipeOpDT
#' 
#' @description
#'   This let's us work with a [data.table] instead of a Task and
#'   delegate all handling of [Task]s and [DataBackend] to the class.
#'   Allows us to specify functions  `train_dt()` and `predict_dt` instead of
#'   `train()` and `predict`, that expect the [data.table] containing only
#'   the features from a [Task], and automatically
#'   reconstruct the appropriate Task from a returned [data.table].
#'   We thus enforce: [dt] -> [dt].
#'   For examples see [pipeOpPCA] or [pipeOpScale].
#' @section Usage:
#' * `f = pipeOpDT$new(id, ps)` \cr
#'     `character(1)`, `[ParamSet]` -> [PipeOpDT]
#' @name pipeOpDT
#' @family pipeOp, pipeOpDT
#' @export
PipeOpDT = R6Class("PipeOpDT",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "PipeOpDT", ps = ParamSet$new()) {
      super$initialize(id, ps)
      private$.intype = list("any")
      private$.outtype = list("any")
    },

    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      assert_function(self$train_dt, args = "dt")

      # Get feature dt from task
      task = inputs[[1L]]
      fn = task$feature_names
      d = task$data()

      # Call train_dt function on features
      dt = as.data.table(self$train_dt(d[, ..fn]))
      assert_true(nrow(dt) == nrow(d))

      # Drop old features, add new features
      d[, (fn) := NULL]
      d[, (colnames(dt)) := dt]
      d[, "..row_id" := seq_len(nrow(d))]

      db = DataBackendDataTable$new(d, primary_key = task$backend$primary_key)
      tn = task$target_names

      # Should be:
      list(TaskClassif$new(id = task$id, backend = db, target = tn))
    },

    predict = function() {
      assert_list(self$inputs, len = 1L, type = "Task")
      assert_function(self$predict_dt, args = "newdt")
      task = self$inputs[[1L]]
      fn = task$feature_names
      d = task$data()

      # Call train_dt function on features
      dt = as.data.table(self$predict_dt(d[, ..fn]))
      assert_true(nrow(dt) == nrow(d))

      # Drop old features, add new features
      d[, (fn) := NULL]
      d[, (colnames(dt)) := dt]
      d[, "..row_id" := seq_len(nrow(d))]

      list(task$overwrite(d))
    }
  )
)

# Scale Data
# [dt] -> [dt]
PipeOpScaler = R6Class("PipeOpScaler",

  inherit = PipeOpDT,

  public = list(
    initialize = function(id = "scaler") {
      ps = ParamSet$new(params = list(
        ParamLgl$new("center", default = TRUE),
        ParamLgl$new("scale", default = TRUE)
      ))
      super$initialize(id, ps)
    },

    train_dt = function(dt) {
      sc = scale(as.matrix(d[, ..fn]),
        center = self$param_vals$center,
        scale = self$param_vals$scale)

      private$state = list(
        center = attr(sc, "scaled:center") %??% 0,
        scale = attr(sc, "scaled:scale") %??% 1
      )
      return(sc)
    },

    predict_dt = function(newdt) {
      scaled = (newdt - private$state$center) / private$state$center
      return(scaled)
    }
  )
)

# Rotate data
# [dt] -> [dt]
PipeOpPCA = R6Class("PipeOpPCA",

  inherit = PipeOpDT,

  public = list(
    initialize = function(id = "pca") {
      ps = ParamSet$new(params = list(
        ParamLgl$new("center", default = TRUE),
        ParamLgl$new("scale.", default = FALSE),
        ParamInt$new("rank.", default = NULL, lower = 1, upper = Inf, special_vals = list(NULL))
      ))
      super$initialize(id, ps)
    },

    train_dt = function(dt) {
      pcr = prcomp(as.matrix(dt),
        center = self$param_vals$center,
        scale. = self$param_vals$scale.,
        rank.  = self$param_vals$rank.)
      self$state = pcr
      return(pcr$x)
    },

    predict_dt = function(newdt) {
      rotated = predict(self$state, as.matrix(newdt))
      return(rotated)
    }
  )
)

# Sparse PCA
# [sparseMatrix] -> [dt]
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


# simple feature transform, no hyperpars
PipeOpDownsample = R6Class("PipeOpDownsample",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "downsample") {
      ps = ParamSet$new(params = list(
        ParamNum$new("perc", default = 0.7, lower = 0, upper = 1),
        ParamLgl$new("stratify", default = FALSE)
      ))
      super$initialize(id, ps)
      private$.intype = list("any")
      private$.outtype = list("any")

    },

    train = function() {
      assert_list(self$inputs, len = 1L, type = "Task")
      # FIXME: this is really bad code how i change the data of the task
      # ich muss hier das backend austauschen
      task = self$inputs[[1L]]
      fn = task$feature_names
      # FIXME: Discuss whether we want to use the current mlr implementation
      list(TaskClassif$new(id = task$id, data = d, target = task$target_names))
    },

    predict = function() {
      assert_list(self$inputs, len = 1L, type = "Task")
      # FIXME: Make sure dimensions fit (if input did not have full rank)
      list(as.data.frame(as.matrix(input) %*% self$params))
    }
  )
)

# cpoNull = PipeOp$new(
#   id = "null",
#   in.format = "task",
#   out.format = "task",
#   train = function(inlist, param_vals) {
#     list(
#       control = list(),
#       task = task
#     )
#   },

#   predict = function(inlist, control) {
#     list(
#       task = inlist$task
#     )
#   },

#   param_set = ParamSet$new()
# )

# cpoDropConst = PipeOp$new(
#   id = "dropconst",
#   in.format = "data-target",
#   out.format = "data",

#   train = function(inlist, param_vals) {

#   # perc = 0, na.ignore = FALSE, tol = .Machine$double.eps^.5

#     #FIXME: where to put these asserts?
#     # can phng handle this now automatically?

#     # assertNumber(perc, lower = 0, upper = 1)
#     # assertSubset(dont.rm, choices = names(data))
#     # assertFlag(na.ignore)
#     # assertNumber(tol, lower = 0)

#     isEqual = function(x, y) {
#       res = (x == y) | (is.na(x) & is.na(y))
#       replace(res, is.na(res), FALSE)
#     }
#     #FIXME: this function was really slow due to computeMode in BBmisc. what to do?
#     # sorting the vector first is probably a good idea. then iterate in C
#     digits = ceiling(log10(1 / tol))
#     cns = setdiff(colnames(data), dont.rm)
#     ratio = map_dbl(data[cns], function(x) {
#       if (allMissing(x))
#         return(0)
#       if (is.double(x))
#         x = round(x, digits = digits)
#       m = computeMode(x, na.rm = param_vals$na.ignore, ties.method = "first")
#       if (param_vals$na.ignore) {
#         mean(m != x, na.rm = TRUE)
#       } else {
#         mean(!isEqual(x, m))
#       }
#     }, use.names = FALSE)

#     dropped.cols = cns[ratio <= perc]
#     # FIXME: do we show such messages in verbose mode?
#     # if (show.info && length(dropcols))
#       # messagef("Removing %i columns: %s", length(dropcols), collapse(dropcols))
#     dropNamed(data, dropcols)
#     list(
#       data = dropNamed(data, drop.cols),
#       drop.cols = drop.cols
#     )
#   },

#   predict = function(inlist, control) {
#     data[control$dropped.cols]
#   },

#   param_set = ParamSet$new(params = list(
#     ParamDbl$new("perc", default = 0.005, lower = 0, upper = 1),
#     ParamDbl$new("tol", default = .Machine$double.eps^.5, lower = 0, upper = 1),
#     ParamLgl$new("na.ignore", default = FALSE)
#   ))
# )


