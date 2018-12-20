# simple feature transform, no hyperpars
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
      private$.result = inputs[[1L]]
      self$state = list()
      inputs
    },

    predict = function(inputs) {
      return(inputs)
    }
  )
)

# This let's us work with a dt instead of a Task
# [dt] -> [dt]
PipeOpFeatureTransform = R6Class("PipeOpFeatureTransform",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "PipeOpFeatureTransform", ps = ParamSet$new()) {
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
      dt = self$train_dt(d[, ..fn])
      assert_data_table(dt)
      assert_true(nrow(dt) == nrow(d))

      # Drop old features, add new features
      d[, (fn) := NULL]
      d[, (colnames(dt)) := dt]
      d[, "..row_id" := seq_len(nrow(d))]

      db = DataBackendDataTable$new(d, primary_key = task$backend$primary_key)
      tn = task$target_names

      # Should be:
      # private$.result = task$overwrite(d)
      list(TaskClassif$new(id = task$id, backend = db, target = tn))
    },

    predict = function() {
      assert_list(self$inputs, len = 1L, type = "Task")
      assert_function(self$predict_dt, args = "newdt")
      task = self$inputs[[1L]]
      fn = task$feature_names
      d = task$data()

      # Call train_dt function on features
      dt = self$predict_dt(d[, ..fn])
      assert_data_table(dt)
      assert_true(nrow(dt) == nrow(d))

      # Drop old features, add new features
      d[, (fn) := NULL]
      d[, (colnames(dt)) := dt]
      d[, "..row_id" := seq_len(nrow(d))]

      list(task$overwrite(d))
    }
  )
)

PipeOpPCA = R6Class("PipeOpPCA",

  inherit = PipeOpFeatureTransform,

  public = list(
    initialize = function(id = "pca") {
      ps = ParamSet$new(params = list(
        ParamLgl$new("center", default = TRUE),
        ParamLgl$new("scale.", default = FALSE),
        ParamInt$new("rank.", default = NULL, lower = 1, upper = Inf)
      ))
      super$initialize(id, ps)
    },

    train_dt = function(dt) {
      pcr = prcomp(as.matrix(dt),
        center = self$param_vals$center,
        scale. = self$param_vals$scale.,
        rank.  = self$param_vals$rank.)
      self$state = pcr
      as.data.table(pcr$x)
    },

    predict_dt = function(newdt) {
      rotated = predict(self$state, as.matrix(newdt))
      return(as.data.table(rotated))
    }
  )
)

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

# simple feature transform, but hyperpars
PipeOpScaler = R6Class("PipeOpScaler",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "scaler") {

      ps = ParamSet$new(params = list(
        ParamLgl$new("center", default = TRUE),
        ParamLgl$new("scale", default = TRUE)
      ))
      super$initialize(id, ps)
      private$.intype = list("any")
      private$.outtype = list("any")

    },

    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      task = inputs[[1L]]

      fn = task$feature_names
      d = task$data()

      sc = scale(as.matrix(d[, ..fn]),
        center = self$param_vals$center,
        scale = self$param_vals$scale)

      private$.params = list(
        center = attr(sc, "scaled:center") %??% FALSE,
        scale = attr(sc, "scaled:scale") %??% FALSE
      )
      d[, fn] = as.data.table(sc)

      db = as_data_backend(d)
      private$.result = TaskClassif$new(id = task$id, backend = db, target = task$target_names)
      private$.result
    },

    predict2 = function() {
      assert_list(self$inputs, len = 1L, type = "Task")
      task = self$inputs[[1L]]
      as.data.frame(scale(as.matrix(input),
        center = self$params$center, scale = self$params$scale))
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


