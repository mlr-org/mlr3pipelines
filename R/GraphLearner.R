#' @title GraphLearner
#' @format [`R6Class`] object inheriting from [`mlr3::Learner`].
#'
#' @description
#' A [`Learner`][mlr3::Learner] that encapsulates a [`Graph`] to be used in
#' [mlr3][mlr3::mlr3-package] resampling and benchmarks.
#'
#' The Graph must return a single [`Prediction`][mlr3::Prediction] on its `$predict()`
#' call. The result of the `$train()` call is discarded, only the
#' internal state changes during training are used.
#' @export
GraphLearner = R6Class("GraphLearner", inherit = Learner,
  public = list(
    graph = NULL,
    model = NULL,
    initialize = function(graph, task_type = "classif", id = paste(graph$ids(sorted = TRUE), collapse = "."), param_vals = list(), predict_type = mlr_reflections$predict_types[[task_type]][1]) {
      # Please don't `assert_r6(graph, "Graph")` here, we have assert_graph(coerce = TRUE) for that, graph can be a PipeOp too
      assert_choice(task_type, c("classif", "regr"))
      # FIXME: drop task_type and allow all task types, as soon as mlr3 allows that
      assert_subset(task_type, mlr_reflections$task_types)
      graph = assert_graph(graph, coerce = TRUE, deep_copy = TRUE)
      self$graph = graph
      param_vals = insert_named(self$graph$param_set$values, param_vals)
      super$initialize(id = id, task_type = task_type,
        feature_types = mlr_reflections$task_feature_types,
        predict_types = mlr_reflections$predict_types[[task_type]],
        packages = graph$packages,
        properties = mlr_reflections$learner_properties[[task_type]])
      private$.predict_type = predict_type
      self$graph$param_set$values = param_vals
    },
    train = function(task) {
      self$graph$train(task)
      self$model = self$graph
      invisible(self)
    },
    predict = function(task) {
      prediction = self$graph$predict(task)
      assert_list(prediction, types = "Prediction", len = 1,
        .var.name = sprintf("Prediction returned by Graph %s", self$id))
      prediction[[1]]
    }
  ),
  active = list(
    hash = function() {
      self$graph$hash
    },
    predict_type = function(rhs) {
      # overload this to avoid feasibility checks. all predict_types are allowed.
      if (!missing(rhs)) {
        private$.predict_type = rhs
      }
      private$.predict_type
    },
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, self$graph$param_set)) {
        stop("param_set is read-only.")
      }
      self$graph$param_set
    }
  )
)
