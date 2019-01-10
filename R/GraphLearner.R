#' @title GraphLearner
#' @format [R6::R6Class()] inheriting from [LearnerClassif].
#' @Description
#' A [Learner] that encapsulates a [Graph] to be used in mlr3
#' resampling and benchmarks.
#'
#' The Graph must return a single [`Prediction`] on its `$predict()`
#' call. The result of the `$train()` call is discarded, only the
#' internal state changes during training are used.
GraphLearner = R6Class("GraphLearner", inherit = Learner,
  public = list(
    graph = NULL,
    initialize = function(graph, task_type = "classif") {
      # FIXME: drop task_type and allow all task types, as soon as mlr3 allows that
      id = paste(graph$ids(sorted = TRUE), collapse = ".")
      super$initialize(id = id, task_type = task_type,
        feature_types = mlr_reflections$task_feature_types,
        predict_types = unique(unlist(mlr_reflections$predict_types, use.names = FALSE)),
        packages = graph$packages,
        param_set = graph$param_set,
        param_vals = graph$param_vals,
        properties = unique(unlist(mlr_reflections$learner_properties, use.names = FALSE)))
      private$.predict_type = "response"  # FIXME: Learner init should do this.
      self$graph = graph
    },
    train = function(task) {
      self$graph$train(task)
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
    param_vals = function(rhs) {
      if (!missing(rhs)) {
        private$.hash = NA_character_
        self$graph$param_vals = rhs
      }
      self$graph$param_vals
    },
    predict_type = function(rhs) {
      # overload this to avoid feasibility checks. all predict_types are allowed.
      if (!missing(rhs)) {
        private$.predict_type = rhs
      }
      private$.predict_type
    }
  )
)
