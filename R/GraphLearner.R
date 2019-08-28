#' @title GraphLearner
#'
#' @name mlr_learners_graph
#' @format [`R6Class`] object inheriting from [`mlr3::Learner`].
#'
#' @description
#' A [`Learner`][mlr3::Learner] that encapsulates a [`Graph`] to be used in
#' [mlr3][mlr3::mlr3-package] resampling and benchmarks.
#'
#' The Graph must return a single [`Prediction`][mlr3::Prediction] on its `$predict()`
#' call. The result of the `$train()` call is discarded, only the
#' internal state changes during training are used.
#' @family Learners
#' @export
GraphLearner = R6Class("GraphLearner", inherit = Learner,
  public = list(
    graph = NULL,
    initialize = function(graph, id = paste(graph$ids(sorted = TRUE), collapse = "."), param_vals = list(), task_type = NULL, predict_type = NULL) {

      graph = as_graph(graph, clone = TRUE)
      self$graph = graph
      output = graph$output
      if (nrow(output) != 1) {
        stop("'graph' must have exactly one output channel")
      }
      if (!are_types_compatible(output$predict, "Prediction")) {
        stop("'graph' output type not 'Prediction' (or compatible with it)")
      }

      if (is.null(task_type)) {
        class_table = mlr_reflections$task_types
        input = graph$input
        inferred = c(
          match(c(output$train, output$predict), class_table$prediction),
          match(c(input$train, input$predict), class_table$task))
        inferred = unique(class_table$type[na.omit(inferred)])
        if (length(inferred) > 1) {
          stopf("GraphLearner can not infer task_type from given Graph\nin/out types leave multiple possibilities: %s", str_collapse(inferred))
        }
        task_type = c(inferred, "classif")[1]
      }
      assert_subset(task_type, mlr_reflections$task_types$type)

      if (is.null(predict_type)) {
        predict_type = names(mlr_reflections$learner_predict_types[[task_type]])[1]
      }

      assert_subset(predict_type, names(mlr_reflections$learner_predict_types[[task_type]]))

      param_vals = insert_named(self$graph$param_set$values, param_vals)
      super$initialize(id = id, task_type = task_type,
        feature_types = mlr_reflections$task_feature_types,
        predict_types = names(mlr_reflections$learner_predict_types[[task_type]]),
        packages = graph$packages,
        properties = mlr_reflections$learner_properties[[task_type]],
        param_vals = param_vals)
      private$.predict_type = predict_type
      self$graph$param_set$values = param_vals
    },
    train_internal = function(task) {
      on.exit({self$graph$state = NULL})
      self$graph$train(task)
      state = self$graph$state
      state
    },
    predict_internal = function(task) {
      on.exit({self$graph$state = NULL})
      self$graph$state = self$model
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
      # overload this to avoid feasibility checks. all predict_types are allowed in principle
      # (although we don't--can't--change the underlying graph's behaviour).
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

