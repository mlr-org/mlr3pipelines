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
#'
#' The `predict_type` of a [`GraphLearner`] can be obtained or set via it's `predict_type` active binding.
#' Setting a new predict type will try to set the `predict_type` in all relevant 
#' [`PipeOp`] / [`Learner`][mlr3::Learner] encapsulated within the [`Graph`]. 
#' Similarly, the predict_type of a Graph will always be the smallest denominator in the [`Graph`].
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

      param_vals = insert_named(self$graph$param_set$values, param_vals)
      super$initialize(id = id, task_type = task_type,
        feature_types = mlr_reflections$task_feature_types,
        predict_types = names(mlr_reflections$learner_predict_types[[task_type]]),
        packages = graph$packages,
        properties = mlr_reflections$learner_properties[[task_type]])
      self$graph$param_set$values = param_vals
      if(!is.null(predict_type)) self$predict_type = predict_type
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
      if (!missing(rhs)) {
        private$set_predict_type(rhs)
      }
      private$get_predict_type()
    },
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, self$graph$param_set)) {
        stop("param_set is read-only.")
      }
      self$graph$param_set
    }
  ),
  private = list(
    deep_clone = function(name, value) {
      # FIXME this repairs the mlr3::Learner deep_clone() method which is broken.
      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      value
    },
    get_predict_type = function() {
      get_po_predict_type = function(x) {
        if (!is.null(x$predict_type)) return(x$predict_type)
        prdcssrs = self$graph$edges[dst_id == x$id, ]$src_id
        if (length(prdcssrs)) {
          predict_types = map_chr(self$graph$pipeops[prdcssrs], get_po_predict_type)
          if (length(unique(predict_types)) == 1L)
            unique(predict_types)
          else
            NULL
        } else {
          NULL
        }
      }
      predict_type = get_po_predict_type(self$graph$pipeops[[self$graph$rhs]])
      if (is.null(predict_type))
        mlr_reflections$learner_predict_types[[self$task_type]][[1]]
      else 
        predict_type
    },
    set_predict_type = function(predict_type) {
      set_po_predict_type = function(x, predict_type) {
        assert_subset(predict_type, unlist(mlr_reflections$learner_predict_types[[self$task_type]]))
        if (!is.null(x$predict_type)) x$predict_type = predict_type
        prdcssrs = self$graph$edges[dst_id == x$id, ]$src_id
        if (length(prdcssrs)) {
          map(self$graph$pipeops[prdcssrs], set_po_predict_type, predict_type = predict_type)
        }
      }
      set_po_predict_type(self$graph$pipeops[[self$graph$rhs]], predict_type)
    }
  )
)

#' @export
as_learner.Graph = function(x, clone = FALSE) {
  GraphLearner$new(x)
}
