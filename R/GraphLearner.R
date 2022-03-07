#' @title Encapsulate a Graph as a Learner
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
#'
#' A `GraphLearner` is always constructed in an untrained state. When the `graph` argument has a
#' non-`NULL` `$state`, it is ignored.
#'
#' @section Construction:
#' ```
#' GraphLearner$new(graph, id = NULL, param_vals = list(), task_type = NULL, predict_type = NULL)
#' ```
#'
#' * `graph` :: [`Graph`] | [`PipeOp`]\cr
#'   [`Graph`] to wrap. Can be a [`PipeOp`], which is automatically converted to a [`Graph`].
#'  This argument is usually cloned, unless `clone_graph` is `FALSE`; to access the [`Graph`] inside `GraphLearner` by-reference, use `$graph`.\cr
#' * `id` :: `character(1)`
#'   Identifier of the resulting [`Learner`][mlr3::Learner].
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings . Default `list()`.
#' * `task_type` :: `character(1)`\cr
#'   What `task_type` the `GraphLearner` should have; usually automatically inferred for [`Graph`]s that are simple enough.
#' * `predict_type` :: `character(1)`\cr
#'   What `predict_type` the `GraphLearner` should have; usually automatically inferred for [`Graph`]s that are simple enough.
#' * `clone_graph` :: `logical(1)`\cr
#'   Whether to clone `graph` upon construction. Unintentionally changing `graph` by reference can lead to unexpected behaviour,
#'   so `TRUE` (default) is recommended. In particular, note that the `$state` of `$graph` is set to `NULL` by reference on
#'   construction of `GraphLearner`, during `$train()`, and during `$predict()` when `clone_graph` is `FALSE`.
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`], as well as:
#' * `graph` :: [`Graph`]\cr
#'   [`Graph`] that is being wrapped. This field contains the prototype of the [`Graph`] that is being trained, but does *not*
#'   contain the model. Use `graph_model` to access the trained [`Graph`] after `$train()`. Read-only.
#' * `graph_model` :: [`Learner`][mlr3::Learner]\cr
#'   [`Graph`] that is being wrapped. This [`Graph`] contains a trained state after `$train()`. Read-only.
#'
#' @section Internals:
#' [`as_graph()`] is called on the `graph` argument, so it can technically also be a `list` of things, which is
#' automatically converted to a [`Graph`] via [`gunion()`]; however, this will usually not result in a valid [`Graph`] that can
#' work as a [`Learner`][mlr3::Learner]. `graph` can furthermore be a [`Learner`][mlr3::Learner], which is then automatically
#' wrapped in a [`Graph`], which is then again wrapped in a `GraphLearner` object; this usually only adds overhead and is not
#' recommended.
#'
#' @family Learners
#' @export
#' @examples
#' library("mlr3")
#'
#' graph = po("pca") %>>% lrn("classif.rpart")
#'
#' lr = GraphLearner$new(graph)
#' lr = as_learner(graph)  # equivalent
#'
#' lr$train(tsk("iris"))
#'
#' lr$graph$state  # untrained version!
#' # The following is therefore NULL:
#' lr$graph$pipeops$classif.rpart$learner_model$model
#'
#' # To access the trained model from the PipeOpLearner's Learner, use:
#' lr$graph_model$pipeops$classif.rpart$learner_model$model
#'
#' # Feature importance (of principal components):
#' lr$graph_model$pipeops$classif.rpart$learner_model$importance()
GraphLearner = R6Class("GraphLearner", inherit = Learner,
  public = list(
    initialize = function(graph, id = NULL, param_vals = list(), task_type = NULL, predict_type = NULL, clone_graph = TRUE) {
      graph = as_graph(graph, clone = assert_flag(clone_graph))
      graph$state = NULL

      id = assert_string(id, null.ok = TRUE) %??% paste(graph$ids(sorted = TRUE), collapse = ".")
      private$.graph = graph

      output = graph$output
      if (nrow(output) != 1) {
        stop("'graph' must have exactly one output channel")
      }
      if (!are_types_compatible(output$predict, "Prediction")) {
        stop("'graph' output type not 'Prediction' (or compatible with it)")
      }

      if (is.null(task_type)) {
        task_type = infer_task_type(graph)
      }
      assert_subset(task_type, mlr_reflections$task_types$type)

      super$initialize(id = id, task_type = task_type,
        feature_types = mlr_reflections$task_feature_types,
        predict_types = names(mlr_reflections$learner_predict_types[[task_type]]),
        packages = graph$packages,
        properties = mlr_reflections$learner_properties[[task_type]])

      if (length(param_vals)) {
        private$.graph$param_set$values = insert_named(private$.graph$param_set$values, param_vals)
      }
      if (!is.null(predict_type)) self$predict_type = predict_type
    }
  ),
  active = list(
    hash = function() {
      digest(list(self$id, self$graph$hash), algo = "xxhash64")
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
    },
    graph = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.graph)) stop("graph is read-only")
      private$.graph
    },
    graph_model = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.graph)) {
        stop("graph_model is read-only")
      }
      if (is.null(self$model)) {
        private$.graph
      } else {
        g = private$.graph$clone(deep = TRUE)
        g$state = self$model
        g
      }
    }
  ),
  private = list(
    .graph = NULL,
    deep_clone = function(name, value) {
      # FIXME this repairs the mlr3::Learner deep_clone() method which is broken.
      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      if (name == "state") {
        value$log = copy(value$log)
      }
      value
    },

    .train = function(task) {
      on.exit({self$graph$state = NULL})
      self$graph$train(task)
      state = self$graph$state
      state
    },
    .predict = function(task) {
      on.exit({self$graph$state = NULL})
      self$graph$state = self$model
      prediction = self$graph$predict(task)
      assert_list(prediction, types = "Prediction", len = 1,
        .var.name = sprintf("Prediction returned by Graph %s", self$id))
      prediction[[1]]
    },
    get_predict_type = function() {
      # recursively walk backwards through the graph
      get_po_predict_type = function(x) {
        if (!is.null(x$predict_type)) return(x$predict_type)
        prdcssrs = self$graph$edges[dst_id == x$id, ]$src_id
        if (length(prdcssrs)) {
          # all non-null elements
          predict_types = discard(map(self$graph$pipeops[prdcssrs], get_po_predict_type), is.null)
          if (length(unique(predict_types)) == 1L)
            return(unlist(unique(predict_types)))
        }
        return(NULL)
      }
      predict_type = get_po_predict_type(self$graph$pipeops[[self$graph$rhs]])
      if (is.null(predict_type))
        names(mlr_reflections$learner_predict_types[[self$task_type]])[[1]]
      else
        predict_type
    },
    set_predict_type = function(predict_type) {
      # recursively walk backwards through the graph
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
  GraphLearner$new(x, clone_graph = clone)
}

#' @export
as_learner.PipeOp = function(x, clone = FALSE) {
  as_learner(as_graph(x, clone = FALSE), clone = clone)
}


infer_task_type = function(graph) {
  output = graph$output
  # check the high level input and output
  class_table = mlr_reflections$task_types
  input = graph$input
  task_type = c(
    match(c(output$train, output$predict), class_table$prediction),
    match(c(input$train, input$predict), class_table$task))
  task_type = unique(class_table$type[stats::na.omit(task_type)])
  if (length(task_type) > 1L) {
    stopf("GraphLearner can not infer task_type from given Graph\nin/out types leave multiple possibilities: %s", str_collapse(task_type))
  }
  if (length(task_type) == 0L) {
    # recursively walk backwards through the graph
    # FIXME: think more about target transformation graphs here
    get_po_task_type = function(x) {
      task_type = c(
        match(c(x$output$train, x$output$predict), class_table$prediction),
        match(c(x$input$train, x$input$predict), class_table$task))
      task_type = unique(class_table$type[stats::na.omit(task_type)])
      if (length(task_type) > 1) {
        stopf("GraphLearner can not infer task_type from given Graph\nin/out types leave multiple possibilities: %s", str_collapse(task_type))
      }
      if (length(task_type) == 1) {
        return(task_type)  # early exit
      }
      prdcssrs = graph$edges[dst_id == x$id, ]$src_id
      if (length(prdcssrs)) {
        # all non-null elements
        task_types = discard(map(graph$pipeops[prdcssrs], get_po_task_type), is.null)
        if (length(unique(task_types)) == 1L) {
          return(unlist(unique(task_types)))
        }
      }
      return(NULL)
    }
    task_type = get_po_task_type(graph$pipeops[[graph$rhs]])
  }
  c(task_type, "classif")[[1]]  # "classif" as final fallback
}
