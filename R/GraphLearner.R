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
#' * `internal_tuned_values` :: named `list()` or `NULL`\cr
#'   The internal tuned parameter values.
#'   `NULL` is returned if the learner is not trained or none of the wrapped learners supports internal tuning.
#' * `internal_valid_scores` :: named `list()` or `NULL`\cr
#'   The internal tuned parameter values.
#'   `NULL` is returned if the learner is not trained or none of the wrapped learners supports internal validation.
#' * `validate` :: `numeric(1)`, `"predefined"`, `"test"` or `NULL`\cr
#'   How to construct the validation data. This also has to be configured in the individual learners wrapped by
#'   `PipeOpLearner`, see [`set_validate.GraphLearner`] on how to configure this.
#' * `marshaled` :: `logical(1)`\cr
#'   Whether the learner is marshaled.
#'
#' @section Methods:
#' * `marshal(...)`\cr
#'   (any) -> `self`\cr
#'   Marshal the model.
#' * `unmarshal(...)`\cr
#'   (any) -> `self`\cr
#'   Unmarshal the model.
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
#' \dontshow{ if (requireNamespace("rpart")) \{ }
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
#' \dontshow{ \} }
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

      private$.can_validate = some(learner_wrapping_pipeops(graph), function(po) "validation" %in% po$learner$properties)
      private$.can_internal_tuning = some(learner_wrapping_pipeops(graph), function(po) "internal_tuning" %in% po$learner$properties)

      properties = setdiff(mlr_reflections$learner_properties[[task_type]],
        c("validation", "internal_tuning")[!c(private$.can_validate, private$.can_internal_tuning)])

      super$initialize(id = id, task_type = task_type,
        feature_types = mlr_reflections$task_feature_types,
        predict_types = names(mlr_reflections$learner_predict_types[[task_type]]),
        packages = graph$packages,
        properties = properties,
        man = "mlr3pipelines::GraphLearner"
      )

      if (length(param_vals)) {
        private$.graph$param_set$values = insert_named(private$.graph$param_set$values, param_vals)
      }
      if (!is.null(predict_type)) self$predict_type = predict_type
    },
    base_learner = function(recursive = Inf) {
      assert(check_numeric(recursive, lower = Inf), check_int(recursive))
      if (recursive <= 0) return(self)
      gm = self$graph_model
      gm_output = gm$output
      if (nrow(gm_output) != 1) stop("Graph has no unique output.")
      last_pipeop_id = gm_output$op.id

      # pacify static checks
      src_id = NULL
      dst_id = NULL

      repeat {
        last_pipeop = gm$pipeops[[last_pipeop_id]]
        learner_model = if ("learner_model" %in% names(last_pipeop)) last_pipeop$learner_model
        if (!is.null(learner_model)) break
        last_pipeop_id = gm$edges[dst_id == last_pipeop_id, src_id]
        if (length(last_pipeop_id) > 1) stop("Graph has no unique PipeOp containing a Learner")
        if (length(last_pipeop_id) == 0) stop("No Learner PipeOp found.")
      }
      learner_model$base_learner(recursive - 1)
    },
    marshal = function(...) {
      learner_marshal(.learner = self, ...)
    },
    unmarshal = function(...) {
      learner_unmarshal(.learner = self, ...)
    }
  ),
  active = list(
    internal_valid_scores = function(rhs) {
      assert_ro_binding(rhs)
      self$state$internal_valid_scores
    },
    internal_tuned_values = function(rhs) {
      assert_ro_binding(rhs)
      self$state$internal_tuned_values
    },
    validate = function(rhs) {
      if (!missing(rhs)) {
        if (!private$.can_validate) {
          stopf("None of the Learners wrapped by GraphLearner '%s' support validation.", self$id)
        }
        private$.validate = assert_validate(rhs)
      }
      private$.validate
    },
    marshaled = function() {
      learner_marshaled(self)
    },
    hash = function() {
      digest(list(class(self), self$id, self$graph$hash, private$.predict_type, private$.validate,
        self$fallback$hash, self$parallel_predict), algo = "xxhash64")
    },
    phash = function() {
      digest(list(class(self), self$id, self$graph$phash, private$.predict_type, private$.validate,
        self$fallback$hash, self$parallel_predict), algo = "xxhash64")
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
    .validate = NULL,
    .can_validate = NULL,
    .can_internal_tuning = NULL,
    .extract_internal_tuned_values = function() {
      if (!private$.can_validate) return(NULL)
      itvs = unlist(map(
        learner_wrapping_pipeops(self$graph_model), function(po) {
          if (exists("internal_tuned_values", po$learner)) {
            po$learner_model$internal_tuned_values
          }
        }
      ), recursive = FALSE)
      if (is.null(itvs) || !length(itvs)) return(named_list())
      itvs
    },
    .extract_internal_valid_scores = function() {
      if (!private$.can_internal_tuning) return(NULL)
      ivs = unlist(map(
        learner_wrapping_pipeops(self$graph_model), function(po) {
          if (exists("internal_valid_scores", po$learner)) {
            po$learner_model$internal_valid_scores
          }
        }
      ), recursive = FALSE)
      if (is.null(ivs) || !length(ivs)) return(named_list())
      ivs
    },
    deep_clone = function(name, value) {
      private$.param_set = NULL
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
      if (!is.null(get0("validate", self))) {
        some_pipeops_validate = some(learner_wrapping_pipeops(self), function(po) !is.null(get0("validate", po$learner)))
        if (!some_pipeops_validate) {
          lg$warn("GraphLearner '%s' specifies a validation set, but none of its Learners use it.", self$id)
        }
      }

      on.exit({self$graph$state = NULL})
      self$graph$train(task)
      state = self$graph$state
      class(state) = c("graph_learner_model", class(state))
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

#' @title Configure Validation for a GraphLearner
#'
#' @description
#' Configure validation for a graph learner.
#'
#' In a [`GraphLearner`], validation can be configured on two levels:
#' 1. On the [`GraphLearner`] level, which specifies **how** the validation set is constructed before entering the graph.
#' 2. On the level of the [`Learner`]s that are wrapped by [`PipeOpLearner`] and [`PipeOpLearnerCV`], which specifies
#'    which pipeops actually make use of the validation data.
#'    All learners wrapped by [`PipeOpLearner`] and [`PipeOpLearnerCV`] should in almost all cases either set it
#'    to `NULL` (disable) or `"predefined"` (enable).
#'
#' @param learner ([`GraphLearner`])\cr
#'   The graph learner to configure.
#' @param validate (`numeric(1)`, `"predefined"`, `"test"`, or `NULL`)\cr
#'   How to set the `$validate` field of the learner.
#'   If set to `NULL` all validation is disabled, both on the graph learner level, but also for all pipeops.
#' @param ids (`NULL` or `character()`)\cr
#'   For which pipeops to enable validation.
#'   This parameter is ignored when `validate` is set to `NULL`.
#'   By default, validation is enabled for the base learner.
#' @param args (named `list()`)\cr
#'   Rarely needed.
#'   A named list of lists, specifying additional argments to be passed to [`set_validate()`] for the respective learners.
#'   Names must be a subset of the `ids`.
#' @param ... (any)\cr
#'   Currently unused.
#'
#' @export
#' @examples
#' library(mlr3)
#' # simple
#' glrn = as_learner(po("pca") %>>% lrn("classif.debug"))
#' set_validate(glrn, 0.3)
#' glrn$validate
#' glrn$graph$pipeops$classif.debug$learner$validate
#' set_validate(glrn, NULL)
#' glrn$validate
#' glrn$graph$pipeops$classif.debug$learner$validate
#'
#' # complex
#' glrn = as_learner(ppl("stacking", list(lrn("classif.debug"), lrn("classif.featureless")),
#'   lrn("classif.debug", id = "final")))
#' set_validate(glrn, 0.2, ids = c("classif.debug", "final"))
#' glrn$validate
#' glrn$graph$pipeops$classif.debug$learner$validate
#' glrn$graph$pipeops$final$learner$validate
set_validate.GraphLearner = function(learner, validate, ids = NULL, args = list(), ...) {
  if (is.null(validate)) {
    learner$validate = NULL
    walk(learner_wrapping_pipeops(learner), function(po) {
      po$learner$validate = NULL
    })
    return(invisible(learner))
  }

  if (is.null(ids)) {
    ids = base_pipeop(learner)$id
  } else {
    assert_subset(ids, ids(keep(learner_wrapping_pipeops(learner), function(po) "validation" %in% po$learner$properties)))
  }

  assert_list(args, types = "list")
  assert_subset(names(args), ids)

  prev_validate_pos = discard(map(learner_wrapping_pipeops(learner), function(po) get0("validate", po$learner, ifnotfound = NA)),
    function(x) identical(x, NA))

  prev_validate = learner$validate

  on.exit({
    iwalk(prev_validate_pos, function(val, poid) learner$graph$pipeops[[poid]]$learner$validate = val)
    learner$validate = prev_validate
  }, add = TRUE)

  learner$validate = validate

  walk(ids, function(poid) {
    # learner might be another GraphLearner / AutoTuner so we call into set_validate() again
    withCallingHandlers({
      invoke(set_validate, learner = learner$graph$pipeops[[poid]]$learner, .args = insert_named(list(validate = "predefined"), args[[poid]]))
    }, error = function(e) {
      e$message = sprintf("Failed to set validate for PipeOp '%s':\n%s", poid, e$message)
      stop(e)
    }, warning = function(w) {
      w$message = sprintf("Failed to set validate for PipeOp '%s':\n%s", po$id, w$message)
      warning(w)
      invokeRestart("muffleWarning")
    })
  })
  on.exit()

  invisible(learner)
}


#' @export
disable_internal_tuning.GraphLearner = function(learner, ids, ...) {
  pvs = learner$param_set$values
  on.exit({learner$param_set$values = pvs}, add = TRUE)
  if (length(ids)) {
    walk(learner_wrapping_pipeops(learner), function(po) {
      disable_internal_tuning(po$learner, ids = po$param_set$ids()[sprintf("%s.%s", po$id, po$param_set$ids()) %in% ids])
    })
  }
  on.exit()
  invisible(learner)
}


#' @export
marshal_model.graph_learner_model = function(model, inplace = FALSE, ...) {
  xm = map(.x = model, .f = marshal_model, inplace = inplace, ...)
  # if none of the states required any marshaling we return the model as-is
  if (!some(xm, is_marshaled_model)) return(model)

  structure(list(
    marshaled = xm,
    packages = "mlr3pipelines"
  ), class = c("graph_learner_model_marshaled", "list_marshaled", "marshaled"))
}

#' @export
unmarshal_model.graph_learner_model_marshaled = function(model, inplace = FALSE, ...) {
  # need to re-create the class as it gets lost during marshaling
  structure(
    map(.x = model$marshaled, .f = unmarshal_model, inplace = inplace, ...),
    class = gsub(x = head(class(model), n = -1), pattern = "_marshaled$", replacement = "")
  )
}

#' @export
as_learner.Graph = function(x, clone = FALSE, ...) {
  GraphLearner$new(x, clone_graph = clone)
}

#' @export
as_learner.PipeOp = function(x, clone = FALSE, ...) {
  as_learner(as_graph(x, clone = FALSE, ...), clone = clone)
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
