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


      private$.can_validate = some(
        keep(graph$pipeops, function(x) inherits(x, "PipeOpLearner") || inherits(x, "PipeOpLearnerCV")),
        function(po) "validation" %in% po$learner$properties
      )

      inner_tuning = some(
        keep(graph$pipeops, function(x) inherits(x, "PipeOpLearner") || inherits(x, "PipeOpLearnerCV")),
        function(po) "inner_tuning" %in% po$learner$properties
      )

      properties = setdiff(mlr_reflections$learner_properties[[task_type]],
        c("validation", "inner_tuning")[!c(private$.validate, inner_tuning)])

      super$initialize(id = id, task_type = task_type,
        feature_types = mlr_reflections$task_feature_types,
        predict_types = names(mlr_reflections$learner_predict_types[[task_type]]),
        packages = graph$packages,
        properties = properties,
        man = "mlr3pipelines::GraphLearner"
      )

      private$.param_set = NULL

      if (length(param_vals)) {
        private$.graph$param_set$values = insert_named(private$.graph$param_set$values, param_vals)
      }
      if (!is.null(predict_type)) self$predict_type = predict_type
    },
    base_learner = function(recursive = Inf) {
      self$base_pipeop(recursive = recursive)$learner_model
    },
    base_pipeop = function(recursive = Inf) {
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
      last_pipeop$base_pipeop(recursive - 1)

    },

    #' @description
    #' Retrieves the inner validation scores as a named `list()`.
    inner_valid_scores = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(self$state)) {
        stopf("Learner not trained")
      }
      self$state$inner_valid_scores
    },
    #' @description
    #' Retrieves the inner tuned values as a named `list()`.
    inner_tuned_values = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(self$state)) {
        stopf("Learner not trained")
      }
      self$state$inner_tuned_values
    }
  ),
  active = list(
    validate = function(rhs) {
      if (!missing(rhs)) {
        if (!private$.can_validate) {
          stopf("None of the Learners wrapped by GraphLearner '%s' support validation.", self$id)
        }
        private$.validate = assert_validate(rhs)
      }
      private$.validate

    },
    hash = function() {
      digest(list(class(self), self$id, self$graph$hash, private$.predict_type,
        self$fallback$hash, self$parallel_predict), algo = "xxhash64")
    },
    phash = function() {
      digest(list(class(self), self$id, self$graph$phash, private$.predict_type,
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
    .extract_inner_tuned_values = function() {


      warningf("Implementthis")
      list()

    },
    .extract_inner_valid_scores = function() {
      warningf("Implementthis")
      list()
      # map(
      #   keep(self$graph$pipeops, function(po) inherits(po, "PipeOpLearnerCV") || inherits(po, "PipeOpLearner")),
      #   function(po) {
      #     po$inner_
      #   }
      # )
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
        some_pipeops_validate = map(
          filter(self$graph$pipeops, function(po) inherits(po, "PipeOpLearner") || inherits(po, "PipeOpLearnerCV")),
          function(po) !is.null(get0("validate", po$learner))
        )

        if (!some_pipeops_validate) {
          lg$warn("GraphLearner '%s' specifies a validation set, but none of its Learners use it.", self$id)
        }
      }

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

#' @title Configure Validation for a GraphLearner
#'
#' @description
#' Configure validation for a graph learner.
#'
#' In a [`GraphLearner`], validation can be configured on two levels:
#' 1. On the [`GraphLearner`] level.
#' 2. On the level of the [`Learner`]s that are wrapped by [`PipeOpLearner`] and [`PipeOpLearnerCV`].
#'
#' Therefore, enabling validation requires to specify not only how to create the validation set (1), but also which
#' pipeops should actually use it.
#' Only the [`GraphLearner`] can specify **how** to create the validation set.
#' All learners wrapped by [`PipeOpLearner`] and [`PipeOpLearnerCV`] can only set it to `NULL` (disable) or
#' `"inner_valid"` (enable).
#'
#' @param learner ([`GraphLearner`])\cr
#'   The graph learner to configure.
#' @param validate (`numeric(1)`, `"inner_valid"` or `NULL`)\cr
#'   How to set the `$validate` field of the learner.
#'   If set to `NULL` all validation is disabled.
#' @param ids (`NULL` or `character()`)\cr
#'   For which pipeops to enable validation.
#'   This parameter is ignored when `validate` is set to `NULL`.
#'   By default, validation is enabled for the base learner.
#' @param args (named `list()`)\cr
#'   Rarely needed.
#'   A named list of lists, specifying additional argments to be passed to [`set_validate()`] for the pipeops.
#'   The names must be a subset of `ids`.
#' @export
#' @examples
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
#' glrn = as_learner(ppl("stacking", lrns(c("classif.debug", "classif.featureless")), lrn("classif.debug", id = "final")))
#' set_validate(glrn, 0.2, which = c("classif.debug", "final"))
#' glrn$validate
#' glrn$graph$pipeops$classif.debug$learner$validate
#' glrn$graph$pipeops$final$learner$validate
set_validate.GraphLearner = function(learner, validate, ids = NULL, args = list()) {
  if (is.null(learner$validate)) {
    learner$validate = NULL
    walk(learner_wrapping_pipeops(learner), function(po) {
      if (exists("validate", po$learner)) {
        po$learner$validate = NULL
      }
    })
    return(invisible(learner))
  }

  if (is.null(ids)) {
    which = learner$base_pipeop()$id
  } else {
    assert_subset(ids, ids(keep(learner_wrapping_pipeops(learner), function(po) "validation" %in% po$learner$properties)))
    assert_true(length(ids) > 0)
  }

  assert_list(args, types = "list")
  assert_subset(names(args), ids)

  prev_validate_pos = discard(map(learner_wrapping_pipeops(learner), function(po) get0("validate", po$learner), is.null))
  prev_validate = learner$validate

  on.exit({
    iwalk(prev_validate_pos, function(val, poid) learner$graph$pipeops[[poid]] = val)
    learner$valiate = prev_validate
  }, add = TRUE)

  learner$validate = validate

  walk(ids, function(poid) {
    # learner might be another GraphLearner / AutoTuner
    invoke(set_validate learner = learner$graph$pipeops[[poid]]$learner, validate = "inner_valid", .args = args[[poid]])
  })
  on.exit()

  invisible(learner)
}


#' @title Set Inner Tuning of a GraphLearner
#' @description
#' First, all values specified by `...` are
#' All [`PipeOpLearner`] and [`PipeOpLearnerCV`]
#' @param validate (`numeric(1)`, `"inner_valid"`, or `NULL`)\cr
#'   How to set the `$validate` field of the learner.
#' @param args (named `list()`)\cr
#'   Names are ids of the [`GraphLearner`]'s `PipeOps` and values are lists containing arguments passed to the
#'   respective wrapped [`Learner`].
#'   By default, the values `.disable` and `validate` are used, but can be overwritten on a per-pipeop basis.
#'
#'   When enabling, the inner tuning of the `$base_learner()` is enabled by default.
#'   When disabling, all inner tuning is disable by default.
#' @export
set_inner_tuning.GraphLearner = function(.learner, .disable = FALSE, validate = NA, args = NULL, ...) {
  if (is.null(args)) {
    args = set_names(list(list()), .learner$base_pipeops()$id
  }
  all_pipeops = .learner$graph$pipeops
  lrn_pipeops = learner_wrapping_pipeops(all_pipeops)

  assert_list(args, names = "unique")
  assert_subset(names(args), ids(lrn_pipeops))


  # clean up when something goes wrong
  prev_pvs = .learner$param_set$values
  prev_validate = discard(map(lrn_pipeops, function(po) if (exists("validate", po$learner)) po$learner$validate), is.null)
  on.exit({
    .learner$param_set$set_values(.values = prev_pvs)
    iwalk(prev_validate, function(val, poid) .learner$graph$pipeops[[poid]]$learner$validate = val)
  }, add = TRUE)

  walk(lrn_pipeops[names(args)], function(po) {
    browser()
    invoke(set_inner_tuning, .learner = po$learner,
      .args = insert_named(list(validate = validate, .disable = .disable), args[[po$id]])
    )
  })

  # Now:
  # Set validate for GraphLearner and verify that the configuration is reasonable

  if (.disable) {
    .learner$validate = if (identical(validate, NA)) NULL else validate
    some_pipeops_validate = some(lrn_pipeops, function(po) {
      if (!exists("validate", po$learner)) {
        return(FALSE)
      }
      !is.null(po$learner$validate)
    })
    # if none of the pipeops does validation, we also disable it in the GraphLearner
    # (unless a value was explicitly specified)
    if (!some_pipeops_validate && identical(validate, NA)) {
      .learner$validate = NULL
    }
  } else {
    if (!identical(validate, NA)) {
      .learner$validate = validate
    }

    some_pipeops_validate = some(lrn_pipeops, function(po) {
      if (is.null(get0("validate", po$learner))) return(FALSE)
      if (is.null(.learner$validate)) {
        warningf("PipeOp '%s' from GraphLearner '%s' wants a validation set but GraphLearner does not specify one. This likely not what you want.",
          po$id, .learner$id)
      }
      if (!identical(po$learner$validate, "inner_valid")) {
        warningf("PipeOp '%s' from GraphLearner '%s' specifies validation set other than 'inner_valid'. This is likely not what you want.",
          po$id, .learner$id)
      }
      TRUE
    })

    if (!is.null(.learner$param_set$values$validate) && !some_pipeops_validate) {
      warningf("GraphLearner '%s' specifies a validation set, but none of its Learners use it. This is likely not what you want.", .learner$id)
    }
  }

  on.exit()
  invisible(.learner)
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
