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


      private$.validate = some(
        keep(graph$pipeops, function(x) inherits(x, "PipeOpLearner") || inherits(x, "PipeOpLearnerCV")),
        function(po) "validation" %in% po$learner$properties
      )

      inner_tuning = some(
        keep(graph$pipeops, function(x) inherits(x, "PipeOpLearner") || inherits(x, "PipeOpLearnerCV")),
        function(po) "inner_tuning" %in% po$learner$properties
      )

      properties = setdiff(mlr_reflections$learner_properties[[task_type]],
        c("validation", "inner_tuning")[c(!private$.validate, !inner_tuning)])

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
      if (is.null(private$.param_set)) {
        private$.param_set = ParamSetCollection$new(sets = c(list(self$graph$param_set),
          if (private$.validate) ps(validate = p_uty(default = NULL, tags = "train", custom_check = check_validate)
        )))
      }
      private$.param_set
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
    .param_set = NULL,
    .extract_inner_tuned_values = function() {

    },
    .extract_inner_valid_scores = function() {
      .NotYetImplemented()
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


#' @param ids (`character(1)`)\cr
#'   The ids of the parameters to disable.
#'   When enabling, the inner tuning of the `$base_learner()` is enabled by default.
#'   When disabling, all inner tuning is disable by default.
#' @export
set_inner_tuning.GraphLearner = function(learner, disable = FALSE, ids = NULL, param_vals = list(), ...) {
  all_pipeops = learner$graph$pipeops
  lrn_pipeops = all_pipeops[inherits(all_pipeops, "PipeOpLearner") | inherits(all_pipeops, "PipeOpLearnerCV")]

  if (is.null(ids) && disable) {
    ids = as.character(unlist(imap(lrn_pipeops, function(po, prefix) {
      sprintf("%s.%s", prefix, names(po$param_set$tags[map_lgl(po$param_set$tags, function(t) "inner_tuning" %in% t)]))
    })))
  } else if (is.null(ids) && !disable) {
    lrn_base = learner$base_learner()

    # need to find the pipeop that is the base learner. Cannot directly use id, because id of pipeop might
    # differ from learner id
    po_baselrn = NULL
    for (po in lrn_pipeops[inherits(po, "PipeOpLearner")]) {
      if (identical(po$learner, lrn_base)) {
        po_baselrn = po
        break
      }
    }
    ids = paste0(
      po_baselrn$id, ".",
      names(po_baselrn$param_set$tags[map_lgl(po_baselrn$param_set$tags, function(tags) "inner_tuning" %in% tags)])
    )
  }
  assert_subset(ids, learner$param_set$ids())
  pv_prev = learner$param_set$values

  # reset to previous pvs if anything goes wrong
  on.exit({learner$param_set$set_values(.values = pv_prev)}, add = TRUE)

  learner$param_set$set_values(.values = param_vals)


  # pipeop_ids are those learners that wrap a learner and have a parameter that is containes in ids
  po_ids = as.character(unlist(discard(map(lrn_pipeops, function(po) {
    if (some(names(param_vals) %in% sprintf("%s.%s", po$id, po$param_set$ids()))) po$id
  }), is.null)))

  # now we walk through the learners and call set_inner_tuning() WITHOUT passing the parameters, as we have already
  # set them above
  walk(lrn_pipeops[po_ids], set_inner_tuning, disable = disable)

  # now put up some extra guardrails because it is not intuitive how to configure validation in the GraphLearner

  some_pipeops_validate = FALSE
  if (disable) {
    for (po in lrn_pipeops) {
      if (!is.null(po$param_set$values$validate)) {
        some_pipeops_validate = TRUE
        break
      }
    }
    # if none of the pipeops does validation, we also disable it in the GraphLearner
    # (unless a value was explicitly passed via param_vals)
    if (!some_pipeops_validate && is.null(param_vals$validate)) {
      learner$param_set$set_values(validate = NULL)
    }
  } else {
    for (po in lrn_pipeops) {
      if (!is.null(po$param_set$values$validate) && is.null(learner$param_set$values$validate)) {
        warningf("PipeOp '%s' from GraphLearner '%s' wants a validation set but GraphLearner does not specify one. This likely not what you want.",
          po$id, learner$id)
      }
      if (!is.null(po$param_set$values$validate)) {
        if (!identical(po$param_set$values$validate, "inner_valid")) {
          warningf("PipeOp '%s' from GraphLearner '%s' specifies validation set other than 'inner_valid'. This is likely not what you want.")
        }
        some_pipeops_validate = TRUE
      }
    }
    if (!is.null(learner$param_set$values$validate) && !some_pipeops_validate) {
      warningf("GraphLearner '%s' specifies a validation set, but none of its Learners use it.", learner$id)
    }
  }

  on.exit()
  invisible(learner)
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
