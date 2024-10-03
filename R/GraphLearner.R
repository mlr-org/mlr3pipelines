#' @title Encapsulate a Graph as a Learner
#'
#' @name mlr_learners_graph
#' @format [`R6Class`][R6::R6Class] object inheriting from [`mlr3::Learner`].
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
#' Fields inherited from [`Learner`][mlr3::Learner], as well as:
#' * `graph` :: [`Graph`]\cr
#'   [`Graph`] that is being wrapped. This field contains the prototype of the [`Graph`] that is being trained, but does *not*
#'   contain the model. Use `graph_model` to access the trained [`Graph`] after `$train()`. Read-only.
#' * `graph_model` :: [`Learner`][mlr3::Learner]\cr
#'   [`Graph`] that is being wrapped. This [`Graph`] contains a trained state after `$train()`. Read-only.
#' * `pipeops` :: named `list` of [`PipeOp`] \cr
#'   Contains all [`PipeOp`]s in the underlying [`Graph`], named by the [`PipeOp`]'s `$id`s. Read-only.
#' * `edges` :: [`data.table`][data.table::data.table]  with columns `src_id` (`character`), `src_channel` (`character`), `dst_id` (`character`), `dst_channel` (`character`)\cr
#'   Table of connections between the [`PipeOp`]s in the underlying [`Graph`]. A [`data.table`][data.table::data.table]. `src_id` and `dst_id` are `$id`s of [`PipeOp`]s that must be present in
#'   the `$pipeops` list. `src_channel` and `dst_channel` must respectively be `$output` and `$input` channel names of the
#'   respective [`PipeOp`]s.
#' * `param_set` :: [`ParamSet`][paradox::ParamSet]\cr
#'
#' * pipeops_param_set
#' * pipeops_param_set_values
#' * `internal_tuned_values` :: named `list()` or `NULL`\cr
#'   The internal tuned parameter values collected from all `PipeOp`s.
#'   `NULL` is returned if the learner is not trained or none of the wrapped learners supports internal tuning.
#' * `internal_valid_scores` :: named `list()` or `NULL`\cr
#'   The internal validation scores as retrieved from the `PipeOps`.
#'   The names are prefixed with the respective IDs of the `PipeOp`s.
#'   `NULL` is returned if the learner is not trained or none of the wrapped learners supports internal validation.
#' * `validate` :: `numeric(1)`, `"predefined"`, `"test"` or `NULL`\cr
#'   How to construct the validation data. This also has to be configured for the individual `PipeOp`s such as
#'   `PipeOpLearner`, see [`set_validate.GraphLearner`].
#'   For more details on the possible values, see [`mlr3::Learner`].
#' * `marshaled` :: `logical(1)`\cr
#'   Whether the learner is marshaled.
#' * `impute_selected_features` :: `logical(1)`\cr
#'   Whether to heuristically determine `$selected_features()` as all `$selected_features()` of all "base learner" Learners,
#'   even if they do not have the `"selected_features"` property / do not implement `$selected_features()`.
#'   If `impute_selected_features` is `TRUE` and the base learners do not implement `$selected_features()`,
#'   the `GraphLearner`'s `$selected_features()` method will return all features seen by the base learners.
#'   This is useful in cases where feature selection is performed inside the `Graph`:
#'   The `$selected_features()` will then be the set of features that were selected by the `Graph`.
#'   If `impute_selected_features` is `FALSE`, the `$selected_features()` method will throw an error if `$selected_features()`
#'   is not implemented by the base learners.\cr
#'   This is a heuristic and may report more features than actually used by the base learners,
#'   in cases where the base learners do not implement `$selected_features()`.
#'   The default is `FALSE`.
#'
#' @section Methods:
#' Methods inherited from [`Learner`][mlr3::Learner], as well as:
#' * `ids(sorted = FALSE)` \cr
#'   (`logical(1)`) -> `character` \cr
#'   Get IDs of all [`PipeOp`]s. This is in order that [`PipeOp`]s were added if
#'   `sorted` is `FALSE`, and topologically sorted if `sorted` is `TRUE`.
#' * `plot(html = FALSE, horizontal = FALSE)` \cr
#'   (`logical(1)`, `logical(1)`) -> `NULL` \cr
#'   Plot the [`Graph`], using either the \pkg{igraph} package (for `html = FALSE`, default) or
#'   the `visNetwork` package for `html = TRUE` producing a [`htmlWidget`][htmlwidgets::htmlwidgets].
#'   The [`htmlWidget`][htmlwidgets::htmlwidgets] can be rescaled using [`visOptions`][visNetwork::visOptions].
#'   For `html = FALSE`, the orientation of the plotted graph can be controlled through `horizontal`.
#' * `marshal`\cr
#'   (any) -> `self`\cr
#'   Marshal the model.
#' * `unmarshal`\cr
#'   (any) -> `self`\cr
#'   Unmarshal the model.
#' * `base_learner(recursive = Inf, return_po = FALSE, return_all = FALSE, resolve_branching = TRUE)`\cr
#'   (`numeric(1)`, `logical(1)`, `logical(1)`, `character(1)`) -> `Learner` | [`PipeOp`] | `list` of `Learner` | `list` of [`PipeOp`]\cr
#'   Return the base learner of the `GraphLearner`. If `recursive` is 0, the `GraphLearner` itself is returned.
#'   Otherwise, the [`Graph`] is traversed backwards to find the first `PipeOp` containing a `$learner_model` field.
#'   If `recursive` is 1, that `$learner_model` (or containing `PipeOp`, if `return_po` is `TRUE`) is returned.
#'   If `recursive` is greater than 1, the discovered base learner's `base_learner()` method is called with `recursive - 1`.
#'   `recursive` must be set to 1 if `return_po` is TRUE, and must be set to at most 1 if `return_all` is `TRUE`.\cr
#'   If `return_po` is `TRUE`, the container-`PipeOp` is returned instead of the `Learner`.
#'   This will typically be a [`PipeOpLearner`] or a [`PipeOpLearnerCV`].\cr
#'   If `return_all` is `TRUE`, a `list` of `Learner`s or `PipeOp`s is returned.
#'   If `return_po` is `FALSE`, this list may contain [`Multiplicity`] objects, which are not unwrapped.
#'   If `return_all` is `FALSE` and there are multiple possible base learners, an error is thrown.
#'   This may also happen if only a single [`PipeOpLearner`] is present that was trained with a [`Multiplicity`].\cr
#'   If `resolve_branching` is `TRUE`, and when a [`PipeOpUnbranch`] is encountered, the
#'   corresponding [`PipeOpBranch`] is searched, and its hyperparameter configuration is used to select the base learner.
#'   There may be multiple corresponding [`PipeOpBranch`]s, which are all considered.
#'   If `resolve_branching` is `FALSE`, [`PipeOpUnbranch`] is treated as any other `PipeOp` with multiple inputs; all possible branch paths are considered equally.
#'
#' The following standard extractors as defined by the [`Learner`][mlr3::Learner] class are available.
#' Note that these typically only extract information from the `$base_learner()`.
#' This works well for simple [`Graph`]s that do not modify features too much, but may give unexpected results for `Graph`s that
#' add new features or move information between features.
#'
#' As an example, consider a feature `A` with missing values, and a feature `B` that is used for imputation, using a [`po("imputelearner")`][PipeOpImputeLearner].
#' In a case where the following [`Learner`][mlr3::Learner] performs embedded feature selection and only selects feature `A`,
#' the `selected_features()` method could return only feature `A`, and `$importance()` may even report 0 for feature `B`.
#' This would not be entirely accurate when considering the entire `GraphLearner`, as feature `B` is used for imputation and would therefore have an impact on predictions.
#' The following should therefore only be used if the [`Graph`] is known to not have an impact on the relevant properties.
#'
#' * `importance()`\cr
#'   () -> `numeric`\cr
#'   The `$importance()` returned by the base learner, if it has the `"importance` property.
#'   Throws an error otherwise.
#' * `selected_features()`\cr
#'   () -> `character`\cr
#'   The `$selected_features()` returned by the base learner, if it has the `"selected_features` property.
#'   If the base learner does not have the `"selected_features"` property and `impute_selected_features` is `TRUE`,
#'   all features seen by the base learners are returned.
#'   Throws an error otherwise.
#' * `oob_error()`\cr
#'   () -> `numeric(1)`\cr
#'   The `$oob_error()` returned by the base learner, if it has the `"oob_error` property.
#'   Throws an error otherwise.
#' * `loglik()`\cr
#'   () -> `numeric(1)`\cr
#'   The `$loglik()` returned by the base learner, if it has the `"loglik` property.
#'   Throws an error otherwise.
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
    impute_selected_features = FALSE,
    initialize = function(graph, id = NULL, param_vals = list(), task_type = NULL, predict_type = NULL, clone_graph = TRUE) {
      graph = as_graph(graph, clone = assert_flag(clone_graph))
      graph$state = NULL

      id = assert_string(id, null.ok = TRUE) %??% paste(graph$ids(sorted = TRUE), collapse = ".")
      self$id = id  # init early so 'base_learner()' can use it in error messages
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

      private$.can_validate = some(graph$pipeops, function(po) "validation" %in% po$properties)
      private$.can_internal_tuning = some(graph$pipeops, function(po) "internal_tuning" %in% po$properties)

      baselearners = unlist(multiplicity_flatten(self$base_learner(recursive = 1, return_all = TRUE, resolve_branching = FALSE)), recursive = FALSE, use.names = FALSE)
      blproperties = unique(unlist(map(baselearners, "properties"), recursive = FALSE, use.names = FALSE))

      properties = setdiff(mlr_reflections$learner_properties[[task_type]],
        c("validation", "internal_tuning", "importance", "oob_error", "loglik"))

      properties = c(properties,
        if (private$.can_validate) "validation",
        if (private$.can_internal_tuning) "internal_tuning",
        intersect(c("importance", "oob_error", "loglik"), blproperties)
      )

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
    base_learner = function(recursive = Inf, return_po = FALSE, return_all = FALSE, resolve_branching = TRUE) {
      assert(check_numeric(recursive, lower = Inf), check_int(recursive))
      assert_flag(return_po)
      assert_flag(return_all)
      if (return_po && recursive != 1) stop("recursive must be == 1 if return_po is TRUE")
      if (recursive <= 0) return(if (return_all) list(self) else self)
      if (return_all && recursive > 1) stop("recursive must be <= 1 if return_all is TRUE")

      # graph_base_learner() corresponds to base_learner(recursive = 1, return_po = TRUE, return_all = TRUE)
      result = graph_base_learner(self$graph_model, resolve_branching = resolve_branching)

      if (!return_all) {
        if (length(result) < 1) stopf("No base learner found in Graph %s.", self$id)
        if (length(result) > 1) stopf("Graph %s has no unique PipeOp containing a Learner.", self$id)
        if (!return_po) {
          result = multiplicity_flatten(result[[1]]$learner_model)
          if (length(result) != 1) {
            # if learner_model is not a Multiplicity, multiplicity_flatten will return a list of length 1
            stopf("Graph %s's base learner is a Multiplicity that does not contain exactly one Learner.", self$id)
          }
          return(result[[1]]$base_learner(recursive - 1))
        } else {
          return(result[[1]])
        }
      }
      # if we are here, return_all is TRUE, and recursive is therefore 1.
      if (!return_po) {
        result = map(result, "learner_model")
      }
      result
    },
    marshal = function(...) {
      learner_marshal(.learner = self, ...)
    },
    unmarshal = function(...) {
      learner_unmarshal(.learner = self, ...)
    },
    importance = function() {
      base_learner = self$base_learner(recursive = 1)
      if ("importance" %in% base_learner$properties && !is.null(base_learner$importance)) {
        base_learner$importance()
      } else {
        stopf("Baselearner %s of %s does not implement '$importance()'.", base_learner$id, self$id)
      }
    },
    selected_features = function() {
      base_learners = self$base_learner(recursive = 1, return_all = TRUE)
      base_learners_flat = unlist(lapply(base_learners, multiplicity_flatten), recursive = FALSE, use.names = FALSE)
      selected_features_all = lapply(base_learners_flat, function(x) {
        if ("selected_features" %in% x$properties && !is.null(x$selected_features)) {
          x$selected_features()
        } else if (self$impute_selected_features) {
          if (is.null(x$state)) {
            stopf("No model stored in base learner %s of Graph %s.", x$id, self$id)
          }
          x$state$feature_names
        } else {
          stopf("Baselearner %s of %s does not implement 'selected_features'.\nYou can try setting $impute_selected_features to TRUE.", x$id, self$id)
        }
      })
      unique(unlist(selected_features_all, recursive = FALSE, use.names = FALSE))
    },
    oob_error = function() {
      base_learner = self$base_learner(recursive = 1)
      if ("oob_error" %in% base_learner$properties && !is.null(base_learner$oob_error)) {
        base_learner$oob_error()
      } else {
        stopf("Baselearner %s of %s does not implement '$oob_error()'.", base_learner$id, self$id)
      }
    },
    loglik = function() {
      base_learner = self$base_learner(recursive = 1)
      if ("loglik" %in% base_learner$properties && !is.null(base_learner$loglik)) {
        base_learner$loglik()
      } else {
        stopf("Baselearner %s of %s does not implement '$loglik()'.", base_learner$id, self$id)
      }
    },
    ids = function(sorted = FALSE) {
      private$.graph$ids(sorted = sorted)
    },
    plot = function(html = FALSE, horizontal = FALSE, ...) {
      private$.graph$plot(html = html, horizontal = horizontal, ...)
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
          stopf("None of the PipeOps in Graph '%s' supports validation.", self$id)
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
        assert_subset(rhs, unlist(mlr_reflections$learner_predict_types[[self$task_type]], use.names = FALSE))
      }

      # we look for *all* pipeops with a predict_type if we want to set it, but
      # we only retrieve the predict_type of the active Learner (from branching) if we
      # are getting.
      predict_type_pipeops = graph_base_learner(
        self$graph, resolve_branching = missing(rhs), lookup_field = "predict_type")
      if (!missing(rhs)) {
        walk(predict_type_pipeops, function(po) po$predict_type = rhs)
        return(rhs)
      }
      pt = unique(unlist(map(predict_type_pipeops, "predict_type"), recursive = FALSE, use.names = FALSE))
      if (!length(pt)) return(names(mlr_reflections$learner_predict_types[[self$task_type]])[[1]])
      if (length(pt) > 1) {
        # if there are multiple predict types, predict the "first" one, according to reflections
        return(intersect(names(mlr_reflections$learner_predict_types[[self$task_type]]), pt)[[1]])
      }
      pt
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
    },
    pipeops = function(rhs) {
      if (!missing(rhs) && (!identical(rhs, private$.graph$pipeops || !identical(rhs, self$graph_model$pipeops)))) {
        stop("pipeops is read-only")
      }
      if (is.null(self$model)) {
        private$.graph$pipeops
      } else {
        self$graph_model$pipeops
      }
    },
    edges = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.graph$edges)) {
        stop("edges is read-only")
      }
      private$.graph$edges
    },
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, self$graph$param_set)) {
        stop("param_set is read-only.")
      }
      self$graph$param_set
    },
    pipeops_param_set = function(rhs) {
      value = map(self$graph$pipeops, "param_set")
      if (!missing(rhs) && !identical(value, rhs)) {
        stop("pipeops_param_set is read-only")
      }
      value
    },
    pipeops_param_set_values = function(rhs) {
      value = map(glrn$pipeops, function(x) x$param_set$values)
      if (!missing(rhs) && !identical(value, rhs)) {
        stop("pipeops_param_set_values is read-only")
      }
      value
    }
  ),
  private = list(
    .graph = NULL,
    .validate = NULL,
    .can_validate = NULL,
    .can_internal_tuning = NULL,
    .extract_internal_tuned_values = function() {
      if (!private$.can_validate) return(NULL)
      itvs = unlist(map(pos_with_property(self$graph_model, "internal_tuning"), "internal_tuned_values"), recursive = FALSE)
      if (!length(itvs)) return(named_list())
      itvs
    },
    .extract_internal_valid_scores = function() {
      if (!private$.can_internal_tuning) return(NULL)
      ivs = unlist(map(pos_with_property(self$graph_model, "validation"), "internal_valid_scores"), recursive = FALSE)
      if (!length(ivs)) return(named_list())
      ivs
    },
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
      if (!is.null(get0("validate", self))) {
        some_pipeops_validate = some(pos_with_property(self, "validation"), function(po) !is.null(po$validate))
        if (!some_pipeops_validate) {
          lg$warn("GraphLearner '%s' specifies a validation set, but none of its PipeOps use it.", self$id)
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
#' 2. On the level of the individual `PipeOp`s (such as `PipeOpLearner`), which specifies
#'    which pipeops actually make use of the validation data (set its `$validate` field to `"predefined"`) or not (set it to `NULL`).
#'    This can be specified via the argument `ids`.
#'
#' @param learner ([`GraphLearner`])\cr
#'   The graph learner to configure.
#' @param validate (`numeric(1)`, `"predefined"`, `"test"`, or `NULL`)\cr
#'   How to set the `$validate` field of the learner.
#'   If set to `NULL` all validation is disabled, both on the graph learner level, but also for all pipeops.
#' @param ids (`NULL` or `character()`)\cr
#'   For which pipeops to enable validation.
#'   This parameter is ignored when `validate` is set to `NULL`.
#'   By default, validation is enabled for the final `PipeOp` in the `Graph`.
#' @param args_all (`list()`)\cr
#'  Rarely needed. A named list of parameter values that are passed to all subsequet [`set_validate()`][mlr3::set_validate] calls on the individual
#'  `PipeOp`s.
#' @param args (named `list()`)\cr
#'   Rarely needed.
#'   A named list of lists, specifying additional argments to be passed to [`set_validate()`][mlr3::set_validate] when calling it on the individual
#'   `PipeOp`s.
#' @param ... (any)\cr
#'   Currently unused.
#'
#' @export
#' @examples
#' library(mlr3)
#'
#' glrn = as_learner(po("pca") %>>% lrn("classif.debug"))
#' set_validate(glrn, 0.3)
#' glrn$validate
#' glrn$graph$pipeops$classif.debug$learner$validate
#'
#' set_validate(glrn, NULL)
#' glrn$validate
#' glrn$graph$pipeops$classif.debug$learner$validate
#'
#' set_validate(glrn, 0.2, ids = "classif.debug")
#' glrn$validate
#' glrn$graph$pipeops$classif.debug$learner$validate
set_validate.GraphLearner = function(learner, validate, ids = NULL, args_all = list(), args = list(), ...) {
  prev_validate_pos = map(pos_with_property(learner$graph$pipeops, "validation"), "validate")
  prev_validate = learner$validate
  on.exit({
    iwalk(prev_validate_pos, function(prev_val, poid) {
      # Here we don't call into set_validate() as this also does not ensure that we are able to correctly
      # reset the configuration to the previous state, is less transparent and might fail again
      # The error message informs the user about this though via the calling handlers below
      learner$graph$pipeops[[poid]]$validate = prev_val
    })
    learner$validate = prev_validate
  }, add = TRUE)

  if (is.null(validate)) {
    learner$validate = NULL
    walk(pos_with_property(learner$graph$pipeops, "validation"), function(po) {
      invoke(set_validate, po, validate = NULL, args_all = args_all, args = args[[po$id]] %??% list())
    })
    on.exit()
    return(invisible(learner))
  }

  if (is.null(ids)) {
    ids = learner$base_learner(recursive = 1, return_po = TRUE)$id
  } else {
    assert_subset(ids, ids(pos_with_property(learner$graph$pipeops, "validation")))
  }

  assert_list(args, types = "list")
  assert_list(args_all)
  assert_subset(names(args), ids)

  learner$validate = validate

  walk(ids, function(poid) {
    # learner might be another GraphLearner / AutoTuner so we call into set_validate() again
    withCallingHandlers({
      args = insert_named(insert_named(list(validate = "predefined"), args_all), args[[poid]])
      invoke(set_validate, learner$graph$pipeops[[poid]], .args = args)
    }, error = function(e) {
      e$message = sprintf(paste0(
        "Failed to set validate for PipeOp '%s':\n%s\n",
        "Trying to heuristically reset validation to its previous state, please check the results"), poid, e$message)
      stop(e)
    }, warning = function(w) {
      w$message = sprintf(paste0(
        "Failed to set validate for PipeOp '%s':\n%s\n",
        "Trying to heuristically reset validation to its previous state, please check the results"), poid, w$message)
      warning(w)
      invokeRestart("muffleWarning")
    })
  })
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
    pipeops_visited = new.env(parent = emptyenv())
    # recursively walk backwards through the graph
    # FIXME: think more about target transformation graphs here
    get_po_task_type = function(x) {
      if (get0(x$id, pipeops_visited, ifnotfound = FALSE)) return(NULL)
      assign(x$id, TRUE, pipeops_visited)
      task_type = c(
        match(c(x$output$train, x$output$predict), class_table$prediction),
        match(c(x$input$train, x$input$predict), class_table$task))
      task_type = unique(class_table$type[stats::na.omit(task_type)])
      if (length(task_type) >= 1) {
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
  if (length(task_type) != 1L) {
    # We could not infer type from any PipeOp output channels, so we try to infer it from the base learners
    baselearners = map(graph_base_learner(graph, resolve_branching = FALSE), "learner_model")
    task_type = unique(unlist(map(baselearners, function(x) {
      # Currently we should not have Multiplicities here, since Graph gets NULLed explicitly upon construction.
      # If we ever allow initializing a Learner with a trained Graph, the following will be necessary.
      x = multiplicity_flatten(x)
      if (length(x) >= 1) return(x[[1]]$task_type)
      return(NULL)  # only if multiplicity of length 0
    }), recursive = FALSE, use.names = FALSE))
  }
  if (length(task_type) > 1) {
    stopf("GraphLearner can not infer task_type from given Graph\nbase_learner() and in/out types leave multiple possibilities: %s", str_collapse(task_type))
  }
  c(task_type, "classif")[[1]]  # "classif" as final fallback
}

# for a PipeOpUnbranch, search for its predecessor PipeOp that is currently "active",
# i.e. that gets non-NOP-input in the current hyperparameter configuration of PipeOpBranch ops.
# Returns a list, named by PipeOpUnbranch IDs, containing the incoming PipeOp IDs.
# PipeOpBranch ops that are connected to overall Graph input get an empty string as predecessor ID.
# Returns a named `list`, named by PipeOpUnbranch IDs, containing the incoming PipeOp ID.
# Typically, there is only one incoming PipeOp ID; only if a `selection` is not set, or a `TuneToken`,
# do we concede that we do not know the exact input; in this case the list entry contains a vector
# of all possible incoming PipeOp IDs.
get_po_unbranch_active_input = function(graph) {
  # query a given PipeOpBranch what its selected output is
  # Currently, PipeOpBranch 'selection' can be either integer-valued or a string.
  # If it is something else (unset, or a TuneToken), we cannot infer the active output.
  get_po_branch_active_output = function(pipeop) {
    assertR6(pipeop, "PipeOpBranch")
    pob_ps = pipeop$param_set
    selection = pob_ps$values$selection
    # will have to check if selection is numeric / character, in case it
    # is not given or a TuneToken or something like that.
    if (pob_ps$class[["selection"]] == "ParamInt") {
      if (!test_int(selection)) {
        stopf("Cannot infer active output of PipeOpBranch %s with non-numeric 'selection'.", pipeop$id)
        # return(pipeop$output$name)
      }
      return(pipeop$output$name[[pob_ps$values$selection]])
    } else {
      if (!test_string(selection)) {
        stopf("Cannot infer active output of PipeOpBranch %s with non-string 'selection'.", pipeop$id)
        # return(pipeop$output$name)
      }
      return(pob_ps$values$selection)
    }
  }
  # pacify static checks
  src_id = NULL
  dst_id = NULL
  src_channel = NULL
  dst_channel = NULL
  state = NULL
  reason = NULL

  # This algorithnm is similar to reduce_graph(): It uses a data.table of edges
  # with an additional column that tracks the state (active or not) of each edge.
  # We also track a description ("reason") of why an edge is active or not: the
  # last PipeOpBranch encountered, and its active state. This should make for
  # informative error messages.

  branch_state_info = copy(graph$edges)
  branch_state_info[, `:=`(state = NA, reason = list())]
  graph_input = graph$input
  branch_state_info = rbind(
    branch_state_info,
    graph_input[, list(src_id = "", src_channel = graph_input$name, dst_id = graph_input$op.id,
      dst_channel = graph_input$channel.name, state = TRUE, reason = list("direct Graph input, which is always active"))]
  )
  ids = graph$ids(sorted = TRUE)  # Topologically sorted IDs
  po_unbranch_active_input = character(0)
  for (pipeop_id in ids) {
    pipeop = graph$pipeops[[pipeop_id]]
    inedges = branch_state_info[dst_id == pipeop_id, ]
    # PipeOpUnbranch with more than one active input is an error
    if (inherits(pipeop, "PipeOpUnbranch") && sum(inedges$state) > 1) {
      stopf("PipeOpUnbranch %s has multiple active inputs: %s.",
        pipeop_id,
        inedges[state == TRUE, andpaste(sprintf("input '%s' from %s", dst_channel, unlist(reason, recursive = FALSE, use.names = TRUE)))]
      )
    }

    # any PipeOp with conflicting inputs: This is only OK if it is a PipeOpUnbranch
    if (all(inedges$state) != any(inedges$state)) {
      if (!inherits(pipeop, "PipeOpUnbranch")) {
        stopf("Inconsistent selection of PipeOpBranch outputs:\n%s in conflict with %s at PipeOp %s.",
          inedges[state == FALSE, andpaste(unique(unlist(reason, recursive = FALSE, use.names = TRUE)))],
          inedges[state == TRUE, andpaste(unique(unlist(reason, recursive = FALSE, use.names = TRUE)))],
          pipeop_id
        )
      }
      # PipeOpUnbranch selects down to the single selected input.
      # we have already checked that this is unique.
      state_current = TRUE
      reason_current = inedges$reason[inedges$state]
      po_unbranch_active_input[[pipeop_id]] = inedges$src_id[inedges$state]
    } else {
      # all inputs are in agreement
      state_current = any(inedges$state)
      reason_current = unique(unlist(inedges$reason, recursive = FALSE, use.names = FALSE))
    }

    if (state_current && inherits(pipeop, "PipeOpBranch")) {
      # PipeOpBranch is only special when it is actually active
      active_output = get_po_branch_active_output(pipeop)
      branch_state_info[src_id == pipeop_id, `:=`(state = src_channel == active_output,
        reason = as.list(sprintf("PipeOpBranch '%s' %s output '%s'",
          pipeop_id, ifelse(src_channel == active_output, "active", "inactive"),
          src_channel))
      )]
    } else {
      branch_state_info[src_id == pipeop_id, `:=`(state = state_current, reason = list(reason_current))]
    }
  }
  po_unbranch_active_input
}

andpaste = function(x, sep = ", ", lastsep = ", and ") {
  if (length(x) == 0) return("")
  if (length(x) == 1) return(x[[1]])
  paste0(paste(first(x, -1), collapse = sep), lastsep, last(x))
}

graph_base_learner = function(graph, resolve_branching = TRUE, lookup_field = "learner_model") {
  # GraphLearner$base_learner(), where return_all is TRUE, return_po is TRUE, and recursive is 1.
  # We are looking for all PipeOps with the non-NULL field named `lookup_field`, typically "learner_model", possibly resolving branching.

  gm_output = graph$output
  if (nrow(gm_output) != 1) {
    # should never happen, since we checked this in initialize(), but theoretically the user could have changed the graph by-reference
    stop("Graph has no unique output.")
  }
  last_pipeop_id = gm_output$op.id

  # pacify static checks
  src_id = NULL
  dst_id = NULL
  src_channel = NULL
  dst_channel = NULL
  delayedAssign("po_unbranch_active_input", get_po_unbranch_active_input(graph))  # only call get_pobranch_active_output() if we encounter a PipeOpUnbranch

  pipeops_visited = new.env(parent = emptyenv())
  search_base_learner_pipeops = function(current_pipeop) {
    repeat {
      last_pipeop = graph$pipeops[[current_pipeop]]
      if (get0(current_pipeop, pipeops_visited, ifnotfound = FALSE)) return(list())
      assign(current_pipeop, TRUE, pipeops_visited)
      field_content = get0(lookup_field, last_pipeop, ifnotfound = NULL)
      if (!is.null(field_content)) return(list(last_pipeop))
      next_pipeop = graph$edges[dst_id == current_pipeop, src_id]
      if (length(next_pipeop) > 1) {
        # more than one predecessor
        if (inherits(last_pipeop, "PipeOpUnbranch") && resolve_branching) {
          tryCatch({
            next_pipeop = po_unbranch_active_input[[current_pipeop]]
          }, error = function(e) {
            if (e$message %like% "Cannot infer active output of PipeOpBranch") {
              resolve_branching <<- FALSE  # give up; this happens when tuning.
            } else {
              stop(e)
            }
          })
        }
        if (!inherits(last_pipeop, "PipeOpUnbranch") || !resolve_branching) {
          return(unique(unlist(lapply(next_pipeop, search_base_learner_pipeops), recursive = FALSE, use.names = FALSE)))
        }
        # PipeOpUnbranch inference succeeded; continue with selected branch.
        if (next_pipeop == "") next_pipeop = character(0)
      }
      if (length(next_pipeop) == 0) return(list())
      current_pipeop = next_pipeop
    }
  }

  unique(search_base_learner_pipeops(last_pipeop_id))
}
