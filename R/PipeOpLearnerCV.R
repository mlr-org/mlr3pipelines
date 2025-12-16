#' @title Wrap a Learner into a PipeOp with Cross-validated Predictions as Features
#'
#' @usage NULL
#' @name mlr_pipeops_learner_cv
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Wraps an [`mlr3::Learner`] into a [`PipeOp`].
#'
#' Returns cross-validated predictions during training as a [`Task`][mlr3::Task] and stores a model of the
#' [`Learner`][mlr3::Learner] trained on the whole data in `$state`. This is used to create a similar
#' [`Task`][mlr3::Task] during prediction.
#' Optionally, the fitted models obtained during the resampling phase can be reused for prediction by averaging
#' their predictions, avoiding the need for an additional fit on the complete training data.
#'
#' The [`Task`][mlr3::Task] gets features depending on the capsuled [`Learner`][mlr3::Learner]'s
#' `$predict_type`. If the [`Learner`][mlr3::Learner]'s `$predict.type` is `"response"`, a feature `<ID>.response` is created,
#' for `$predict.type` `"prob"` the `<ID>.prob.<CLASS>` features are created, and for `$predict.type` `"se"` the new columns
#' are `<ID>.response` and `<ID>.se`. `<ID>` denotes the `$id` of the `PipeOpLearnerCV` object.
#'
#' Inherits the `$param_set` (and therefore `$param_set$values`) from the [`Learner`][mlr3::Learner] it is constructed from.
#'
#' `PipeOpLearnerCV` can be used to create "stacking" or "super learning" [`Graph`]s that use the output of one [`Learner`][mlr3::Learner]
#' as feature for another [`Learner`][mlr3::Learner]. Because the `PipeOpLearnerCV` erases the original input features, it is often
#' useful to use [`PipeOpFeatureUnion`] to bind the prediction [`Task`][mlr3::Task] to the original input [`Task`][mlr3::Task].
#'
#' @section Construction:
#' ```
#' PipeOpLearnerCV$new(learner, id = NULL, param_vals = list())
#' ```
#'
#' * `learner` :: [`Learner`][mlr3::Learner] \cr
#'   [`Learner`][mlr3::Learner] to use for cross validation / prediction, or a string identifying a
#'   [`Learner`][mlr3::Learner] in the [`mlr3::mlr_learners`] [`Dictionary`][mlr3misc::Dictionary].
#'  This argument is always cloned; to access the [`Learner`][mlr3::Learner] inside `PipeOpLearnerCV` by-reference, use `$learner`.\cr
#' * `id` :: `character(1)`
#'   Identifier of the resulting object, internally defaulting to the `id` of the [`Learner`][mlr3::Learner] being wrapped.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' `PipeOpLearnerCV` has one input channel named `"input"`, taking a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' `PipeOpLearnerCV` has one output channel named `"output"`, producing a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' The output is a task with the same target as the input task, with features replaced by predictions made by the [`Learner`][mlr3::Learner].
#' During training, this prediction is the out-of-sample prediction made by [`resample`][mlr3::resample], during prediction, this is the
#' ordinary prediction made on the data by a [`Learner`][mlr3::Learner] trained on the training phase data.
#'
#' @section State:
#' The `$state` is set to the `$state` slot of the [`Learner`][mlr3::Learner] object, together with the `$state` elements inherited from the
#' [`PipeOpTaskPreproc`]. It is a named `list` with the inherited members, as well as:
#' * `model` :: `any`\cr
#'   Model created by the [`Learner`][mlr3::Learner]'s `$.train()` function.
#' * `train_log` :: [`data.table`][data.table::data.table] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during training.
#' * `train_time` :: `numeric(1)`\cr
#'   Training time, in seconds.
#' * `predict_log` :: `NULL` | [`data.table`][data.table::data.table] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during prediction.
#' * `predict_time` :: `NULL` | `numeric(1)`
#'   Prediction time, in seconds.
#' * `predict_method` :: `character(1)`\cr
#'   `"full"` when prediction uses a learner fitted on all training data, `"cv_ensemble"` when predictions are averaged over
#'   models trained on resampling folds.
#' * `cv_model_states` :: `NULL` | `list`\cr
#'   Present for `predict_method = "cv_ensemble"`. Contains the states of the learners trained on each resampling fold.
#'
#' This state is given the class `"pipeop_learner_cv_state"`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOpTaskPreproc`], as well as the parameters of the [`Learner`][mlr3::Learner] wrapped by this object.
#' Besides that, parameters introduced are:
#' * `resampling.method` :: `character(1)`\cr
#'   Which resampling method do we want to use. Currently only supports `"cv"` and `"insample"`. `"insample"` generates
#'   predictions with the model trained on all training data.
#' * `resampling.folds` :: `numeric(1)`\cr
#'   Number of cross validation folds. Initialized to 3. Only used for `resampling.method = "cv"`.
#' * `resampling.keep_response` :: `logical(1)`\cr
#'   Only effective during `"prob"` prediction: Whether to keep response values, if available. Initialized to `FALSE`.
#' * `resampling.predict_method` :: `character(1)`\cr
#'   Controls how predictions are produced after training. `"full"` (default) fits the wrapped learner on the entire training data.
#'   `"cv_ensemble"` reuses the models fitted during resampling and averages their predictions. This option currently supports
#'   classification and regression learners together with `resampling.method = "cv"`.
#' * `resampling.prob_aggr` :: `character(1)`\cr
#'   Probability aggregation used when `"cv_ensemble"` predictions are produced for classification learners that can emit class probabilities.
#'   Shares the semantics with [`PipeOpClassifAvg`]: `"mean"` (linear opinion pool, default) and `"log"` (log opinion pool / product of experts).
#'   Only present for learners that support `"prob"` predictions.
#' * `resampling.prob_aggr_eps` :: `numeric(1)`\cr
#'   Stabilization constant applied when `resampling.prob_aggr = "log"` to clamp probabilities before taking logarithms.
#'   Defaults to `1e-12`. Only present for learners that support `"prob"` predictions.
#' * `resampling.se_aggr` :: `character(1)`\cr
#'   Standard error aggregation used when `"cv_ensemble"` predictions are produced for regression learners with `predict_type`
#'   containing `"se"`. Shares the definitions with [`PipeOpRegrAvg`], i.e. `"predictive"`, `"mean"`, `"within"`, `"between"`, `"none"`.
#'   Initialized to `"predictive"` (within-fold variance plus between-fold disagreement) when constructed with a [`Learner`][mlr3::Learner] that has `predict_type = "se"`;
#'   otherwise to `"none"`.\cr
#'   Only present for learners that support `"se"` predictions.
#' * `resampling.se_aggr_rho` :: `numeric(1)`\cr
#'   Equicorrelation parameter for `resampling.se_aggr = "mean"`, interpreted as in [`PipeOpRegrAvg`]. Ignored otherwise.
#'   Defaults to `0` when `resampling.se_aggr = "mean"`.\cr
#'   Only present for learners that support `"se"` predictions.
#'
#' @section Internals:
#' The `$state` is currently not updated by prediction, so the `$state$predict_log` and `$state$predict_time` will always be `NULL`.
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`], as well as:
#' * `learner` :: [`Learner`][mlr3::Learner]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. Read-only.
#' * `learner_model` :: [`Learner`][mlr3::Learner]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. This learner contains the model if the `PipeOp` is trained. Read-only.
#'
#' @section Methods:
#' Methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family Pipeops
#' @family Meta PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examplesIf requireNamespace("rpart")
#' library("mlr3")
#'
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#'
#' lrncv_po = po("learner_cv", learner)
#' lrncv_po$learner$predict_type = "response"
#'
#' nop = mlr_pipeops$get("nop")
#'
#' graph = gunion(list(
#'   lrncv_po,
#'   nop
#' )) %>>% po("featureunion")
#'
#' graph$train(task)
#'
#' graph$pipeops$classif.rpart$learner$predict_type = "prob"
#' graph$pipeops$classif.rpart$param_set$values$resampling.predict_method = "cv_ensemble"
#'
#' graph$train(task)
PipeOpLearnerCV = R6Class("PipeOpLearnerCV",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(learner, id = NULL, param_vals = list()) {
      private$.learner = as_learner(learner, clone = TRUE)
      id = id %??% private$.learner$id
      # FIXME: can be changed when mlr-org/mlr3#470 has an answer
      type = private$.learner$task_type
      task_type = mlr_reflections$task_types[type, mult = "first"]$task

      params = list(
        method = p_fct(levels = c("cv", "insample"), init = "cv", tags = c("train", "required")),
        folds = p_int(lower = 2L, upper = Inf, init = 3, tags = c("train", "required")),
        keep_response = p_lgl(init = FALSE, tags = c("train", "required")),
        predict_method = p_fct(levels = c("full", "cv_ensemble"), init = "full", tags = c("train", "required"))
      )

      if ("prob" %in% private$.learner$predict_types) {
        params$prob_aggr = p_fct(
          levels = c("mean", "log"),
          init = "mean",
          tags = c("train", "predict", "prob_aggr", "required")
        )
        params$prob_aggr_eps = p_dbl(
          lower = 0,
          upper = 1,
          default = 1e-12,
          tags = c("train", "predict", "prob_aggr"),
          depends = quote(prob_aggr == "log")
        )
      }

      if ("se" %in% private$.learner$predict_types) {
        params$se_aggr = p_fct(levels = c("predictive", "mean", "within", "between", "none"), tags = c("train", "predict", "se_aggr", "required"),
          init = if (private$.learner$predict_type == "se") "predictive" else "none")
        params$se_aggr_rho = p_dbl(lower = -1, upper = 1, tags = c("train", "predict", "se_aggr"), depends = quote(se_aggr == "mean"), default = 0)
      }

      private$.crossval_param_set = ParamSet$new(params)
      # Dependencies in paradox have been broken from the start and this is known since at least a year:
      # https://github.com/mlr-org/paradox/issues/216
      # The following would make it _impossible_ to set "method" to "insample", because then "folds"
      # is both _required_ (required tag above) and at the same time must be unset (because of this
      # dependency). We will opt for the least annoying behaviour here and just not use dependencies
      # in PipeOp ParamSets.
      # private$.crossval_param_set$add_dep("folds", "method", CondEqual$new("cv"))  # don't do this.

      super$initialize(id, alist(resampling = private$.crossval_param_set, private$.learner$param_set), param_vals = param_vals, can_subset_cols = TRUE, task_type = task_type, tags = c("learner", "ensemble"))
    }

  ),
  active = list(
    learner = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$learner is read-only.")
        }
      }
      private$.learner
    },
    learner_model = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$learner_model is read-only.")
        }
      }
      if (is.null(self$state) || is_noop(self$state)) {
        private$.learner
      } else {
        multiplicity_recurse(self$state, private$state_to_model)
      }
    },
    predict_type = function(val) {
      if (!missing(val)) {
        assert_subset(val, names(mlr_reflections$learner_predict_types[[private$.learner$task_type]]))
        private$.learner$predict_type = val
      }
      private$.learner$predict_type
    }
  ),
  private = list(
    .state_class = "pipeop_learner_cv_state",
    .crossval_param_set = NULL,
    .learner = NULL,

    .train_task = function(task) {
      on.exit({private$.learner$state = NULL})

      pv = private$.crossval_param_set$values
      predict_method = pv$predict_method %??% "full"

      if (predict_method == "cv_ensemble") {
        if (pv$method != "cv") {
          stopf("`resampling.predict_method = \"cv_ensemble\"` requires `resampling.method = \"cv\"`, got '%s'.", pv$method)
        }
        private$assert_cv_predict_supported()
      }

      cv_model_states = NULL
      if (predict_method == "full") {
        # Train a learner for predicting
        self$state = private$.learner$train(task)$state
        self$state$predict_method = "full"
      }

      # Compute CV Predictions
      if (pv$method != "insample") {
        rdesc = mlr_resamplings$get(pv$method)
        if (pv$method == "cv") rdesc$param_set$values = list(folds = pv$folds)
        rr = resample(task, private$.learner, rdesc, store_models = predict_method == "cv_ensemble")
        prds = as.data.table(rr$prediction(predict_sets = "test"))
        if (predict_method == "cv_ensemble") {
          cv_model_states = map(rr$learners, "state")
          self$state = private$make_cv_state(cv_model_states)
        }
      } else {
        if (predict_method == "cv_ensemble") {
          stop("`resampling.predict_method = \"cv_ensemble\"` can not be combined with `resampling.method = \"insample\"`.")
        }
        prds = as.data.table(private$.learner$predict(task))
      }

      private$pred_to_task(prds, task)
    },

    .predict_task = function(task) {
      on.exit({private$.learner$state = NULL})
      state = self$state
      predict_method = private$get_predict_method(state)
      prediction_dt = if (predict_method == "cv_ensemble") {
        if (is.null(state$cv_model_states) || !length(state$cv_model_states)) {
          stop("`resampling.predict_method = \"cv_ensemble\"` was selected, but no stored model states are available.")
        }
        private$predict_from_cv_models(task, state$cv_model_states)
      } else {
        private$.learner$state = state
        as.data.table(private$.learner$predict(task))
      }
      private$pred_to_task(prediction_dt, task)
    },

    pred_to_task = function(prds, task) {
      if (!is.null(prds$truth)) prds[, truth := NULL]
      if (!self$param_set$values$resampling.keep_response && self$learner$predict_type %in% c("impact", "prob")) {
        prds[, response := NULL]
      }
      se_aggr = private$.crossval_param_set$get_values()$se_aggr %??% "none"
      if ((self$learner$predict_type != "se" || se_aggr == "none") && "se" %in% colnames(prds)) {
        set(prds, j = "se", value = NULL)
      }
      renaming = setdiff(colnames(prds), c("row_id", "row_ids"))
      setnames(prds, renaming, sprintf("%s.%s", self$id, renaming))

      # This can be simplified for mlr3 >= 0.11.0;
      # will be always "row_ids"
      row_id_col = intersect(colnames(prds), c("row_id", "row_ids"))
      setnames(prds, old = row_id_col, new = task$backend$primary_key)
      task$select(character(0))$cbind(prds)
    },

    predict_from_cv_models = function(task, cv_model_states) {
      predictions = map(cv_model_states, function(state) {
        private$.learner$state = state
        pred = private$.learner$predict(task)
        private$.learner$state = NULL
        pred
      })
      private$aggregate_predictions(predictions)
    },

    aggregate_predictions = function(predictions) {
      if (!length(predictions)) stop("No predictions available to aggregate.")
      task_type = private$.learner$task_type
      if (task_type == "classif") {
        return(private$aggregate_classif_predictions(predictions))
      }
      if (task_type == "regr") {
        return(private$aggregate_regr_predictions(predictions))
      }
      stopf("`resampling.predict_method = \"cv_ensemble\"` is not implemented for task type '%s'.", task_type)
    },

    # Note: The following aggregation methods use similar logic to PipeOpClassifAvg and PipeOpRegrAvg
    # (particularly the weighted_matrix_sum and weighted_factor_mean helper functions from PipeOpEnsemble).
    # However, they return data.table instead of Prediction objects to integrate with pred_to_task().
    # This design avoids the overhead of creating intermediate Prediction objects that would need to be 
    # immediately converted to data.table.
    aggregate_classif_predictions = function(predictions) {
      row_ids = predictions[[1]]$row_ids
      k = length(predictions)
      weights = rep(1 / k, k)

      prob_list = map(predictions, "prob")
      prob_cfg = private$.crossval_param_set$get_values(tags = "prob_aggr")
      if (length(prob_list) && !any(map_lgl(prob_list, is.null))) {
        prob = switch(prob_cfg$prob_aggr,
          mean = weighted_matrix_sum(prob_list, weights),
          log = weighted_matrix_logpool(prob_list, weights, epsilon = prob_cfg$prob_aggr_eps %??% 1e-12)
        )

        prob = pmin(pmax(prob, 0), 1)
        lvls = colnames(prob)
        response = factor(lvls[max.col(prob, ties.method = "random")], levels = lvls)
        dt = cbind(
          data.table(row_ids = row_ids, response = response), 
          setnames(data.table(prob), paste0("prob.", lvls))
        )
        return(dt)
      }

      responses = map(predictions, "response")
      lvls = levels(responses[[1]])
      freq = weighted_factor_mean(responses, weights, lvls)
      response = factor(lvls[max.col(freq, ties.method = "random")], levels = lvls)

      data.table(row_ids = row_ids, response = response)
    },

    aggregate_regr_predictions = function(predictions) {
      responses = map(predictions, "response")
      k = length(responses)
      response = Reduce(`+`, responses) / k

      ses_list = map(predictions, "se")
      if (!all(map_lgl(ses_list, is.null))) {
        if (any(map_lgl(ses_list, is.null))) {
          stop("Learners returned standard errors for only a subset of CV models.")
        }
      } else {
        # Let aggregate_se_weighted handle this
        ses_list = NULL
      }

      se_cfg = private$.crossval_param_set$get_values()
      weights = rep(1 / k, k)

      method = se_cfg$se_aggr %??% "none"
      rho = se_cfg$se_aggr_rho %??% 0
      se = aggregate_se_weighted(responses, ses_list, weights, method = method, rho = rho)

      data.table(row_ids = predictions[[1L]]$row_ids, response = response, se = se)
    },

    make_cv_state = function(cv_model_states) {
      list(
        model = NULL,
        train_log = NULL,
        train_time = NA_real_,
        predict_log = NULL,
        predict_time = NULL,
        predict_method = "cv_ensemble",
        cv_model_states = cv_model_states
      )
    },

    get_predict_method = function(state) {
      if (is.null(state) || is_noop(state) || !is.list(state)) {
        return("full")
      }
      state$predict_method %??% "full"
    },

    assert_cv_predict_supported = function() {
      if (private$.learner$task_type %nin% c("classif", "regr")) {
        stopf("`resampling.predict_method = \"cv_ensemble\"` is only supported for classification and regression learners (got '%s').", private$.learner$task_type)
      }
    },

    state_to_model = function(state) {
      predict_method = private$get_predict_method(state)
      if (predict_method == "cv_ensemble") {
        return(private$build_cv_graph_learner(state$cv_model_states))
      }
      clone_with_state(private$.learner, state)
    },
    
    build_cv_graph_learner = function(cv_model_states) {
      assert_list(cv_model_states, types = "list", min.len = 1)
      pipeops = map(seq_along(cv_model_states), function(i) {
        po_id = sprintf("%s.cv_model_%02d", self$id, i)
        polrn = PipeOpLearner$new(private$.learner, id = po_id)
        polrn$state = cv_model_states[[i]]
        polrn
      })
      agg_id = sprintf("%s.cv_avg", self$id)
      aggregator = switch(private$.learner$task_type,
        classif = PipeOpClassifAvg$new(innum = length(pipeops), id = agg_id),
        regr = PipeOpRegrAvg$new(innum = length(pipeops), id = agg_id),
        stopf("Task type '%s' not supported for cv ensemble.", private$.learner$task_type)
      )
      extra_cfg = list()
      if (inherits(aggregator, "PipeOpClassifAvg")) {
        extra_cfg = private$.crossval_param_set$get_values(tags = "prob_aggr")
      }
      if (inherits(aggregator, "PipeOpRegrAvg")) {
        extra_cfg = private$.crossval_param_set$get_values(tags = "se_aggr")
      }
      aggregator$param_set$set_values(.values = extra_cfg)

      aggregator$state = list()
      graph = gunion(pipeops) %>>!% aggregator
      graph_state = graph$state
      class(graph_state) = c("graph_learner_model", class(graph_state))
      glrn = GraphLearner$new(graph)
      glrn$model = graph_state
      glrn
    },

    .additional_phash_input = function() private$.learner$phash
  )
)

#' @export
marshal_model.pipeop_learner_cv_state = function(model, inplace = FALSE, ...) {
  # Note that a Learner state contains other reference objects, but we don't clone them here, even when inplace
  # is FALSE. For our use-case this is just not necessary and would cause unnecessary overhead in the mlr3
  # workhorse function
  was_marshaled = FALSE
  if (!is.null(model$model)) {
    model$model = marshal_model(model$model, inplace = inplace)
    was_marshaled = was_marshaled || is_marshaled_model(model$model)
  }
  if (!is.null(model$cv_model_states)) {
    model$cv_model_states = map(model$cv_model_states, marshal_model, inplace = inplace)
    was_marshaled = was_marshaled || some(model$cv_model_states, is_marshaled_model)
  }
  # only wrap this in a marshaled class if something was actually marshaled above
  if (was_marshaled) {
    model = structure(
      list(marshaled = model, packages = "mlr3pipelines"),
      class = c(paste0(class(model), "_marshaled"), "marshaled")
    )
  }
  model
}

#' @export
unmarshal_model.pipeop_learner_cv_state_marshaled = function(model, inplace = FALSE, ...) {
  state_marshaled = model$marshaled
  if (!is.null(state_marshaled$model)) {
    state_marshaled$model = unmarshal_model(state_marshaled$model, inplace = inplace)
  }
  if (!is.null(state_marshaled$cv_model_states)) {
    state_marshaled$cv_model_states = map(state_marshaled$cv_model_states, unmarshal_model, inplace = inplace)
  }
  state_marshaled
}


mlr_pipeops$add("learner_cv", PipeOpLearnerCV, list(R6Class("Learner", public = list(id = "learner_cv", task_type = "classif", param_set = ps(), predict_types = "response"))$new()))
