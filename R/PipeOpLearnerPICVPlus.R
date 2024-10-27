#' @title Wrap a Learner into a PipeOp with Cross-validation Plus Confidence Intervals as Predictions
#'
#' @usage NULL
#' @name mlr_pipeops_learner_pi_cvplus
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`].
#'
#' @description
#' Wraps an [`mlr3::Learner`] into a [`PipeOp`].
#'
#' Inherits the `$param_set` (and therefore `$param_set$values`) from the [`Learner`][mlr3::Learner] it is constructed from.
#'
#' Using [`PipeOpLearnerPICVPlus`], it is possible to embed a [`mlr3::Learner`] into a [`Graph`].
#' [`PipeOpLearnerPICVPlus`] can then be used to perform cross validation plus (or jackknife plus).
#' During training, [`PipeOpLearnerPICVPlus`] performs cross validation on the training data.
#' During prediction, the models from the training stage are used to construct predictive confidence intervals for the prediction data based on
#' out-of-fold residuals and out-of-fold predictions.
#'
#' @section Construction:
#' ```
#' PipeOpLearnerPICVPlus$new(learner, id = NULL, param_vals = list())
#' ```
#'
#' * `learner` :: [`LearnerRegr`][mlr3::LearnerRegr]
#'   [`LearnerRegr`][mlr3::LearnerRegr] to use for the cross validation models in the Cross Validation Plus method.
#'  This argument is always cloned; to access the [`Learner`][mlr3::Learner] inside `PipeOpLearnerPICVPlus` by-reference, use `$learner`.\cr
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, internally defaulting to the `id` of the [`Learner`][mlr3::Learner] being wrapped.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction.
#'   Default is `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpLearnerPICVPlus`] has one input channel named `"input"`, taking a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' [`PipeOpLearnerPICVPlus`] has one output channel named `"output"`, producing `NULL` during training and a [`PredictionRegr`][mlr3::PredictionRegr]
#' during prediction.
#'
#' The output during prediction is a [`PredictionRegr`][mlr3::PredictionRegr] with `predict_type` `quantiles` on the prediction input data.
#' The `alpha` and `1 - alpha` quantiles are the `quantiles` of the prediction interval produced by the cross validation plus method.
#' The `response` is the median of the prediction of all cross validation models on the prediction data.
#'
#' @section State:
#' The `$state` is a named `list` with members:
#' * `cv_model_states` :: `list`\cr
#'   List of the state of each cross validation model created by the [`Learner`][`mlr3::Learner`]'s `$.train()` function during resampling with method `"cv"`.
#' * `residuals` :: `data.table`\cr
#'   `data.table` with columns `fold` and `residual`. Lists the Regression residuals for each observation and cross validation fold.
#'
#' This state is given the class `"pipeop_learner_cv_state"`.
#'
#' @section Parameters:
#' The parameters of the [`Learner`][mlr3::Learner] wrapped by this object, as well as:
#' * `folds` :: `numeric(1)`\cr
#'   Number of cross validation folds. Initialized to 3.
#' * `alpha` :: `numeric(1)`\cr
#'   Quantile to use for the cross validation plus prediction intervals. Initialized to 0.05.
#'
#' @section Internals:
#' The `$state` is updated during training.
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`], as well as:
#' * `learner` :: [`Learner`][mlr3::Learner]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped.
#'   Read-only.
#' * `learner_model` :: [`Learner`][mlr3::Learner] or `list`\cr
#'   If the [`PipeOpLearnerPICVPlus`] has been trained, this is a `list` containing the [`Learner`][mlr3::Learner]s of the cross validation models.
#'   Otherwise, this contains the [`Learner`][mlr3::Learner] that is being wrapped.
#'   Read-only.
#' * `predict_type`\cr
#'   Predict type of the [`PipeOpLearnerPICVPlus`], which is always `"response"  "quantiles"`.
#'   This can be different to the predict type of the [`Learner`][mlr3::Learner] that is being wrapped.
#'
#' @section Methods:
#' Methods inherited from [`PipeOp`].
#'
#' @references
#' `r format_bib("barber_2021")`
#'
#' @family PipeOps
#' @family Meta PipeOps
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
#' @examples
#' \dontshow{ if (requireNamespace("rpart")) \{ }
#' library("mlr3")
#'
#' task = tsk("mtcars")
#' learner = lrn("regr.rpart")
#' lrncvplus_po = mlr_pipeops$get("learner_pi_cvplus", learner)
#'
#' lrncvplus_po$train(list(task))
#' lrncvplus_po$predict(list(task))
#' \dontshow{ \} }
PipeOpLearnerPICVPlus = R6Class("PipeOpLearnerPICVPlus",
  inherit = PipeOp,
  public = list(
    initialize = function(learner, id = NULL, param_vals = list()) {
      private$.learner = as_learner(learner, clone = TRUE)
      id = id %??% private$.learner$id
      type = private$.learner$task_type

      if ("regr" != type) {
        stop("PipeOpLearnerPICVPlus only supports regression.")
      }

      task_type = mlr_reflections$task_types[type, mult = "first"]$task
      out_type  = mlr_reflections$task_types[type, mult = "first"]$prediction

      # paradox requirements 1.0
      private$.cvplus_param_set = ps(
        folds = p_int(lower = 2L, upper = Inf, tags = c("train", "required")),
        alpha = p_dbl(lower = 0L, upper = 1L, tags = c("predict", "required"))
      )

      private$.cvplus_param_set$values = list(folds = 3, alpha = 0.05) # default

      super$initialize(id, param_set = alist(cvplus = private$.cvplus_param_set, private$.learner$param_set),
                        param_vals = param_vals,
                        input = data.table(name = "input", train = task_type, predict = task_type),
                        output = data.table(name = "output", train = "NULL", predict = out_type),
                        packages = learner$packages,
                        tags = c("learner", "ensemble")
      )
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
        multiplicity_recurse(self$state, function(state) {
          map(state$cv_model_states, clone_with_state, learner = private$.learner)
        })
      }
    },
    predict_type = function(val) {
      if (!missing(val)) {
        stop("$predict_type is read-only.")
      }
      mlr_reflections$learner_predict_types$regr$quantiles  # Returns c("response", "quantiles")
    }
  ),
  private = list(
    .state_class = "pipeop_learner_pi_cvplus_state",

    .train = function(inputs) {
      task = inputs[[1L]]
      pv = private$.cvplus_param_set$values

      # Compute CV Predictions
      rdesc = rsmp("cv", folds = pv$folds)
      rr = resample(task, private$.learner, rdesc, store_models = TRUE)

      prds = rbindlist(map(rr$predictions(predict_sets = "test"), as.data.table), idcol = "fold")

      # Add states of trained models and residuals to PipeOp state
      self$state = list(cv_model_states = map(rr$learners, "state"),
                        residuals = prds[, .(fold, residual = abs(truth - response))])

      list(NULL)
    },

    .predict = function(inputs) {
      task = inputs[[1L]]
      pv = private$.cvplus_param_set$values

      mu_hat = map(self$state$cv_model_states, function(state) {
        on.exit({private$.learner$state = NULL})
        private$.learner$state = state
        as.data.table(private$.learner$predict(task))
      })

      get_quantiles = function(observation) {
        quantiles = pmap_dtr(self$state$residuals, function(fold, residual) {
          list(lower = mu_hat[[fold]][observation, response] - residual,
               upper = mu_hat[[fold]][observation, response] + residual)
        })
        list(q_lower = stats::quantile(quantiles$lower, probs = pv$alpha),
             q_upper = stats::quantile(quantiles$upper, probs = 1 - pv$alpha))
      }

      quantiles = as.matrix(map_dtr(seq_len(task$nrow), get_quantiles))
      quantiles = unname(quantiles)
      attr(quantiles, "probs") = c(pv$alpha, 1 - pv$alpha)

      response = map_dbl(seq_len(task$nrow), function(observation) {
        stats::quantile(map_dbl(mu_hat, function(fold) {fold[observation, response]}), probs = 0.5)
      })

      list(PredictionRegr$new(
        row_ids = task$row_ids, truth = task$truth(),response = response, quantiles = quantiles
        ))
    },

    .cvplus_param_set = NULL,
    .learner = NULL,
    .additional_phash_input = function() private$.learner$phash
  )
)

#' @export
marshal_model.pipeop_learner_pi_cvplus_state = function(model, inplace = FALSE, ...) {
  # Note that a Learner state contains other reference objects, but we don't clone them here, even when inplace
  # is FALSE. For our use-case this is just not necessary and would cause unnecessary overhead in the mlr3
  # workhorse function
  model$cv_model_states = map(model$cv_model_states, marshal_model, inplace = inplace)
  # only wrap this in a marshaled class if the model was actually marshaled above
  # (the default marshal method does nothing)
  if (some(model$cv_model_states, is_marshaled_model)) {
    model = structure(
      list(marshaled = model, packages = "mlr3pipelines"),
      class = c(paste0(class(model), "_marshaled"), "marshaled")
    )
  }
  model
}

#' @export
unmarshal_model.pipeop_learner_pi_cvplus_state_marshaled = function(model, inplace = FALSE, ...) {
  state_marshaled = model$marshaled
  state_marshaled$cv_model_states = map(state_marshaled$cv_model_states, unmarshal_model, inplace = inplace)
  state_marshaled
}

mlr_pipeops$add("learner_pi_cvplus", PipeOpLearnerPICVPlus, list(R6Class("Learner", public = list(id = "learner_pi_cvplus", task_type = "regr", param_set = ps(), packages = "mlr3pipelines"))$new()))


