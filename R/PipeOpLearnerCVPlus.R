#' @title Wrap a Learner into a PipeOp with Cross-validation Plus Confidence Intervals as Predictions
#'
#' @usage NULL
#' @name mlr_pipeops_learner_cvplus
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`].
#'
#' @description
#' Wraps an [`mlr3::Learner`] into a [`PipeOp`].
#'
#' Inherits the `$param_set` (and therefore `$param_set$values`) from the [`Learner`][mlr3::Learner] it is constructed from.
#'
#' Using [`PipeOpLearnerCVPlus`], it is possible to embed a [`mlr3::Learner`] into a [`Graph`].
#' [`PipeOpLearnerCVPlus`] can then be used to perform cross validation plus (or jackknife plus).
#' During training, [`PipeOpLearnerCVPlus`] performs cross validation on the training data.
#' During prediction, the models from the training stage are used to constructs predictive confidence intervals for the prediction data based on out-of-fold residuals and out-of-fold predictions.
#'
#' @section Construction:
#' ```
#' PipeOpLearnerCVPlus$new(learner, id = NULL, param_vals = list())
#' ```
#'
#' * `learner` :: [`LearnerRegr`][mlr3::LearnerRegr]
#'   [`LearnerRegr`][mlr3::LearnerRegr] to use for the cross validation models in the Cross Validation Plus method.
#'  This argument is always cloned; to access the [`Learner`][mlr3::Learner] inside `PipeOpLearnerCVPlus` by-reference, use `$learner`.\cr
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, internally defaulting to the `id` of the [`Learner`][mlr3::Learner] being wrapped.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction.
#'   Default is `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpLearnerCVPlus`] has one input channel named `"input"`, taking a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' [`PipeOpLearnerCVPlus`] has one output channel named `"output"`, producing `NULL` during training and a [`PredictionRegr`][mlr3::PredictionRegr]
#' during prediction.
#'
#' The output during prediction is a [`PredictionRegr`][mlr3::PredictionRegr] with `predict_type` `quantiles` on the prediction input data.
#' The `alpha` and `1 - alpha` quantiles are the `quantiles` of the prediction interval produced by the cross validation plus method.
#' The `response` is the median of the prediction of all cross validation models on the prediction data.
#'
#' @section State:
#' The `$state` is a named `list` with members:
#' * `cv_models` :: `list`\cr
#'   List of cross validation models created by the [`Learner`][`mlr3::Learner`]'s `$.train()` function during resampling with method `"cv"`.
#' * `residuals` :: `data.table`\cr
#'   `data.table` with columns `fold` and `residual`. Lists the Regression residuals for each observation and cross validation fold.
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
#'   [`Learner`][mlr3::Learner] that is being wrapped. Read-only.
#' * `learner_model` :: [`Learner`][mlr3::Learner]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. This learner contains the model if the `PipeOp` is trained. Read-only.
#' * `predict_type`\cr
#'   Predict type of the [`PipeOpLearnerCVPlus`], which is always `"response"  "quantiles"`.
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
#' learner = lrn("regr.ranger")
#' lrncvplus_po = mlr_pipeops$get("learner_cv_plus", learner)
#'
#' lrncvplus_po$train(list(task))
#' lrncvplus_po$predict(list(task))
#' \dontshow{ \} }
PipeOpLearnerCVPlus = R6Class("PipeOpLearnerCVPlus",
  inherit = PipeOp,
  public = list(
    initialize = function(learner, id = NULL, param_vals = list()) {
      private$.learner = as_learner(learner, clone = TRUE)
      id = id %??% private$.learner$id
      type = private$.learner$task_type

      if (!("regr" %in% type)) {
        stop("PipeOpLearnerCVPlus only supports regression.")
      }

      task_type = mlr_reflections$task_types[type, mult = "first"]$task
      out_type = mlr_reflections$task_types[type, mult = "first"]$task

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
        multiplicity_recurse(self$state, clone_with_state, learner = private$.learner)
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
    .state_class = "pipeop_learner_cv_plus_state",

    .train = function(task) {
      pv = private$.cvplus_param_set$values

      # Compute CV Predictions
      rdesc = rsmp("cv", folds = pv$folds)
      rr = resample(task, private$.learner, rdesc, store_models = TRUE)

      prds = rbindlist(map(rr$predictions(predict_sets = "test"), as.data.table), idcol = "fold")

      # Add trained models and residuals to PipeOp state
      self$state = list(cv_models = rr$learners,
                        residuals = prds[, .(fold, residual = abs(truth - response))])

      list(NULL)
    },
    .predict = function(task) {
      pv = private$.cvplus_param_set$values

      mu_hat = map(self$state$cv_models, function(learner) {
          as.data.table(learner$predict(task))
        })

      get_quantiles = function(observation) {
        quantiles = pmap_dtr(self$state$residuals, function(fold, residual) {
          list(lower = mu_hat[[fold]][observation, response] - residual,
               upper = mu_hat[[fold]][observation, response] + residual)
        })
        list(q_lower = quantile(quantiles$lower, probs = pv$alpha),
             q_upper = quantile(quantiles$upper, probs = 1 - pv$alpha))
      }

      quantiles = matrix(map_dtr(seq_len(task$nrow), get_quantiles), nrow = task$nrow, byrow = TRUE)
      quantiles = unname(quantiles)

      response = matrix(map_dtr(seq_len(task$nrow), function(observation) {
        quantile(pmap_dbl(mu_hat, function(fold) fold[observation, response], numeric(1)), probs = 0.5)
      }), nrow = task$nrow, byrow = TRUE)

      attr(quantiles, "probs") = c(pv$alpha, 1 - pv$alpha)

      list(list(response = response, quantiles = quantiles))
    },

    .cvplus_param_set = NULL,
    .learner = NULL,
    .additional_phash_input = function() private$.learner$phash
  )
)


mlr_pipeops$add("learner_cv_plus", PipeOpLearnerCVPlus, list(R6Class("Learner", public = list(id = "learner_cv_plus", task_type = "regr", param_set = ps()))$new()))
