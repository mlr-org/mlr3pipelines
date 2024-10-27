#' @title Wrap a Learner into a PipeOp to to predict multiple Quantiles
#'
#' @usage NULL
#' @name mlr_pipeops_learner_quantiles
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`].
#'
#' @description
#' Wraps a [`LearnerRegr`][mlr3::LearnerRegr] into a [`PipeOp`] to predict multiple quantiles.
#'
#' [`PipeOpLearnerQuantiles`] only supports [`LearnerRegr`][mlr3::LearnerRegr]s that have `quantiles` as a possible `pedict_type`.
#'
#' It produces quantile-based predictions for multiple quantiles in one [`PredictionRegr`][mlr3::Prediction]. This is especially helpful if the [`LearnerRegr`][mlr3::LearnerRegr] can only predict one quantile (like for example [`LearnerRegrGBM`][mlr3extralearners::LearnerRegrGBM])
#'
#' Inherits the `$param_set` (and therefore `$param_set$values`) from the [`Learner`][mlr3::Learner] it is constructed from.
#'
#' @section Construction:
#' ```
#' PipeOpLearnerQuantiles$new(learner, id = NULL, param_vals = list())
#' ```
#'
#' * `learner` :: [`Learner`][mlr3::Learner] | `character(1)`\cr
#'   [`Learner`][mlr3::Learner] to wrap, or a string identifying a [`Learner`][mlr3::Learner] in the [`mlr3::mlr_learners`] [`Dictionary`][mlr3misc::Dictionary].
#'   The [`Learner`][mlr3::Learner] has to be a [`LearnerRegr`][mlr3::LearnerRegr] with `predict_type` `"quantiles"`.
#'   This argument is always cloned; to access the [`Learner`][mlr3::Learner] inside `PipeOpLearnerQuantiles` by-reference, use `$learner`.
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, internally defaulting to the `id` of the [`Learner`][mlr3::Learner] being wrapped.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpLearnerQuantiles`] has one input channel named `"input"`, taking a [`TaskRegr`][mlr3::TaskRegr] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' [`PipeOpLearnerQuantiles`] has one output channel named `"output"`, producing `NULL` during training and a [`PredictionRegr`][mlr3::Prediction] object
#' during prediction.
#'
#' The output during prediction is a [`PredictionRegr`][mlr3::PredictionRegr] on the prediction input data that aggregates all `result`s produced by the [`Learner`][mlr3::Learner] for each quantile in `quantiles`.
#' trained on the training input data.
#'
#' @section State:
#' The `$state` is set during training. It is a named `list` with the member:
#' * `model_states` :: `list`\cr
#'   List of the states of all models created by the [`Learner`][mlr3::Learner]'s `$.train()` function.
#'
#' @section Parameters:
#' The parameters are exactly the parameters of the [`Learner`][mlr3::Learner] wrapped by this object.
#' * `q_vals` :: `numeric`\cr
#'   Quantiles to use for training and prediction.
#'   Initialized to `c(0.05, 0.5, 0.95)`
#'
#' * `q_response` :: `numeric(1)`\cr
#'   Which quantile in `quantiles` to use as a `response` for the [`PredictionRegr`][mlr3::PredictionRegr] during prediction.
#'   Initialized to `0.5`.
#'
#' @section Internals:
#' The `$state` is updated during training.
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`], as well as:
#' * `learner` :: [`LearnerRegr`][mlr3::LearnerRegr]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. Read-only.
#' * `learner_model` :: [`Learner`][mlr3::Learner]\cr
#'   If [`PipeOpLearnerQuantiles`] has been trained, this is a `list` containing the [`Learner`][mlr3::Learner]s for each quantile.
#'   Otherwise, this contains the [`Learner`][mlr3::Learner] that is being wrapped.
#'   Read-only.
#' * `predict_type`\cr
#'   Predict type of the [`PipeOpLearnerQuantiles`], which is always `"response"  "quantiles"`.
#'
#' @section Methods:
#' Methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Meta PipeOps
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("boston_housing")
#' learner = lrn("regr.debug")
#' po = mlr_pipeops$get("learner_quantiles", learner)
#'
#' po$train(list(task))
#' po$predict(list(task))
PipeOpLearnerQuantiles = R6Class("PipeOpLearnerQuantiles",
  inherit = PipeOp,
  public = list(
   initialize = function(learner, id = NULL, param_vals = list()) {
     private$.learner = as_learner(learner, clone = TRUE)
     id = id %??% private$.learner$id
     type = private$.learner$task_type

     if ("regr" != type) {
       stop("PipeOpLearnerQuantiles only supports regression.")
     }

     task_type = mlr_reflections$task_types[type, mult = "first"]$task
     out_type  = mlr_reflections$task_types[type, mult = "first"]$prediction

     # paradox requirements 1.0
     private$.quantiles_param_set = ps(
       q_vals = p_uty(custom_check = crate(function(x) {
         checkmate::check_numeric(x, lower = 0L, upper = 1L, any.missing = FALSE, min.len = 1L, sorted = TRUE)
       }), tags = c("train", "predict", "required")),
       q_response = p_dbl(lower = 0L, upper = 1L, tags = c("train", "predict", "required"))
      )

     private$.quantiles_param_set$values = list(q_vals = c(0.05, 0.5, 0.95), q_response = 0.5)  # default

     super$initialize(id, param_set = alist(quantiles = private$.quantiles_param_set, private$.learner$param_set),
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
         map(state$model_states, clone_with_state, learner = private$.learner)
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
   .state_class = "pipeop_learner_quantiles_state",

   .train = function(inputs) {
     task = inputs[[1L]]
     pv = private$.quantiles_param_set$values

     assert_subset(pv$q_response, pv$q_vals, empty.ok = FALSE)
     if ("quantiles" %nin% private$.learner$predict_types) {
       stopf("Learner needs to be able to predict quantiles.")
     }
     private$.learner$predict_type = "quantiles"

     # train learner on all quantiles in q_vals
     states = map(pv$q_vals, function(quantile) {
       on.exit({private$.learner$state = NULL})
       private$.learner$quantiles = quantile
       private$.learner$train(task)$state
     })

     # add states of trained models to PipeOp state
     self$state = list(model_states = states)

     list(NULL)
   },

   .predict = function(inputs) {
     task = inputs[[1L]]
     pv = private$.quantiles_param_set$values

     prds = pmap(list(self$state$model_states, pv$q_vals), function(state, quantile) {
       on.exit({private$.learner$state = NULL})
       private$.learner$state = state
       private$.learner$quantiles = quantile
       as.data.table(private$.learner$predict(task))
     })

     quantiles = as.matrix(map_dtc(prds, "response"))
     unname(quantiles)
     attr(quantiles, "probs") = pv$q_vals
     attr(quantiles, "response") = pv$q_response

     # return quantile PredictionRegr with all requested quantiles
     list(as_prediction(as_prediction_data(list(quantiles = quantiles), task = task)))
   },

   .quantiles_param_set = NULL,
   .learner = NULL,
   .additional_phash_input = function() private$.learner$phash
  )
)

#' @export
marshal_model.pipeop_learner_quantiles_state = function(model, inplace = FALSE, ...) {
  # Note that a Learner state contains other reference objects, but we don't clone them here, even when inplace
  # is FALSE. For our use-case this is just not necessary and would cause unnecessary overhead in the mlr3
  # workhorse function
  model$model_states = map(model$model_states, marshal_model, inplace = inplace)
  # only wrap this in a marshaled class if the model was actually marshaled above
  # (the default marshal method does nothing)
  if (some(model$model_states, is_marshaled_model)) {
    model = structure(
      list(marshaled = model, packages = "mlr3pipelines"),
      class = c(paste0(class(model), "_marshaled"), "marshaled")
    )
  }
  model
}

#' @export
unmarshal_model.pipeop_learner_quantiles_state_marshaled = function(model, inplace = FALSE, ...) {
  state_marshaled = model$marshaled
  state_marshaled$model_states = map(state_marshaled$model_states, unmarshal_model, inplace = inplace)
  state_marshaled
}


mlr_pipeops$add("learner_quantiles", PipeOpLearnerQuantiles, list(R6Class("Learner", public = list(id = "learner_quantiles", task_type = "regr", param_set = ps(), packages = "mlr3pipelines"))$new()))
