#' @title Change the Threshold of a Classification Prediction
#'
#' @usage NULL
#' @name mlr_pipeops_threshold
#' @format [`R6Class`][R6::R6Class] inheriting from [`PipeOp`].
#'
#' @description
#' Change the threshold of a `Prediction` during the `predict` step.
#' The incoming [`Learner`][mlr3::Learner]'s `$predict_type` needs to be `"prob"`.
#' Internally calls `PredictionClassif$set_threshold`.
#'
#' @section Construction:
#' ```
#' PipeOpThreshold$new(id = "threshold", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"threshold"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction.
#'   Defaults to `numeric(0)`.
#'
#' @section Input and Output Channels:
#' During training, the input and output are `NULL`.
#' A [`PredictionClassif`][mlr3::PredictionClassif] is required as input and returned as output during prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' * `thresholds` :: `numeric`\cr
#'   A numeric vector of thresholds for the different class levels.
#'   May have length 1 for binary classification predictions, must
#'   otherwise have length of the number of target classes; see
#'   [`PredictionClassif`][mlr3::PredictionClassif]'s `$set_threshold()` method.
#'   Initialized to `0.5`, i.e. thresholding for binary classification
#'   at level `0.5`.
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`], as well as:
#' * `predict_type` :: `character(1)`\cr
#'   Type of prediction to return. Either `"prob"` (default) or `"response"`.
#'   Setting to `"response"` should rarely be used; it may potentially save some memory but has
#'   no other benefits.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @examplesIf requireNamespace("rpart")
#' library("mlr3")
#' t = tsk("german_credit")
#' gr = po(lrn("classif.rpart", predict_type = "prob")) %>>%
#'   po("threshold", param_vals = list(thresholds = 0.9))
#' gr$train(t)
#' gr$predict(t)
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
PipeOpThreshold = R6Class("PipeOpThreshold",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "threshold", param_vals = list()) {
      param_set = ps(thresholds = p_uty(custom_check = check_numeric_valid_threshold, tags = "predict"))

      param_set$values$thresholds = 0.5
      super$initialize(id, param_set = param_set, param_vals = param_vals, packages = character(0),
        input = data.table(name = "input", train = "NULL", predict = "PredictionClassif"),
        output = data.table(name = "output", train = "NULL", predict = "PredictionClassif"),
        tags = "target transform")
    }
  ),
  active = list(
    predict_type = function(rhs) {
      if (!missing(rhs)) {
        assert_choice(rhs, c("prob", "response"))
        private$.predict_type = rhs
      }
      private$.predict_type
    }
  ),
  private = list(
    .predict_type = "prob",
    .train = function(inputs) {
      self$state = list()
      list(NULL)
    },
    .predict = function(inputs) {
      prd = inputs[[1]]$clone()
      thr = self$param_set$values$thresholds
      assert_subset("prob", prd$predict_types)
      if (length(thr) > 1) {
        if (length(thr) != nlevels(prd$truth)) {
          stop("'thresholds' parameter must have length one or length equal to number of outcome levels")
        }
        if (is.null(names(thr))) {
          # Set names in case none are set.
          names(thr) = colnames(prd$prob)
        }
      }

      prd$set_threshold(thr)
      if (self$predict_type == "response") {
        prd$predict_types = "response"
        prd$data$prob = NULL
      }
      list(prd)
    }
  )
)

mlr_pipeops$add("threshold", PipeOpThreshold)
