#' @title PipeOpThreshold
#'
#' @usage NULL
#' @name mlr_pipeops_threshold
#' @format [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Change the threshold of a `Prediction` during the `predict` step.
#' The incoming [`Learner`][mlr3::Learner]'s `$predict_type` needs to be `"prob"`.
#'
#' @section Construction:
#' ```
#' PipeOpThreshold$new(id = "threshold", param_vals = list())
#' ```
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"classifavg"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction.
#'   Default `numeric(0)`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOp`]. A [`PredictionClassif`][mlr3::PredictionClassif]
#' is required as input and returned as output during prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' * `thresholds` :: `numeric`\cr
#' A numeric vector of thresholds for the different class levels.
#' For binary tasks, this can be a single number, else a vector.
#' Has to have the same length as number of class levels.
#' Defaults to `numeric(0)`, i.e. no threshold adjustment is performed.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#' @family PipeOps
#' @export
#'
#' @examples
#' library("mlr3")
#' t = tsk("german_credit")
#' gr = po(lrn("classif.rpart", predict_type = "prob")) %>>%
#'   po("threshold", param_vals = list(thresholds = 0.9))
#' gr$train(t)
#' gr$predict(t)
PipeOpThreshold = R6Class("PipeOpThreshold",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "threshold", param_set = ParamSet$new(), param_vals = list(), packages = character(0), prediction_type = "Prediction") {
      param_set$add(ParamUty$new("thresholds", custom_check = check_numeric, tags = "predict"))
      param_set$values$thresholds = numeric(0)
      super$initialize(id, param_set = param_set, param_vals = param_vals, packages = packages,
        input = data.table(name = "input", train = "NULL", predict = "PredictionClassif"),
        output = data.table(name = "output", train = "NULL", predict = "PredictionClassif"))
    },
    train_internal = function(inputs) {
      self$state = list()
      list(NULL)
    },
    predict_internal = function(inputs) {
      prds = inputs[[1]]$clone()
      thr = self$param_set$values$thresholds
      if (length(thr) == 0) return(list(prds))
      assert_subset("prob", prds$predict_types)
      # Set names in case none are set.
      if (is.null(names(thr)) && length(thr) > 1) thr = set_names(thr, colnames(prds$prob))
      list(prds$set_threshold(thr))
    }
  )
)

mlr_pipeops$add("threshold", PipeOpThreshold)
