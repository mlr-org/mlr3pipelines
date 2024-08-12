#' @title Split a Classification Task into Binary Classification Tasks
#'
#' @usage NULL
#' @name mlr_pipeops_ovrsplit
#' @format [`R6Class`][R6::R6Class] inheriting from [`PipeOp`].
#'
#' @description
#' Splits a [classification Task][mlr3::TaskClassif] into several binary [classification
#' Tasks][mlr3::TaskClassif] to perform "One vs. Rest" classification. This works in combination
#' with [`PipeOpOVRUnite`].
#'
#' For each target level a new binary [classification Task][mlr3::TaskClassif] is constructed with
#' the respective target level being the positive class and all other target levels being the
#' new negative class `"rest"`.
#'
#' This [`PipeOp`] creates a [`Multiplicity`], which means that subsequent [`PipeOp`]s are executed
#' multiple times, once for each created [binary Task][mlr3::TaskClassif], until a [`PipeOpOVRUnite`]
#' is reached.
#'
#' Note that [`Multiplicity`] is currently an experimental features and the implementation or UI
#' may change.
#'
#' @section Construction:
#' ```
#' PipeOpOVRSplit$new(id = "ovrsplit", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, default `"ovrsplit"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpOVRSplit`] has one input channel named `"input"` taking a [`TaskClassif`][mlr3::TaskClassif]
#' both during training and prediction.
#'
#' [`PipeOpOVRSplit`] has one output channel named `"output"` returning a [`Multiplicity`] of
#' [`TaskClassif`][mlr3::TaskClassif]s both during training and prediction, i.e., the newly
#' constructed binary [classification Tasks][mlr3::TaskClassif].
#'
#' @section State:
#' The `$state` contains the original target levels of the [`TaskClassif`][mlr3::TaskClassif] supplied
#' during training.
#'
#' @section Parameters:
#' [`PipeOpOVRSplit`] has no parameters.
#'
#' @section Internals:
#' The original target levels stored in the `$state` are also used during prediction when creating the new
#' binary [classification Tasks][mlr3::TaskClassif].
#'
#' The names of the element of the output [`Multiplicity`] are given by the levels of the target.
#'
#' If a target level `"rest"` is present in the input [`TaskClassif`][mlr3::TaskClassif], the
#' negative class will be labeled as `"rest." (using as many `"."` postfixes needed to yield a
#' valid label).
#'
#' Should be used in combination with [`PipeOpOVRUnite`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#' @family PipeOps
#' @family Multiplicity PipeOps
#' @family Experimental Features
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
#' @examples
#' \dontshow{ if (requireNamespace("rpart")) \{ }
#' library(mlr3)
#' task = tsk("iris")
#' po = po("ovrsplit")
#' po$train(list(task))
#' po$predict(list(task))
#' \dontshow{ \} }
PipeOpOVRSplit = R6Class("PipeOpOVRSplit",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "ovrsplit", param_vals = list()) {
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = "input", train = "TaskClassif", predict = "TaskClassif"),
        output = data.table(name = "output", train = "[TaskClassif]", predict = "[TaskClassif]"),
        tags = c("target transform", "multiplicity")
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      task = inputs[[1]]
      self$state = list(levels = levels(task$truth()))
      list(as.Multiplicity(private$.splittask(task, self$state$levels)))
    },
    .predict = function(inputs) {
      list(as.Multiplicity(private$.splittask(inputs[[1]], self$state$levels)))
    },
    .splittask = function(task, levels) {
      rest = "rest"
      while (rest %in% levels) {
        rest = paste0(rest, ".")
      }
      sapply(levels, function(l) {
        truthcol = task$data(cols = task$target_names)
        truthcol[[1]] = factor(ifelse(truthcol[[1]] == l, l, rest), levels = c(l, rest))
        convert_task(task$clone(deep = TRUE)$cbind(truthcol))
      }, simplify = FALSE)
    }
  )
)

mlr_pipeops$add("ovrsplit", PipeOpOVRSplit)

#' @title Unite Binary Classification Tasks
#'
#' @usage NULL
#' @name mlr_pipeops_ovrunite
#' @format [`R6Class`][R6::R6Class] inheriting from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @description
#' Perform "One vs. Rest" classification by (weighted) majority vote prediction from [classification
#' Predictions][mlr3::PredictionClassif]. This works in combination with [`PipeOpOVRSplit`].
#'
#' Weights can be set as a parameter; if none are provided, defaults to equal weights for each
#' prediction.
#'
#' Always returns a `"prob"` prediction, regardless of the incoming [`Learner`][mlr3::Learner]'s
#' `$predict_type`. The label of the class with the highest predicted probability is selected as the
#' `"response"` prediction.
#'
#' Missing values during prediction are treated as each class label being equally likely.
#'
#' This [`PipeOp`] uses a [`Multiplicity`] input, which is created by [`PipeOpOVRSplit`] and causes
#' [`PipeOp`]s on the way to this [`PipeOp`] to be called once for each individual [binary Task][mlr3::TaskClassif].
#'
#' Note that [`Multiplicity`] is currently an experimental features and the implementation or UI
#' may change.
#'
#' @section Construction:
#' ```
#' PipeOpOVRUnite$new(id = "ovrunite", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, default `"ovrunite"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpEnsemble`]. Instead of a
#' [`Prediction`][mlr3::Prediction], a [`PredictionClassif`][mlr3::PredictionClassif] is used as
#' input and output during prediction and [`PipeOpEnsemble`]'s `collect` parameter is initialized
#' with `TRUE` to allow for collecting a [`Multiplicity`] input.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOpEnsemble`].
#'
#' @section Internals:
#' Inherits from [`PipeOpEnsemble`] by implementing the `private$.predict()` method.
#'
#' Should be used in combination with [`PipeOpOVRSplit`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpEnsemble`]/[`PipeOp`].
#' @family PipeOps
#' @family Ensembles
#' @family Multiplicity PipeOps
#' @family Experimental Features
#' @template seealso_pipeopslist
#' @include PipeOpEnsemble.R
#' @export
#' @examples
#' \dontshow{ if (requireNamespace("rpart")) \{ }
#' library(mlr3)
#' task = tsk("iris")
#' gr = po("ovrsplit") %>>% lrn("classif.rpart") %>>% po("ovrunite")
#' gr$train(task)
#' gr$predict(task)
#' gr$pipeops$classif.rpart$learner$predict_type = "prob"
#' gr$predict(task)
#' \dontshow{ \} }
PipeOpOVRUnite = R6Class("PipeOpOVRUnite",
  inherit = PipeOpEnsemble,
  public = list(
    initialize = function(id = "ovrunite", param_vals = list()) {
      super$initialize(0, TRUE, id, param_vals = param_vals, prediction_type = "PredictionClassif", tags = "multiplicity")
    }
  ),
  private = list(
    .predict = function(inputs) {
      if (private$.collect) {
        inputs = unclass(inputs[[1]])
      }
      weights = self$param_set$values$weights
      row_ids = inputs[[1]]$row_ids
      map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))
      if (length(weights) == 1) weights = rep(1, length(inputs))
      assert_numeric(weights, any.missing = FALSE, len = length(inputs))
      has_probs = every(inputs, function(x) "prob" %in% x$predict_types)
      has_classif_response = every(inputs, function(x) "response" %in% x$predict_types)

      names(inputs) = map_chr(inputs, function(x) levels(x$truth)[[1]])

      truth = factor(do.call(fcoalesce, map(inputs, function(x) ifelse(as.numeric(x$truth) == 1, levels(x$truth)[[1]], NA_character_))),
        levels = names(inputs))

      if (has_probs) {
        probmat = sapply(inputs, function(x) x$prob[, 1])
      } else if (has_classif_response) {
        probmat = sapply(inputs, function(x) 2 - as.numeric(x$response))
      } else {
        stop("PipeOpOVRUnite input predictions had missing 'prob' and missing 'response' values. At least one of them must be given for all predictions.")
      }
      probmat = sweep(probmat, 2, weights, "*")
      probmat = probmat / rowSums(probmat)
      probmat[is.na(probmat)] = 1 / length(inputs)
      list(PredictionClassif$new(row_ids = row_ids, truth = truth, prob = pmin(pmax(probmat, 0), 1)))
    }
  )
)

mlr_pipeops$add("ovrunite", PipeOpOVRUnite)
