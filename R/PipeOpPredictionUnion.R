#' @title PipeOpPredictionUnion
#'
#' @usage NULL
#' @name mlr_pipeops_predictionunion
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Unite predictions from all input predictions into a single
#' [`Prediction`][mlr3::Prediction].
#'
#' `task_type`s and `predict_types` must be equal across all input predictions.
#'
#' Note that predictions are combined as is, i.e., no checks for duplicated row
#' identifiers etc. are performed.
#'
#' Currently only supports task types `classif` and `regr` by constructing a new
#' [`PredictionClassif`][mlr3::PredictionClassif] and respectively
#' [`PredictionRegr`][mlr3::PredictionRegr].
#'
#' @section Construction:
#' ```
#' PipeOpPredictionUnion$new(innum = 0, id = "predictionunion", param_vals = list())
#' ```
#'
#' * `innum` :: `numeric(1)` | `character`\cr
#'   Determines the number of input channels. If `innum` is 0 (default), a vararg input channel is
#'   created that can take an arbitrary number of inputs. If `innum` is a `character` vector, the
#'   number of input channels is the length of `innum`.
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, default `"predictionunion"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpPredictionUnion`] has multiple input channels depending on the `innum` construction
#' argument, named `"input1"`, `"input2"`, ... if `innum` is nonzero; if `innum` is 0, there is only
#' one *vararg* input channel named `"..."`. All input channels take `NULL` during training and a
#' [`Prediction`][mlr3::Prediction] during prediction.
#'
#' [`PipeOpPredictionUnion`] has one output channel named `"output"`, producing `NULL` during
#' training and a [`Prediction`][mlr3::Prediction] during prediction.
#'
#' The output during prediction is a [`Prediction`][mlr3::Prediction] constructed by combining all
#' input [`Prediction`][mlr3::Prediction]s.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' [`PipeOpPredictionUnion`] has no Parameters.
#'
#' @section Internals:
#' Only sets the fields `row_ids`, `truth`, `response` and if applicable `prob` and `se` during
#' construction of the output [`Prediction`][mlr3::Prediction].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @include PipeOp.R
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' filter = expression(Sepal.Length < median(Sepal.Length))
#' gr = po("copy", outnum = 2) %>>% gunion(list(
#'   po("filterrows", id = "filter1",
#'     param_vals = list(filter = filter)) %>>%
#'   lrn("classif.rpart", id = "learner1"),
#'   po("filterrows", id = "filter2",
#'      param_vals = list(filter = filter, invert = TRUE)) %>>%
#'   lrn("classif.rpart", id = "learner2")
#' )) %>>% po("predictionunion")
#'
#' gr$train(task)
#' gr$predict(task)
PipeOpPredictionUnion = R6Class("PipeOpPredictionUnion",
  inherit = PipeOp,
  public = list(
    initialize = function(innum = 0L, id = "predictionunion", param_vals = list()) {
      assert(
        check_int(innum, lower = 0L),
        check_character(innum, min.len = 1L, any.missing = FALSE)
      )
      if (!is.numeric(innum)) {
        innum = length(innum)
      }
      inname = if (innum) rep_suffix("input", innum) else "..."
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = inname, train = "NULL", predict = "Prediction"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction"))
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      list(NULL)
    },
    .predict = function(inputs) {
      # currently only works for task_type "classif" or "regr"
      check = all((unlist(map(inputs[-1L], .f = `[[`, "task_type")) == inputs[[1L]]$task_type) &
        unlist(map(inputs[-1L], .f = `[[`, "predict_types")) == inputs[[1L]]$predict_types)
      if (!check) {
        stopf("Can only unite predictions of the same task type and predict types.")
      }

      type = inputs[[1L]]$task_type
      if (type %nin% c("classif", "regr")) {
        stopf("Currently only supports task types `classif` and `regr`.")
      }

      row_ids = unlist(map(inputs, .f = `[[`, "row_ids"), use.names = FALSE)
      truth = unlist(map(inputs, .f = `[[`, "truth"), use.names = FALSE)
      response = unlist(map(inputs, .f = `[[`, "response"), use.names = FALSE)

      prediction = 
      if(type == "classif") {
        prob = do.call(rbind, map(inputs, .f = `[[`, "prob"))
        PredictionClassif$new(row_ids = row_ids, truth = truth, response = response, prob = prob)
      } else {
        se = unlist(map(inputs, .f = `[[`, "se"), use.names = FALSE)
        if (length(se) == 0L) se = NULL
        PredictionRegr$new(row_ids = row_ids, truth = truth, response = response, se = se)
      }

      list(prediction)
    }
  )
)

mlr_pipeops$add("predictionunion", PipeOpPredictionUnion)
