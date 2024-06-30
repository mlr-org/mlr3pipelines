#' @title Generate a Randomized Response Prediction
#'
#' @usage NULL
#' @name mlr_pipeops_randomresponse
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Takes in a [`Prediction`][mlr3::Prediction] of `predict_type` `"prob"`
#' (for [`PredictionClassif`][mlr3::PredictionClassif]) or `"se"`
#' (for [`PredictionRegr`][mlr3::PredictionRegr]) and generates a randomized `"response"`
#' prediction.
#'
#' For `"prob"`, the responses are sampled according to
#' the probabilities of the input [`PredictionClassif`][mlr3::PredictionClassif]. For `"se"`,
#' responses are randomly drawn according to the `rdistfun` parameter (default is [`rnorm`]) by using
#' the original responses of the input [`PredictionRegr`][mlr3::PredictionRegr] as the mean and the
#' original standard errors of the input [`PredictionRegr`][mlr3::PredictionRegr] as the standard
#' deviation (sampling is done observation-wise).
#'
#' @section Construction:
#' ```
#' PipeOpRandomResponse$new(id = "randomresponse", param_vals = list(), packages = character(0))
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, default `"randomresponse"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#' * packages :: `character`\cr
#'   Set of all required packages for the `private$.predict()` methods related to the `rdistfun`
#'   parameter. Default is `character(0)`.
#'
#' @section Input and Output Channels:
#' [`PipeOpRandomResponse`] has one input channel named `"input"`, taking `NULL` during training and
#' a [`Prediction`][mlr3::Prediction] during prediction.
#'
#' [`PipeOpRandomResponse`] has one output channel named `"output"`, producing `NULL` during
#' training and a [`Prediction`][mlr3::Prediction] with random responses during prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' * `rdistfun` :: `function` \cr
#'   A function for generating random responses when the predict type is `"se"`. This function must
#'   accept the arguments `n` (integerish number of responses), `mean` (`numeric` for the mean),
#'   and `sd` (`numeric` for the  standard deviation), and must *vectorize* over `mean`
#'   and `sd`. Default is [`rnorm`].
#'
#' @section Internals:
#' If the `predict_type` of the input [`Prediction`][mlr3::Prediction] does not match `"prob"` or
#' `"se"`, the input [`Prediction`][mlr3::Prediction] will be returned unaltered.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
#' @examples
#' \dontshow{ if (requireNamespace("rpart")) \{ }
#' library(mlr3)
#' library(mlr3learners)
#'
#' task1 = tsk("iris")
#' g1 = LearnerClassifRpart$new() %>>% PipeOpRandomResponse$new()
#' g1$train(task1)
#' g1$pipeops$classif.rpart$learner$predict_type = "prob"
#' set.seed(2409)
#' g1$predict(task1)
#'
#' task2 = tsk("mtcars")
#' g2 = LearnerRegrLM$new() %>>% PipeOpRandomResponse$new()
#' g2$train(task2)
#' g2$pipeops$regr.lm$learner$predict_type = "se"
#' set.seed(2906)
#' g2$predict(task2)
#' \dontshow{ \} }
PipeOpRandomResponse = R6Class("PipeOpRandomResponse",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "randomresponse", param_vals = list(), packages = character(0L)) {
      ps = ps(
        rdistfun = p_uty(tags = c("predict", "required"), custom_check = crate(function(x) {
          check_function(x, args = c("n", "mean", "sd"))
        }))
      )
      ps$values = list(rdistfun = stats::rnorm)
      super$initialize(id = id, param_set = ps, param_vals = param_vals, packages = packages,
        input = data.table(name = "input", train = "NULL", predict = "Prediction"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction")
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      list(NULL)
    },
    .predict = function(inputs) {
      prediction = inputs[[1L]]
      predict_type = intersect(prediction$predict_types, c("prob", "se"))
      if (!length(predict_type)) {
        return(list(prediction))  # early exit for predict_types that are neither "prob" nor "se"
      }
      response = switch(predict_type,
        "prob" = {
          # apply does annoying things if prob has 0 rows
          values = if (nrow(prediction$data$prob)) {
            apply(prediction$data$prob, 1L, function(row) sample(levels(prediction$truth), 1L, prob = row))
          }
          factor(values, levels = levels(prediction$truth), ordered = is.ordered(prediction$truth))
        },
        "se" = {
          tab = as.data.table(prediction)
          tab[, self$param_set$values$rdistfun(n = get(".N"), mean = get("response"), sd = get("se"))]
        }
      )
      type = prediction$task_type
      list(get(mlr_reflections$task_types[type, mult = "first"]$prediction)$new(row_ids = prediction$row_ids,
        truth = prediction$truth, response = response))
    },
    .additional_phash_input = function() NULL
  )
)

mlr_pipeops$add("randomresponse", PipeOpRandomResponse)
