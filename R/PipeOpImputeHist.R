#' @title Impute Numerical Features by Histogram
#'
#' @usage NULL
#' @name mlr_pipeops_imputehist
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute numerical features by histogram.
#'
#' During training, a histogram is fitted on each column using R's [`hist()`][graphics::hist] function.
#' The fitted histogram is then sampled from for imputation. Sampling happens in a two-step process:
#' First, a bin is sampled from the histogram, then a value is sampled uniformly from the bin.
#' This is an approximation to sampling from the empirical training data distribution (i.e. sampling
#' from training data with replacement), but is much more memory efficient for large datasets, since the `$state`
#' does not need to save the training data.
#'
#' @section Construction:
#' ```
#' PipeOpImputeHist$new(id = "imputehist", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"imputehist"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpImpute`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features missing values imputed by (column-wise) histogram; see Description for details.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpImpute`].
#'
#' The `$state$model` is a named `list` of `list`s containing elements `$counts` and `$breaks`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`].
#'
#' @section Internals:
#' Uses the [`graphics::hist()`] function. Features that are entirely `NA` are imputed as `0`.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("pima")
#' task$missings()
#'
#' po = po("imputehist")
#' new_task = po$train(list(task = task))[[1]]
#' new_task$missings()
#'
#' po$state$model
#' @family PipeOps
#' @family Imputation PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpImpute.R
#' @export
PipeOpImputeHist = R6Class("PipeOpImputeHist",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "imputehist", param_vals = list()) {
      super$initialize(id, param_vals = param_vals, packages = "graphics", feature_types = c("integer", "numeric"))
    }
  ),
  private = list(

    .train_imputer = function(feature, type, context) {
      graphics::hist(feature, plot = FALSE)[c("counts", "breaks")]
    },

    .impute = function(feature, type, model, context) {
      if (is.atomic(model)) {  # handle nullmodel
        return(super$.impute(feature, type, model, context))
      }
      which.bins = sample.int(length(model$counts), count_missing(feature), replace = TRUE, prob = model$counts)
      sampled = stats::runif(length(which.bins), model$breaks[which.bins], model$breaks[which.bins + 1L])
      if (type == "integer") {
        sampled = round(sampled)
        # make sure we get an integer. this is faster than pmin(pmax(...)).
        sampled[sampled > .Machine$integer.max] = .Machine$integer.max
        sampled[sampled < -.Machine$integer.max] = -.Machine$integer.max
        sampled = as.integer(sampled)
      }
      feature[is.na(feature)] = sampled
      feature
    }
  )
)

mlr_pipeops$add("imputehist", PipeOpImputeHist)
