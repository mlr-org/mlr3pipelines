#' @title PipeOpImputeLearner
#'
#' @usage NULL
#' @name mlr_pipeops_imputelearner
#' @format [`R6Class`] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute numerical features by predicted values of a learner.
#'
#' @section Construction:
#' ```
#' PipeOpImputeLearner$new(id = "imputelearner", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"imputemean"`.
#' * `learner` :: [`Learner`][mlr3::Learner] | `character(1)`
#'   [`Learner`][mlr3::Learner] to wrap, or a string identifying a [`Learner`][mlr3::Learner] in the [`mlr3::mlr_learners`] [`Dictionary`][mlr3misc::Dictionary].
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpImpute`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features missing values imputed by first training the provided learner and then predicting the expected value.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpImpute`].
#'
#' The `$state$model` is a named `list` of `numeric(n)` indicating the predicted values of the respective feature.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`].
#'
#' @section Internals:
#' Uses the `$train` and `$predict` functions of the provided learner. Features that are entirely `NA` are imputed as `0`.
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
#' po = po("imputemean")
#' new_task = po$train(list(task = task))[[1]]
#' new_task$missings()
#'
#' po$state$model
#' @family PipeOps
#' @family Imputation PipeOps
#' @include PipeOpImpute.R
#' @export
PipeOpImputeLearner = R6Class("PipeOpImputeLearner",
                           inherit = PipeOpImpute,
                           public = list(
                             initialize = function(id = "imputelearner", learner, param_vals = list()) {
                               private$.learner = as_learner(learner)$clone(deep = TRUE)  # FIXME: use `clone=TRUE` when mlr-org/mlr3#344 is fixed

                               super$initialize(id, param_vals = param_vals, whole_task_dependent = TRUE)
                             },

                             select_cols = function(task) task$feature_types[get("type") %in% c("numeric", "integer"), get("id")],

                             train_imputer = function(feature, type, context) {

                               if (all(is.na(feature))) {
                                 pred = rep(0, length(feature))
                               } else {
                                 # TODO add check for missing variables in the imputation predictors
                                 context$impute_col = feature
                                 task = TaskRegr$new(id = "taskimpute", backend = context, target = "impute_col")
                                 private$.learner$train(task)

                                 pred = private$.learner$predict(task)
                               }

                               if (type == "integer") {
                                 pred = as.integer(round(pred))
                               }

                               pred
                             },

                             impute = function(feature, type, model, context) {
                               feature[is.na(feature)] = model$response[is.na(feature)]
                               feature
                             }
                           ),

                           private = list(
                             .affectcols_ps = NULL,
                             .learner = NULL
                           )
)

mlr_pipeops$add("imputelearner", PipeOpImputeLearner)
