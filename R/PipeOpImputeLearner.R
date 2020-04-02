#' @title PipeOpImputeLearner
#'
#' @usage NULL
#' @name mlr_pipeops_imputelearner
#' @format [`R6Class`] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute features using values predicted by a learner.
#'
#' @section Construction:
#' ```
#' PipeOpImputeLearner$new(id = "imputelearner", learner,  param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"imputelearner"`.
#' * `learner` :: [`Learner`][mlr3::Learner] | `character(1)`
#'   [`Learner`][mlr3::Learner] to wrap, or a string identifying a [`Learner`][mlr3::Learner] in the [`mlr3::mlr_learners`] [`Dictionary`][mlr3misc::Dictionary].
#'   The [`Learner`][mlr3::Learner] needs to be able to handle missing values, i.e. have the `missings` property.
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
#' The `$state$models` is a named `list` of `models` created by the [`Learner`][mlr3::Learner]'s `$.train()` function
#' for each column.
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
                               private$.learner = as_learner(learner)$clone(deep = TRUE) # FIXME: use `clone=TRUE` when mlr-org/mlr3#344 is fixed
                               super$initialize(id, param_vals = param_vals, whole_task_dependent = TRUE)
                             },

                             select_cols = function(task) {
                               # Choose columns to impute based on the supplied learner
                               if(private$.learner$task_type == "regr"){
                                 task$feature_types[get("type") %in% c("numeric", "integer"), get("id")]
                               } else {
                                 task$feature_types[get("type") %in% c("logical", "character", "factor", "ordered"), get("id")]
                               }
                             },

                             train_imputer = function(feature, type, context) {
                               on.exit({private$.learner$state = NULL})
                               if (is.null(context) || nrow(context) == 0) {
                                 context = data.table(const = rep(1, length(feature)))
                               }

                               task = private$.create_imputation_task(feature, context)
                               private$.learner$train(task, row_ids = which(!is.na(feature)))$state
                             },

                             impute = function(feature, type, model, context) {
                               on.exit({private$.learner$state = NULL})
                               private$.learner$state = self$state
                               
                               # Use the trained learner to perform the imputation
                               task = private$.create_imputation_task(feature, context)
                               pred = model$predict(task)

                               # Replace the missing values with imputed values of the correct format
                               imp_vals = pred$response[is.na(feature)]
                               imp_vals = private$.convert_to_target(imp_vals, type)

                               feature[is.na(feature)] = imp_vals
                               feature
                             }
                           ),
                           active = list(
                             id = function(val) {
                               if (!missing(val)) {
                                 private$.id = val
                                 private$.learner$param_set$set_id = val
                               }
                               private$.id
                             },
                             learner = function(val) {
                               if (!missing(val)) {
                                 if (!identical(val, private$.learner)) {
                                   stop("$learner is read-only.")
                                 }
                               }
                               private$.learner
                             }
                           ),
                           private = list(
                             .learner = NULL,
                             .create_imputation_task = function(feature, context) {
                               # Create a task that can be used by the learner based on imputation context
                               context[['impute_col']] = private$.convert_to_predictable(feature)

                               imp_tsk = if(private$.learner$task_type == "regr") TaskRegr else TaskClassif
                               imp_tsk$new(id = "taskimpute", backend = context, target = "impute_col")
                             },
                             .convert_to_predictable = function(feature) {
                               # Convert non-factor imputation targets to a factor
                               if (is.numeric(feature))
                                 feature
                               else
                                 factor(feature, ordered = FALSE)
                             },
                             .convert_to_target = function(feature, type) {
                               # Convert an imputed feature to its original type
                               if(type == "integer"){
                                 feature = round(feature)
                               }
                               func = paste0("as.", type)
                               invoke(func, args = list(feature))
                             }
                           )
)

mlr_pipeops$add("imputelearner", PipeOpImputeLearner,
                list(learner = R6Class("Learner", public = list(id = "learner",
                                                                task_type = "regr",
                                                                param_set = ParamSet$new()))$new()))
