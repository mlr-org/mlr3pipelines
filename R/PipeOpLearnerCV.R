#' @title PipeOpLearnerCV
#'
#' @usage NULL
#' @name mlr_pipeops_learner_cv
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Wraps an [`mlr3::Learner`] into a [`PipeOp`].
#'
#' Returns cross-validated predictions during training as a [`Task`][mlr3::Task] and stores a model of the
#' [`Learner`][mlr3::Learner] trained on the whole data in `$state`. This is used to create a similar
#' [`Task`][mlr3::Task] during prediction.
#'
#' The [`Task`][mlr3::Task] gets features depending on the capsuled [`Learner`][mlr3::Learner]'s
#' `$predict_type`. If the [`Learner`][mlr3::Learner]'s `$predict.type` is `"response"`, a feature `<ID>.response` is created,
#' for `$predict.type` `"prob"` the `<ID>.prob.<CLASS>` features are created, and for `$predict.type` `"se"` the new columns
#' are `<ID>.response` and `<ID>.se`. `<ID>` denotes the `$id` of the [`PipeOpLearnerCV`] object.
#'
#' Inherits the `$param_set` (and therefore `$param_set$values`) from the [`Learner`][mlr3::Learner] it is constructed from.
#'
#' [`PipeOpLearnerCV`] can be used to create "stacking" or "super learning" [`Graph`]s that use the output of one [`Learner`][mlr3::Learner]
#' as feature for another [`Learner`][mlr3::Learner]. Because the [`PipeOpLearnerCV`] erases the original input features, it is often
#' useful to use [`PipeOpFeatureUnion`] to bind the prediction [`Task`][mlr3::Task] to the original input [`Task`][mlr3::Task].
#'
#' @section Construction:
#' ```
#' PipeOpLearnerCV$new(learner, id = if (is.character(learner)) learner else learner$id, param_vals = list())
#' ```
#'
#' * `learner` :: [`Learner`][mlr3::Learner] \cr
#'   [`Learner`][mlr3::Learner] to use for cross validation / prediction, or a string identifying a
#'   [`Learner`][mlr3::Learner] in the [`mlr3::mlr_learners`] [`Dictionary`][mlr3misc::Dictionary].
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, defaulting to the `id` of the [`Learner`][mlr3::Learner] being wrapped.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpLearnerCV`] has one input channel named `"input"`, taking a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' [`PipeOpLearnerCV`] has one output channel named `"output"`, producing a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' The output is a task with the same target as the input task, with features replaced by predictions made by the [`Learner`][mlr3::Learner].
#' During training, this prediction is the out-of-sample prediction made by [`resample`][mlr3::resample], during prediction, this is the
#' ordinary prediction made on the data by a [`Learner`][mlr3::Learner] trained on the training phase data.
#'
#' @section State:
#' The `$state` is set to the `$state` slot of the [`Learner`][mlr3::Learner] object, together with the `$state` elements inherited from the
#' [`PipeOpTaskPreproc`]. It is a named `list` with the inherited members, as well as:
#' * `model` :: `any`\cr
#'   Model created by the [`Learner`][mlr3::Learner]'s `$train_internal()` function.
#' * `train_log` :: [`data.table`] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during training.
#' * `train_time` :: `numeric(1)`\cr
#'   Training time, in seconds.
#' * `predict_log` :: `NULL` | [`data.table`] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during prediction.
#' * `predict_time` :: `NULL` | `numeric(1)`
#'   Prediction time, in seconds.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOpTaskPreproc`], as well as the parameters of the [`Learner`][mlr3::Learner] wrapped by this object.
#' Besides that, parameters introduced are:
#' * `resampling.method` :: `character(1)`\cr
#'   Which resampling method do we want to use. Currently only supports `"cv"`.
#' * `resampling.folds` :: `numeric(1)`\cr
#'   Number of cross validation folds. Initialized to 3.
#' * `keep_response` :: `logical(1)`\cr
#'   Only effective during `"prob"` prediction: Whether to keep response values, if available. Initialized to `FALSE`.
#'
#' @section Internals:
#' The `$state` is currently not updated by prediction, so the `$state$predict_log` and `$state$predict_time` will always be `NULL`.
#'
#' @section Fields:
#' Fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`], as well as:
#' * `learner`  :: [`Learner`][mlr3::Learner]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. Read-only.
#'
#' @section Methods:
#' Methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family Pipeops
#' @family Meta PipeOps
#' @export
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#'
#' lrncv_po = po("learner_cv", learner)
#' lrncv_po$learner$predict_type = "response"
#'
#' nop = mlr_pipeops$get("nop")
#'
#' graph = gunion(list(
#'   lrncv_po,
#'   nop
#' )) %>>% po("featureunion")
#'
#' graph$train(task)
#'
#' graph$pipeops$classif.rpart$learner$predict_type = "prob"
#'
#' graph$train(task)
PipeOpLearnerCV = R6Class("PipeOpLearnerCV",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(learner, id = if (is.character(learner)) learner else learner$id, param_vals = list()) {
      private$.learner = as_learner(learner)$clone(deep = TRUE)  # FIXME: use `clone=TRUE` when mlr-org/mlr3#344 is fixed
      private$.learner$param_set$set_id = ""
      task_types = copy(mlr_reflections$task_types)
      setorder(task_types, package)
      task_type = task_types[type == private$.learner$task_type]$task[1L]

      private$.crossval_param_set = ParamSet$new(params = list(
        ParamFct$new("method", levels = "cv", tags = c("train", "required")),
        ParamInt$new("folds", lower = 2L, upper = Inf, tags = c("train", "required")),
        ParamLgl$new("keep_response", tags = c("train", "required"))
      ))
      private$.crossval_param_set$values = list(method = "cv", folds = 3, keep_response = FALSE)
      private$.crossval_param_set$set_id = "resampling"

      super$initialize(id, alist(private$.crossval_param_set, private$.learner$param_set), param_vals = param_vals, can_subset_cols = TRUE, task_type = task_type)
    },

    train_task = function(task) {
      on.exit({private$.learner$state = NULL})

      # Train a learner for predicting
      self$state = private$.learner$train(task)$state
      pv = private$.crossval_param_set$values

      # Compute CV Predictions
      rdesc = mlr_resamplings$get(pv[["method"]])
      rdesc$param_set$values = list(folds = pv[["folds"]])
      res = resample(task, private$.learner, rdesc)
      prds = rbindlist(lapply(map(res$data$prediction, "test"), as.data.table))

      private$pred_to_task(prds, task)
    },

    predict_task = function(task) {
      on.exit({private$.learner$state = NULL})
      private$.learner$state = self$state
      prediction = as.data.table(private$.learner$predict(task))
      private$pred_to_task(prediction, task)
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
    }
  ),
  private = list(
    pred_to_task = function(prds, task) {
      if (!is.null(prds$truth)) prds[, truth := NULL]
      if (!self$param_set$values$resampling.keep_response && self$learner$predict_type == "prob") {
        prds[, response := NULL]
      }
      renaming = setdiff(colnames(prds), "row_id")
      setnames(prds, renaming, sprintf("%s.%s", self$id, renaming))
      setnames(prds, "row_id", task$backend$primary_key)
      task$select(character(0))$cbind(prds)
    },
    .crossval_param_set = NULL,
    .learner = NULL
  )
)

mlr_pipeops$add("learner_cv", PipeOpLearnerCV, list(R6Class("Learner", public = list(id = "learner_cv", task_type = "classif", param_set = ParamSet$new()))$new()))
