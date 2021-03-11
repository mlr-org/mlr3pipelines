#' @title Wrap a Learner into a PipeOp with Resampled Predictions as Features
#'
#' @usage NULL
#' @name mlr_pipeops_learner_cv
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Wraps a [`mlr3::Learner`] and [`mlr3::Resampling`] into a [`PipeOp`].
#'
#' Returns resampled predictions during training as a [`Task`][mlr3::Task] and stores a model of the
#' [`Learner`][mlr3::Learner] trained on the whole data in `$state`. This is used to create a similar
#' [`Task`][mlr3::Task] during prediction.
#'
#' The [`Task`][mlr3::Task] gets features depending on the capsuled [`Learner`][mlr3::Learner]'s
#' `$predict_type`. If the [`Learner`][mlr3::Learner]'s `$predict.type` is `"response"`, a feature `<ID>.response` is created,
#' for `$predict.type` `"prob"` the `<ID>.prob.<CLASS>` features are created, and for `$predict.type` `"se"` the new columns
#' are `<ID>.response` and `<ID>.se`. `<ID>` denotes the `$id` of the [`PipeOpLearnerCV`] object.
#'
#' In the case of the resampling method returning multiple predictions per row id, the predictions
#' are returned unaltered. The output [`Task`][mlr3::Task] always gains a `row_reference` column
#' named `pre.<ID>` indicating the original row id prior to the resampling process. [`PipeOpAggregate`] should then
#' be used to aggregate these multiple predictions per row id.
#'
#' Inherits both the `$param_set` (and therefore `$param_set$values`) from the [`Learner`][mlr3::Learner] and
#' [`Resampling`][mlr3::Resampling] it is constructed from. The parameter ids of the latter one are prefixed with `"resampling."`
#' and the tags of these parameters are extended by `"train"`.
#'
#' [`PipeOpLearnerCV`] can be used to create "stacking" or "super learning" [`Graph`]s that use the output of one [`Learner`][mlr3::Learner]
#' as features for another [`Learner`][mlr3::Learner]. Because the [`PipeOpLearnerCV`] erases the original input features, it is often
#' useful to use [`PipeOpFeatureUnion`] to bind the prediction [`Task`][mlr3::Task] to the original input [`Task`][mlr3::Task].
#'
#' @section Construction:
#' ```
#' PipeOpLearnerCV$new(learner, resampling = rsmp("cv", folds = 3), id = NULL, param_vals = list())
#' ```
#'
#' * `learner` :: [`Learner`][mlr3::Learner] \cr
#'   [`Learner`][mlr3::Learner] to use for resampling / prediction.
#' * `resampling` :: [`Resampling`][mlr3::Resampling] \cr
#'   [`Resampling`][mlr3::Resampling] to use for resampling. Initialized to 3-fold cross-validation.
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, internally defaulting to the `id` of the [`Learner`][mlr3::Learner] being wrapped.
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
#' The output is a [`Task`][mlr3::Task] with the same target as the input [`Task`][mlr3::Task], with features replaced by predictions made by the [`Learner`][mlr3::Learner].
#' During training, this prediction is the out-of-sample prediction made by [`resample`][mlr3::resample], during prediction, this is the
#' ordinary prediction made on the data by a [`Learner`][mlr3::Learner] trained on the training phase data.
#'
#' @section State:
#' The `$state` is set to the `$state` slot of the [`Learner`][mlr3::Learner] object, together with the `$state` elements inherited from the
#' [`PipeOpTaskPreproc`]. It is a named `list` with the inherited members, as well as:
#' * `model` :: `any`\cr
#'   Model created by the [`Learner`][mlr3::Learner]'s `$.train()` function.
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
#' The parameters are the parameters inherited from the [`PipeOpTaskPreproc`], as well as the parameters of the [`Learner`][mlr3::Learner] and
#' [`Resampling`][mlr3::Resampling] wrapped by this object.
#' Besides that, parameters introduced are:
#' * `keep_response` :: `logical(1)`\cr
#'   Only effective during `"prob"` prediction: Whether to keep response values, if available. Initialized to `FALSE`.
#'
#' @section Internals:
#' The `$state` is currently not updated by prediction, so the `$state$predict_log` and `$state$predict_time` will always be `NULL`.
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`], as well as:
#' * `learner` :: [`Learner`][mlr3::Learner]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. Read-only.
#' * `learner_model` :: [`Learner`][mlr3::Learner]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. This learner contains the model if the `PipeOp` is trained. Read-only.
#' * `resampling` :: [`Resampling`][mlr3::Resampling]\cr
#'   [`Resampling`][mlr3::Resampling] that is being wrapped. Read-only.
#'
#' @section Methods:
#' Methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family Pipeops
#' @family Meta PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#'
#' lrncv_po = po("learner_cv", learner, rsmp("cv"))
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
    initialize = function(learner, resampling = rsmp("cv", folds = 3), id = NULL, param_vals = list()) {
      private$.learner = as_learner(learner, clone = TRUE)
      private$.learner$param_set$set_id = ""
      private$.resampling = as_resampling(resampling, clone = TRUE)
      private$.resampling$param_set$set_id = "resampling"

      # tags of resampling parameters should include "train"; we fix this here
      for (i in seq_along(private$.resampling$param_set$params)) {
        private$.resampling$param_set$params[[i]]$tags = c("train", private$.resampling$param_set$params[[i]]$tags)
      }


      id = id %??% self$learner$id
      task_type = mlr_reflections$task_types[get("type") == private$.learner$task_type][order(get("package"))][1L]$task

      private$.additional_param_set = ParamSet$new(params = list(
        ParamLgl$new("keep_response", tags = c("train", "required"))
      ))
      private$.additional_param_set$values = list(keep_response = FALSE)
      private$.additional_param_set$set_id = ""

      super$initialize(id, param_set = alist(private$.resampling$param_set, private$.additional_param_set, private$.learner$param_set),
        param_vals = param_vals, can_subset_cols = TRUE, task_type = task_type, tags = c("learner", "ensemble"))
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
          stop("$learner is read-only.")
        }
      }
      if (is.null(self$state) || is_noop(self$state)) {
        private$.learner
      } else {
        multiplicity_recurse(self$state, clone_with_state, learner = private$.learner)
      }
    },
    resampling = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.resampling)) {
          stop("$resampling is read-only.")
        }
      }
      private$.resampling
    }
  ),
  private = list(
    .train_task = function(task) {
      on.exit({private$.learner$state = NULL})
      # train a learner for predicting
      self$state = private$.learner$train(task)$state

      # compute resampled predictions
      rr = resample(task, private$.learner, private$.resampling)
      prds = as.data.table(rr$prediction(predict_sets = "test"))

      private$.pred_to_task(prds, task)
    },

    .predict_task = function(task) {
      on.exit({private$.learner$state = NULL})
      private$.learner$state = self$state
      prds = as.data.table(private$.learner$predict(task))
      private$.pred_to_task(prds, task)
    },

    .pred_to_task = function(prds, task) {
      if (!self$param_set$values$keep_response && self$learner$predict_type == "prob") {
        prds[, response := NULL]
      }
      renaming = setdiff(colnames(prds), c("row_ids", "truth"))
      setnames(prds, old = renaming, new = sprintf("%s.%s", self$id, renaming))
      setnames(prds, old = "truth", new = task$target_names)
      row_reference = paste0("pre.", self$id)
      while (row_reference %in% task$col_info$id) {
        row_reference = paste0(row_reference, ".")
      }
      setnames(prds, old = "row_ids", new = row_reference)

      # the following is needed to pertain correct row ids in the case of e.g. cv
      # here we do not necessarily apply PipeOpAggregate later
      backend = if (identical(sort(prds[[row_reference]]), sort(task$row_ids))) {
        set(prds, j = task$backend$primary_key, value = prds[[row_reference]])
        as_data_backend(prds, primary_key = task$backend$primary_key)
      } else {
        as_data_backend(prds)
      }

      # get task_type from mlr_reflections and call constructor
      constructor = get(mlr_reflections$task_types[["task"]][chmatch(task$task_type, table = mlr_reflections$task_types[["type"]], nomatch = 0L)][[1L]])
      newtask = invoke(constructor$new, id = task$id, backend = backend, target = task$target_names, .args = task$extra_args)
      newtask$extra_args = task$extra_args
      newtask$set_col_roles(row_reference, "row_reference")

      newtask
    },
    .additional_param_set = NULL,
    .learner = NULL,
    .resampling = NULL
  )
)

mlr_pipeops$add("learner_cv", PipeOpLearnerCV, list(R6Class("Learner", public = list(id = "learner_cv", task_type = "classif", param_set = ParamSet$new()))$new()))

