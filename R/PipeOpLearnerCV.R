#' @title PipeOpLearnerCV
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
#' In the case of the resampling method returning multiple predictions per row id, the predictions are aggregated via their mean
#' (except for the `"response"` in the case of a [classification Task][mlr3::TaskClassif] which is aggregated using the mode).
#' In the case of the resampling method not returning predictions for all row ids as given in the input [`Task`][mlr3::Task],
#' these predictions are added as missing.
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
#' The output is a task with the same target as the input task, with features replaced by predictions made by the [`Learner`][mlr3::Learner].
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
    initialize = function(learner, resampling = rsmp("cv", folds = 3), id = NULL, param_vals = list()) {
      private$.learner = as_learner(learner, clone = TRUE)
      private$.learner$param_set$set_id = ""
      private$.resampling = as_resampling(resampling, clone = TRUE)
      private$.resampling$param_set$set_id = "resampling"

      # tags of resampling parameters should include "train"; we fix this here
      for (i in seq_along(private$.resampling$param_set$params)) {
        private$.resampling$param_set$params[[i]]$tags = c("train", private$.resampling$param_set$params[[i]]$tags)
      }


      id = id %??% private$.learner$id
      # FIXME: probably should restrict to only classif and regr because of the potential aggregation being done below
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

      # Train a learner for predicting
      self$state = private$.learner$train(task)$state

      # Compute resampled Predictions
      rr = resample(task, private$.learner, private$.resampling)
      prds = as.data.table(rr$prediction(predict_sets = "test"))

      # Some resamplings will result in rows being sampled multiple times and some being missing
      nrows_multiple = length(prds$row_id[duplicated(prds$row_id)])
      missing_rows = setdiff(task$row_ids, prds$row_id)
      nrows_missing = length(missing_rows)

      if (!nrows_multiple && !nrows_missing) {
        return(private$pred_to_task(prds, task))  # early exit
      }

      task_type = task$task_type
      prds_names = colnames(prds)

      prds_corrected = if (nrows_multiple) {
        # classif: prob, regr: response, (se)
        SDcols_multiple = setdiff(prds_names, if (task_type == "classif") c("row_id", "truth", "response") else c("row_id", "truth"))

        # aggregation functions:
        #  - mean for prob, response (regr), se
        #  - mode for response (classif)
        prds_corrected = prds[, map(.SD, function(x) {
          if (length(x) == 1L) return(x)  # early exit
          mean(x, na.rm = TRUE)
        }), by = "row_id", .SDcols = SDcols_multiple]

        if (NROW(prds_corrected) == 0L) prds_corrected = unique(prds[, "row_id"])

        if (task_type == "classif") {
          cbind(prds_corrected, prds[, map(.SD, function(x) {
            if (length(x) == 1L) return(as.character(x))  # early exit
            tt = table(x)
            names(tt[which.max(tt)])
          }), by = "row_id", .SDcols = "response"][, "response"])
        } else {
          prds_corrected
        }
      } else {
        if (task_type == "classif") {
          prds[, "response" := as.character(response)]
        }
        prds[, !"truth"]
      }

      if (nrows_missing) {
        SDcols_missing = setdiff(prds_names, "truth")
        # add missings
        prds_corrected = prds_corrected[, map(.SD, add_missings, len = nrows_missing), .SDcols = SDcols_missing]
        prds_corrected$row_id[is.na(prds_corrected$row_id)] = missing_rows
      }

      if (task_type == "classif") {
        target = task$truth(prds_corrected$row_id)
        prds_corrected$response = factor(prds_corrected$response, levels = levels(target), ordered = is.ordered(target))
      }

      # FIXME: do we need additional safety checks here?

      private$pred_to_task(prds_corrected, task)
    },

    .predict_task = function(task) {
      on.exit({private$.learner$state = NULL})
      private$.learner$state = self$state
      prediction = as.data.table(private$.learner$predict(task))
      private$pred_to_task(prediction, task)
    },

    pred_to_task = function(prds, task) {
      if (!is.null(prds$truth)) prds[, truth := NULL]
      if (!self$param_set$values$keep_response && self$learner$predict_type == "prob") {
        prds[, response := NULL]
      }
      renaming = setdiff(colnames(prds), "row_id")
      setnames(prds, renaming, sprintf("%s.%s", self$id, renaming))
      setnames(prds, "row_id", task$backend$primary_key)
      task$select(character(0))$cbind(prds)
    },
    .additional_param_set = NULL,
    .learner = NULL,
    .resampling = NULL
  )
)

# helper function to add missings to predictions based on their storage mode
add_missings = function(x, len) {
  c(x, switch(typeof(x),
    "character" = rep_len(NA_character_, length.out = len),
    "double" = rep_len(NA_real_, length.out = len),
    "integer" = rep_len(NA_integer_, length.out = len)))
}

mlr_pipeops$add("learner_cv", PipeOpLearnerCV, list(R6Class("Learner", public = list(id = "learner_cv", task_type = "classif", param_set = ParamSet$new()))$new()))
