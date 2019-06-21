#' @title PipeOpLearnerCV
#' @name mlr_pipeop_learner_cv
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Wraps an [`mlr3::Learner`] into a [`PipeOp`].
#' Returns cross-validated predictions during training phase and stores a model of the
#' `Learner` trained on the whole data in `$state` as a new `Task` with the predictions;
#' usually a feature `<ID>.response` if the `Learner`'s `$predict.type` is `"response"`,
#' `<ID>.prob.<CLASS>` and `<ID>.response` for `$predict.type` `"prob"`, or `<ID>.response`
#' and `<ID>.se` for `$predict.type` `"se"`. `<ID>` is here the `$id` of the `PipeOpLearnerCV`.
#'
#' Returns this model's prediction during prediction phase, as a new `Task` with a single
#' column.
#'
#' Inherits the `$param_set` and therefore `$param_set$values` from the `Learner` it is constructed from.
#'
#' @section Public Members / Active Bindings:
#' * `learner`  :: [`Learner`] \cr
#'   Learner to use for cross validation / prediction.
#' @section Methods:
#' * `PipeOpLearner$new(learner, id = learner$id)` \cr
#'   ([`Learner`], `character(1)`) -> `self` \cr
#'   Constructor. The given learner will be used for crossvalidation.
#'
#' @section Parameter Set:
#' * `resampling` :: `character(1)` \cr
#'   Which resampling method do we want to use. Currently only supports 'cv'.
#' * `folds`     :: `numeric(1)` \cr
#'   Number of cross validation folds.
#' * `...` \cr
#'   The [`ParamSet`] of `$learner` is also made available.
#' @family Pipeops
#' @family Meta PipeOps
#' @export
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpLearnerCV = R6Class("PipeOpLearnerCV",
  inherit = PipeOpTaskPreproc,
  public = list(
    learner = NULL,
    initialize = function(learner, id = learner$id, param_vals = list()) {
      assert_learner(learner)
      self$learner = learner$clone(deep = TRUE)
      self$learner$param_set$set_id = learner$id

      private$.crossval_param_set = ParamSet$new(params = list(
        ParamFct$new("resampling", levels = c("cv", "nocv"), default = "cv", tags = "required"),
        ParamInt$new("folds", lower = 2L, upper = Inf, default = 3L),
        ParamLgl$new("keep_response", default = FALSE, tags = "required")
      )
      )
      private$.crossval_param_set$values = list(resampling = "cv", folds = 3, keep_response = FALSE)
      private$.crossval_param_set$set_id = "resampling"

      super$initialize(id, self$param_set, param_vals = param_vals, can_subset_cols = FALSE)
    },

    train_task = function(task) {

      # Train a learner for predicting
      self$state = self$learner$train(task)$model

      pv = private$.crossval_param_set$values

      # Compute CV Predictions
      if (pv[["resampling"]] == "nocv") {
        rdesc = mlr_resamplings$get("custom")$instantiate(task, train_set = list(seq_len(task$nrow)), test_set = list(seq_len(task$nrow)))
      } else {
        rdesc = mlr_resamplings$get(pv[["resampling"]])
        rdesc$param_set$values = list(folds = pv[["folds"]])
      }
      res = resample(task, self$learner, rdesc)
      prds = do.call(rbind, lapply(res$data$prediction_data, new_prediction, task = task))


      private$pred_to_task(prds, task)
    },

    predict_task = function(task) {
      self$learner$model = self$state
      prediction = new_prediction(task, self$learner$predict(task))
      private$pred_to_task(prediction, task)
    }
  ),
  active = list(
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        private$.param_set = ParamSetCollection$new(list(
          private$.crossval_param_set,
          self$learner$param_set
        ))
        private$.param_set$set_id = self$id %??% self$learner$id # self$id may be NULL during initialize() call
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),
  private = list(
    deep_clone = function(name, value) {
      private$.param_set = NULL # required to keep clone identical to original, otherwise tests get really ugly
      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      value
    },
    pred_to_task = function(prds, task) {
      prds = as.data.table(prds)
      prds[, truth := NULL]
      if (!self$param_set$values$resampling.keep_response && self$learner$predict_type == "prob") {
        prds[, response := NULL]
      }
      if ("se" %in% colnames(prds) && self$learner$predict_type != "se") {  # TODO: temporary fix for mlr3#250
        prds[, se := NULL]
      }
      renaming = setdiff(colnames(prds), "row_id")
      setnames(prds, renaming, sprintf("%s.%s", self$id, renaming))
      setnames(prds, "row_id", task$backend$primary_key)
      task$clone()$select(character(0))$cbind(prds)
    },
    .crossval_param_set = NULL
  )
)

register_pipeop("learner_cv", PipeOpLearnerCV, list(R6Class("Learner", public = list(id = "learner_cv", param_set = ParamSet$new()))$new()))
