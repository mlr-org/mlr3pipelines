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
#' Inherits the `$param_set` and `$param_vals` from the `Learner` it is constructed from.
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
    initialize = function(learner, id = learner$id) {
      assert_learner(learner)
      self$learner = learner

      private$.crossval_param_set = ParamSet$new(params = list(
        ParamFct$new("resampling", values = "cv", default = "cv"),
        ParamInt$new("folds", lower = 2L, upper = Inf, default = 3L)
        )
      )
      private$.crossval_param_vals = list(resampling = "cv", folds = 3)

      super$initialize(id, can_subset_cols = FALSE)
    },

    train_task= function(task) {
      learner = self$learner$clone(deep = TRUE)

      # Train a learner for predicting
      self$state = learner$train(task)

      # Compute CV Predictions
      rdesc = mlr_resamplings$get(self$param_vals[["resampling"]])
      rdesc$param_vals = list(folds = self$param_vals[["folds"]])
      res = resample(task, learner, rdesc)
      prds = do.call("rbind", map(res$data$prediction, function(x) as.data.table(x)))

      private$pred_to_task(prds, task)
    },

    predict_task = function(task) {
      prediction = self$state$predict(task)
      newtsk = private$pred_to_task(prediction, task)
    }
  ),

  private = list(
    pred_to_task = function(prds, task) {
      prds = as.data.table(prds)
      prds[, truth := NULL]
      renaming = setdiff(colnames(prds), "row_id")
      setnames(prds, renaming, paste(self$id, renaming, sep = "."))
      setnames(prds, "row_id", task$backend$primary_key)
      task$clone()$select(character(0))$cbind(prds)
    },
    .crossval_param_set = NULL,
    .crossval_param_vals = NULL
  ),

  active = list(
    param_set = function() {
      union_param_sets(list(self$learner$param_set, private$.crossval_param_set))
    },

    param_vals = function(vals) {
      # union param_vals of self$learner and self
      union_param_vals(list(self$learner$param_set, private$.crossval_param_set),
        list(self$learner, private), c("param_vals", ".crossval_param_vals"), vals)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("learner_cv", PipeOpLearnerCV)
