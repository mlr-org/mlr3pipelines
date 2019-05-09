#' @title PipeOpBackupLearner
#'
#' @name mlr_pipeop_backup_learner
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#'   Uses an [`mlr3::Learner`] to repair predictions done by
#'   a previous [`PipeOpLearner`].
#'
#'   Inherits the `$param_set` and `$values` from the `Learner` it is constructed from.
#'
#' @section Public Members / Active Bindings:
#' * `learner`  :: [`Learner`] \cr
#'   Learner to use for backup prediction. Defaults to
#'   [`classif.featureless`][mlr3::mlr_learners_classif.featureless].
#'
#' @section Methods:
#' * `PipeOpLearner$new(learner = mlr_learners$get("classif.featureless", id = "backuplearner", predict_type = "prob"), id = "backuplearner")` \cr
#'   ([`Learner`], `character(1)`) -> `self` \cr
#'   Constructor. The given learner will be used for backup prediction.
#' @family PipeOps
#' @family Meta PipeOps
#' @include PipeOp.R
#' @export
PipeOpBackupLearner = R6Class("PipeOpBackupLearner", inherit = PipeOp,
  public = list(
    learner = NULL,

    initialize = function(learner = mlr_learners$get("classif.featureless", id = "backuplearner", predict_type = "prob"), id = "backuplearner", param_vals = list()) {
      assert_learner(learner)
      self$learner = learner$clone(deep = TRUE)
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = c("learnerin", "taskin"), train = c("NULL", "Task"), predict = c("Prediction", "Task")),
        output = data.table(name = "output", train = "NULL", predict = "Prediction")
      )
      private$.param_set = NULL
    },

    train = function(inputs) {
      task = inputs[[2]]
      self$state = self$learner$train(task)

      list(NULL)
    },

    predict = function(inputs) {
      prediction = inputs[[1]]

      badrows = is.na(prediction$response)

      if (any(badrows)) {
        prediction = prediction$clone(deep = TRUE)

        badrowids = prediction$row_ids[badrows]
        task = inputs[[2]]$clone(deep = TRUE)$filter(badrowids)

        newprediction = self$state$predict(task)
        for (repairing in c("response", "prob", "se")) {
          if (repairing %in% names(prediction) && !is.null(prediction[[repairing]]) &&
            repairing %in% names(newprediction) && !is.null(newprediction[[repairing]])) {
            if (is.matrix(prediction[[repairing]]) || is.data.frame(prediction[[repairing]])) {
              prediction[[repairing]][badrows, ] = newprediction[[repairing]]
            } else {
              prediction[[repairing]][badrows] = newprediction[[repairing]]
            }
          }
        }
      }
      list(prediction)
    }),
  active = list(
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, self$learner$param_set)) {
        stop("param_set is read-only.")
      }
      self$learner$param_set
    },
    id = function(val) {
      if (!missing(val)) {
        private$.id = val
        self$learner$param_set$set_id = val
      }
      private$.id
    })
)
