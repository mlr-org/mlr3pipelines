#' @title Wrap a Learner into a PipeOp
#'
#' @usage NULL
#' @name mlr_pipeops_learner
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`].
#'
#' @description
#' Wraps an [`mlr3::Learner`] into a [`PipeOp`].
#'
#' Inherits the `$param_set` (and therefore `$param_set$values`) from the [`Learner`][mlr3::Learner] it is constructed from.
#'
#' Using `PipeOpLearner`, it is possible to embed [`mlr3::Learner`]s into [`Graph`]s, which themselves can be
#' turned into Learners using [`GraphLearner`]. This way, preprocessing and ensemble methods can be included
#' into a machine learning pipeline which then can be handled as singular object for resampling, benchmarking
#' and tuning.
#'
#' @section Construction:
#' ```
#' PipeOpLearner$new(learner, id = NULL, param_vals = list())
#' ```
#'
#' * `learner` :: [`Learner`][mlr3::Learner] | `character(1)`\cr
#'   [`Learner`][mlr3::Learner] to wrap, or a string identifying a [`Learner`][mlr3::Learner] in the [`mlr3::mlr_learners`] [`Dictionary`][mlr3misc::Dictionary].
#'  This argument is always cloned; to access the [`Learner`][mlr3::Learner] inside `PipeOpLearner` by-reference, use `$learner`.\cr
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting  object, internally defaulting to the `id` of the [`Learner`][mlr3::Learner] being wrapped.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' `PipeOpLearner` has one input channel named `"input"`, taking a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' `PipeOpLearner` has one output channel named `"output"`, producing `NULL` during training and a [`Prediction`][mlr3::Prediction] subclass
#' during prediction; this subclass is specific to the [`Learner`][mlr3::Learner] type given to `learner` during construction.
#'
#' The output during prediction is the [`Prediction`][mlr3::Prediction] on the prediction input data, produced by the [`Learner`][mlr3::Learner]
#' trained on the training input data.
#'
#' @section State:
#' The `$state` is set to the `$state` slot of the [`Learner`][mlr3::Learner] object. It is a named `list` with members:
#' * `model` :: `any`\cr
#'   Model created by the [`Learner`][mlr3::Learner]'s `$.train()` function.
#' * `train_log` :: [`data.table`][data.table::data.table] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during training.
#' * `train_time` :: `numeric(1)`\cr
#'   Training time, in seconds.
#' * `predict_log` :: `NULL` | [`data.table`][data.table::data.table] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during prediction.
#' * `predict_time` :: `NULL` | `numeric(1)`
#'   Prediction time, in seconds.
#'
#' @section Parameters:
#' The parameters are exactly the parameters of the [`Learner`][mlr3::Learner] wrapped by this object.
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
#' * `validate` :: `"predefined"` or `NULL`\cr
#'    This field can only be set for `Learner`s that have the `"validation"` property.
#'    Setting the field to `"predefined"` means that the wrapped `Learner` will use the internal validation task,
#'    otherwise it will be ignored.
#'    Note that specifying *how* the validation data is created is possible via the `$validate` field of the [`GraphLearner`].
#'    For each `PipeOp` it is then only possible to either use it (`"predefined"`) or not use it (`NULL`).
#'    Also see [`set_validate.GraphLearner`] for more information.
#' * `internal_tuned_values` :: named `list()` or `NULL`\cr
#'    The internally tuned values if the wrapped `Learner` supports internal tuning, `NULL` otherwise.
#' * `internal_valid_scores` :: named `list()` or `NULL`\cr
#'    The internal validation scores if the wrapped `Learner` supports internal validation, `NULL` otherwise.
#'
#' @section Methods:
#' Methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Meta PipeOps
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
#' @examplesIf requireNamespace("rpart")
#' library("mlr3")
#'
#' task = tsk("iris")
#' learner = lrn("classif.rpart", cp = 0.1)
#' lrn_po = mlr_pipeops$get("learner", learner)
#'
#' lrn_po$train(list(task))
#' lrn_po$predict(list(task))
PipeOpLearner = R6Class("PipeOpLearner", inherit = PipeOp,
  public = list(
    initialize = function(learner, id = NULL, param_vals = list()) {
      private$.learner = as_learner(learner, clone = TRUE)
      id = id %??% private$.learner$id
      if (!test_po_validate(get0("validate", private$.learner))) {
        stopf(
          "Validate field of PipeOp '%s' must either be NULL or 'predefined'. We recommend specifying the validation data by calling set_validate(<glrn>, validate = <value>) on a GraphLearner. You can read more about this here: https://mlr3book.mlr-org.com/chapters/chapter15/predsets_valid_inttune.html.",  # nolint
          id
        )
      }
      # FIXME: can be changed when mlr-org/mlr3#470 has an answer
      type = private$.learner$task_type
      task_type = mlr_reflections$task_types[type, mult = "first"]$task
      out_type = mlr_reflections$task_types[type, mult = "first"]$prediction
      properties = c("validation", "internal_tuning")
      properties = properties[properties %in% private$.learner$properties]
      super$initialize(id, param_set = alist(private$.learner$param_set), param_vals = param_vals,
        input = data.table(name = "input", train = task_type, predict = task_type),
        output = data.table(name = "output", train = "NULL", predict = out_type),
        tags = "learner", packages = learner$packages, properties = properties
      )
    }
  ),
  active = list(
    internal_tuned_values = function(rhs) {
      assert_ro_binding(rhs)
      if ("internal_tuning" %nin% self$properties) return(NULL)
      self$learner_model$internal_tuned_values
    },
    internal_valid_scores = function(rhs) {
      assert_ro_binding(rhs)
      if ("validation" %nin% self$properties) return(NULL)
      self$learner_model$internal_valid_scores
    },
    validate = function(rhs) {
      if ("validation" %nin% self$properties) {
        if (!missing(rhs)) {
          stopf("PipeOp '%s' does not support validation, because the wrapped Learner doesn't.", self$id)
        }
        return(NULL)
      }
      if (!missing(rhs)) {
        private$.learner$validate = assert_po_validate(rhs)
      }
      private$.learner$validate
    },
    id = function(val) {
      if (!missing(val)) {
        private$.id = val
        if (paradox_info$is_old) {
          private$.learner$param_set$set_id = val
        }
      }
      private$.id
    },
    learner = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$learner is read-only.")
        }
        validate = get0("validate", private$.learner)
        if (!test_po_validate(validate)) {
          warningf(
            "PipeOpLearner '%s' has its validate field set to a value that is neither NULL nor 'predefined'. This will likely lead to unexpected behaviour. We recommend configuring the validation data by calling set_validate(<glrn>, validate = <value>) on a GraphLearner. You can read more about this here: https://mlr3book.mlr-org.com/chapters/chapter15/predsets_valid_inttune.html", # nolint
            self$id)
        }
      }
      private$.learner
    },
    learner_model = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$learner_model is read-only.")
        }
      }
      if (is.null(self$state) || is_noop(self$state)) {
        private$.learner
      } else {
        multiplicity_recurse(self$state, clone_with_state, learner = private$.learner)
      }
    },
    predict_type = function(val) {
      if (!missing(val)) {
        assert_subset(val, names(mlr_reflections$learner_predict_types[[private$.learner$task_type]]))
        private$.learner$predict_type = val
      }
      private$.learner$predict_type
    }
  ),
  private = list(
    .learner = NULL,

    .train = function(inputs) {
      on.exit({private$.learner$state = NULL})
      task = inputs[[1L]]
      self$state = private$.learner$train(task)$state

      list(NULL)
    },

    .predict = function(inputs) {
      on.exit({private$.learner$state = NULL})
      task = inputs[[1]]
      private$.learner$state = self$state
      list(private$.learner$predict(task))
    },
    .additional_phash_input = function() private$.learner$phash
  )
)

mlr_pipeops$add("learner", PipeOpLearner, list(R6Class("Learner", public = list(properties = character(0), id = "learner", task_type = "classif", param_set = ps(), packages = "mlr3pipelines"))$new())) # nolint

# Q: Why do we have two different paths for setting $validate, the set_validate() and the `$validate` AB?
# A: `$validate` is lower level machinery. `set_validate()` has optional arguments, e.g. `ids = ...`.
#    `set_validate.PipeOpLearner` might be called by `set_validate.GraphLearner` and allows passing down
#    further arguments in case the pipeoplearner was another graph learner e.g.
#' @export
set_validate.PipeOpLearner = function(learner, validate, ...) {
  assert_po_validate(validate)
  on.exit({
    # don't use set_validate(), as it also does not work in general and might fail again
    learner$validate = prev_validate
  })
  prev_validate = learner$validate
  withCallingHandlers({
    set_validate(learner$learner, validate = validate, ...)
  }, error = function(e) {
    e$message = sprintf(paste0(
      "Failed to set validate for Learner '%s':\n%s\n",
      "Trying to heuristically reset validation to its previous state, please check the results"), learner$id, e$message)
    stop(e)
  }, warning = function(w) {
    w$message = sprintf(paste0(
      "Failed to set validate for PipeOp '%s':\n%s\n",
      "Trying to heuristically reset validation to its previous state, please check the results"), learner$id, w$message)
    warning(w)
    invokeRestart("muffleWarning")
  })
  on.exit()
  invisible(learner)
}
