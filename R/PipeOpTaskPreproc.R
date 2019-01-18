#' @title PipeOpTaskPreproc
#'
#' @name PipeOpTaskPreproc
#' @format [R6Class] PipeOpTaskPreproc
#'
#' @description
#' Base class for handling most "preprocessing" operations. These
#' are operations that have exactly one Task input and one Task output,
#' and expect the column layout of these tasks during input and output
#' to be the same.
#'
#' Users must implement train_task and predict_task, which have a Task
#' input and should manipulate that task in-place; return values are
#' ignored.
#'
#' If `can_subset` is `TRUE`, then the hyperparameter `affect_columns`
#' is added, which is a function that takes the Task as input and returns
#' a logical as output, indicating what columns to choose.
#'
#' @family PipeOp
NULL

#' @include PipeOp.R
#' @export
PipeOpTaskPreproc = R6Class("PipeOpTaskPreproc",

  inherit = PipeOp,

  public = list(
    intasklayout = NULL,
    outtasklayout = NULL,
    affect_cols = NULL,
    can_subset = NULL,

    initialize = function(param_set = ParamSet$new(), can_subset = TRUE) {
      self$can_subset = can_subset
      if (can_subset) {
        ac_par = R6Class("ParamFctOneArg", inherit = ParamUty,
          private = list(.check = function(x) test_function(x, nargs = 1))
        )$new("affect_columns", special_vals = list(NULL))

        param_set$add(ac_par)
      }
      super$initialize(param_set = param_set,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    },

    train = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      if (self$can_subset && !is.null(self$par_vals$affect_columns)) {
        self$affect_cols = intask$feature_names[self$par_vals$affect_columns(intask)]
        # FIXME: this fails when something is both a feature and something else
        remove_cols = setdiff(intask$feature_names, self$affect_cols)
        intask$set_col_role(remove_cols, character(0))
      }
      self$intasklayout = intask$feature_types
      self$train_task(intask)
      self$outtasklayout = intask$feature_types
      if (self$can_subset) {
        # FIXME: this fails if train_task added a column with the same name
        intask$set_col_role(remove_cols, "feature")
      }
      list(intask)
    },

    predict = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      if (self$can_subset) {
        # FIXME: see train fixme: this fails when something is both a feature and something else
        remove_cols = setdiff(intask$feature_names, self$affect_cols)
        intask$set_col_role(remove_cols, character(0))
      }
      if (!all.equal(self$intasklayout, intask$feature_types)) {
        stopf("Input task during prediction of %s does not match input task during training.", self$id)
      }
      self$predict_task(intask)
      if (!all.equal(self$outtasklayout, intask$feature_types)) {
        stop("Processed output task during prediction of %s does not match output task during training.", self$id)
      }
      if (self$can_subset) {
        # FIXME: see train fixme: this fails if train_task added a column with the same name
        intask$set_col_role(remove_cols, "feature")
      }
      list(intask)
    },

    train_task = function(task) {
      dt = task$data(cols = self$select_cols(task))
      dt = self$train_dt(dt)
      task$select(setdiff(task$feature_names, cols))$cbind(dt)
    },

    predict_task = function(task) {
      dt = task$data(cols = self$select_cols(task))
      dt = self$predict_dt(dt)
      task$select(setdiff(task$feature_names, cols))$cbind(dt)
    },

    train_dt = function(dt) stop("Abstract."),

    predict_dt = function(dt) stop("Abstract."),

    select_cols = function(dt) stop("Abstract.")
  )
)


#' @title PipeOpTaskPreprocSimple
#'
#' @name PipeOpTaskPreprocSimple
#' @format [R6Class] PipeOpTaskPreprocSimple
#'
#' @description
#' Base class for handling many "preprocessing" operations that
#' that perform essentially the same operation during training and prediction.
#' Instead implementing a train_task and a predict_task operation, only
#' a `get_state()` and a `transform()` operation needs to be defined,
#' both of which take one argument: a Task.
#'
#' `get_state` must not change its input value in-place and must return
#' something that will be written into `$state`
#' (which must not be NULL), `transform()` must modify its argument in-state;
#' it is called both during training and prediction.
#'
#' This inherits from [PipeOpTaskPreproc] and behaves essentially the same.
#'
#' @family PipeOp
NULL

#' @include PipeOp.R
#' @export
PipeOpTaskPreprocSimple = R6Class("PipeOpTaskPreprocSimple",

  inherit = PipeOpTaskPreprocSimple,

  public = list(
      train_task = function(task) {
        self$state = self$get_state()
        self$transform(task)
      },
      predict_task = function(task) self$transform(task),

      get_state = function(task)  {
        dt = task$data(cols = self$select_cols(task))
        self$get_state_dt(dt)
      },

      transform = function(task) {
        dt = task$data(cols = self$select_cols(task))
        dt = self$transform_dt(dt)
        task$select(setdiff(task$feature_names, cols))$cbind(dt)
      },

      get_state_dt = function(dt) stop("Abstract"),

      transform_dt= function(dt) stop("Abstract")
  }
)

