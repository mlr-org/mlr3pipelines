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
#' input and should return that task. The task should, if possible, be
#' manipulated in-place, and should not be cloned.
#'
#' If `can_subset_cols` is `TRUE`, then the hyperparameter `affect_columns`
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
    affected_cols = NULL,

    initialize = function(id, param_set = ParamSet$new(), can_subset_cols= TRUE) {
      private$.can_subset_cols = can_subset_cols
      private$.affect_columns = NULL
      super$initialize(id = id, param_set = param_set,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    },

    train = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      do_subset = !is.null(self$affect_columns)
      if (do_subset) {
        self$affected_cols = intask$feature_names[self$affect_columns(intask)]
        # FIXME: this fails when something is both a feature and something else
        remove_cols = setdiff(intask$feature_names, self$affected_cols)
        intask$set_col_role(remove_cols, character(0))
      }
      self$intasklayout = intask$feature_types
      intask = self$train_task(intask)
      self$outtasklayout = intask$feature_types
      if (do_subset) {
        # FIXME: this fails if train_task added a column with the same name
        intask$set_col_role(remove_cols, "feature")
      }
      list(intask)
    },

    predict = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      do_subset = !is.null(self$affect_columns)
      if (do_subset) {
        # FIXME: see train fixme: this fails when something is both a feature and something else
        remove_cols = setdiff(intask$feature_names, self$affected_cols)
        intask$set_col_role(remove_cols, character(0))
      }
      if (!isTRUE(all.equal(self$intasklayout, intask$feature_types))) {
        stopf("Input task during prediction of %s does not match input task during training.", self$id)
      }
      intask = self$predict_task(intask)
      if (!isTRUE(all.equal(self$outtasklayout, intask$feature_types))) {
        stop("Processed output task during prediction of %s does not match output task during training.", self$id)
      }
      if (do_subset) {
        # FIXME: see train fixme: this fails if train_task added a column with the same name
        intask$set_col_role(remove_cols, "feature")
      }
      list(intask)
    },

    train_task = function(task) {
      private$.dt_columns = self$select_cols(task)
      cols = private$.dt_columns
      if (!length(cols)) {
        self$state = list()
        return(NULL)
      }
      dt = task$data(cols = cols)
      dt = as.data.table(self$train_dt(dt))
      task$select(setdiff(task$feature_names, cols))$cbind(dt)
    },

    predict_task = function(task) {
      cols = private$.dt_columns
      if (!length(cols)) {
        return(NULL)
      }
      dt = task$data(cols = cols)
      dt = as.data.table(self$predict_dt(dt))
      task$select(setdiff(task$feature_names, cols))$cbind(dt)
    },

    train_dt = function(dt) stop("Abstract."),

    predict_dt = function(dt) stop("Abstract."),

    select_cols = function(task) task$feature_names
  ),
  active = list(
    can_subset_cols = function() private$.can_subset_cols,
    affect_columns = function(val) {
      if (!missing(val)) {
        assert_true(self$can_subset_cols)
        assert_function(val, nargs = 1, null.ok = TRUE)
        private$.affect_columns = val
      }
      private$.affect_columns
    }
  ),
  private = list(
    .can_subset_cols = NULL,
    .affect_columns = NULL,
    .dt_columns = NULL
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

  inherit = PipeOpTaskPreproc,

  public = list(
      train_task = function(task) {
        self$state = self$get_state()
        self$transform(task)
      },
      predict_task = function(task) self$transform(task),

      get_state = function(task)  {
        private$.dt_columns = self$select_cols(task)
        cols = private$.dt_columns
        if (!length(cols)) {
          return(list())
        }
        dt = task$data(cols = cols)
        self$get_state_dt(dt)
      },

      transform = function(task) {
        cols = private$.dt_columns
        dt = task$data()
        dt = self$transform_dt(dt)
        task$select(setdiff(task$feature_names, cols))$cbind(dt)
      },

      get_state_dt = function(dt) stop("Abstract"),

      transform_dt = function(dt) stop("Abstract")
  )
)

