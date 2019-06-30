#' @title PipeOpTaskPreproc
#'
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Base class for handling most "preprocessing" operations. These
#' are operations that have exactly one Task input and one Task output,
#' and expect the column layout of these tasks during input and output
#' to be the same.
#'
#' Users must implement `$train_task()` and `$predict_task()`, which have a [`Task`]
#' input and should return that `Task`. The `Task` should, if possible, be
#' manipulated in-place, and should not be cloned.
#'
#' If `can_subset_cols` is `TRUE`, then the hyperparameter `affect_columns`
#' is added, which is a function that takes the Task as input and returns
#' a `character` as output, indicating what columns to choose.
#'
#' @section Public Members / Active Bindings:
#' * `state$intasklayout`  :: [`data.table`] with columns `id`, `type` \cr
#'   Set during `$train()`: `$feature_types` of training `Task`, is used for verification
#'   during `$predict()` that columns have not changed.
#' * `state$outtasklayout` :: [`data.table`] with columns `id`, `type` \cr
#'   Set during `$train()`: `$feature_types` of transformed `Task`, is used for verification
#'   during `$predict()` that the transformed `Task` is the same as during training.
#' * `affect_columns` :: `NULL` | `function` \cr
#'   Can be used to subset the columns that the `PipeOp` operates on. If `$affect_columns` is `NULL`,
#'   then all columns are given to the `$train` / `$predict` function. If this is a `function`, it
#'   should take one argument (the input `Task`) and return a `character` indicating the feature
#'   columns that should be operated on. `$affect_columns()` is then called during training and
#'   only features named in its return value are seen by the training / prediction function. \cr
#'   This should not be changed if `can_subset_columns` is `FALSE`, and depends on the `PipeOpTaskPreproc` subclass.
#' * `state$affected_cols` :: `character` \cr
#'   Set during `$train()`: Indicating the names of features to operate on, if  parameter
#'   `$affect_columns` is set.
#' * `can_subset_columns` :: `logical(1)` \cr
#'   Whether `affect_columns` can be set to something other than `NULL`. Read-only, value depends on
#'   the `PipeOpTaskPreproc` subclass.
#'
#' @section Methods:
#' * `PipeOpTaskPreproc$new(id, param_set = ParamSet$new(), can_subset_cols = TRUE)` \cr
#'   (`character(1)`, `ParamSet`, `logical(1)`) -> `self` \cr
#'   Constructor. The `can_subset_cols` argument should be set by the inheriting class to `FALSE` if
#'   affecting only a subset of columns chosen by the user should not be possible; this switches
#'   `can_subset_cols` to `FALSE`.
#' * `train_task(task)` \cr
#'   ([`Task`]) -> [`Task`] \cr
#'   Train `PipeOpTaskPreproc` on `task`, transform it and store a state in `$state`. The transformed `Task` must
#'   be returned. `task` should not be cloned, instead it should be changed in-place. \cr
#'   This method can be overloaded when inheriting from `PipeOpTaskPreproc`, together with `$predict_task()`;
#'   alternatively, `$train_dt()` and `$predict_dt()` (and possibly `$select_cols()`) can be overloaded.
#' * `predict_task(task)` \cr
#'   ([`Task`]) -> [`Task`] \cr
#'   Predict on new data in `task`, possibly using the stored `$state`. `task` should not be cloned, instead it should be
#'   changed in-place. \cr
#'   This method can be overloaded when inheriting from `PipeOpTaskPreproc`, together with `$train_task()`;
#'   alternatively, `$train_dt()` and `$predict_dt()` (and possibly `$select_cols()`) can be overloaded.
#' * `train_dt(dt)` \cr
#'   ([`data.table`]) -> `any` \cr
#'   Train `PipeOpTaskPreproc` on `dt`, transform it and store a state in `$state`. A transformed object must be returned
#'   that can be converted to a `data.table` using [`as.data.table`]. `dt` does not need to be copied deliberately, it
#'   is possible and encouraged to change it in-place. \cr
#'   This method can be overloaded when inheriting `PipeOpTaskPreproc`, together with `$predict_dt()` and optionally
#'   `$select_cols()`; alternatively, `$train_task()` and `$predict_task()` can be overloaded.
#' * `predict_dt(dt)` \cr
#'   ([`data.table`]) -> `any` \cr
#'   Predict on new data in `dt`, possibly using the stored `$state`. A transformed object must be returned
#'   that can be converted to a `data.table` using [`as.data.table`]. `dt` does not need to be copied deliberately, it
#'   is possible and encouraged to change it in-place. \cr
#'   This method can be overloaded when inheriting `PipeOpTaskPreproc`, together with `$train_dt()` and optionally
#'   `$select_cols()`; alternatively, `$train_task()` and `$predict_task()` can be overloaded.
#' * `select_cols(dt)` \cr
#'   ([`data.table`]) -> `character` \cr
#'   Selects which columns the `PipeOp` operates on, if `$train_dt()` and `$predict_dt()` are overloaded. This function
#'   is not called if `$train_task()` and `$predict_task()` are overloaded. It is furthermore not supposed to be changed
#'   by the user, who should use `affect_columns`. `select_cols` is for the *ineriting class* to determine which columns
#'   the operator should function on, e.g. based on feature type. \cr
#'   This method can optionally be overloaded when inheriting `PipeOpTaskPreproc`, together with `$train_dt()` and
#'   `$predict_dt()`; alternatively, `$train_task()` and `$predict_task()` can be overloaded. \cr
#'   If this method is not overloaded, it defaults to selecting all columns.
#' @family mlr3pipelines backend related
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpTaskPreproc = R6Class("PipeOpTaskPreproc",

  inherit = PipeOp,

  public = list(
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), can_subset_cols = TRUE, packages = character(0)) {
      if (can_subset_cols) {
        acp = ParamUty$new("affect_columns", custom_check = check_function_or_null)
        if ("ParamSetCollection" %in% class(param_set)) {
          param_set$add(ParamSet$new(list(acp)))
        } else {
          param_set$add(acp)
        }
      }
      super$initialize(id = id, param_set = param_set, param_vals = param_vals,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task"),
        packages = packages
      )
    },

    train = function(inputs) {

      intask = inputs[[1]]$clone(deep = TRUE)
      do_subset = !is.null(self$param_set$values$affect_columns)
      affected_cols = intask$feature_names
      if (do_subset) {
        affected_cols = self$param_set$values$affect_columns(intask)
        # FIXME: this fails when something is both a feature and something else
        remove_cols = setdiff(intask$feature_names, affected_cols)
        intask$set_col_role(remove_cols, character(0))
      }
      intasklayout = copy(intask$feature_types)

      intask = self$train_task(intask)

      self$state$affected_cols = affected_cols
      self$state$intasklayout = intasklayout
      self$state$outtasklayout = copy(intask$feature_types)

      if (do_subset) {
        # FIXME: this fails if train_task added a column with the same name
        intask$set_col_role(remove_cols, "feature")
      }
      list(intask)
    },

    predict = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      do_subset = !is.null(self$param_set$values$affect_columns)
      if (do_subset) {
        # FIXME: see train fixme: this fails when something is both a feature and something else
        remove_cols = setdiff(intask$feature_names, self$state$affected_cols)
        intask$set_col_role(remove_cols, character(0))
      }
      if (!isTRUE(all.equal(self$state$intasklayout, intask$feature_types, ignore.row.order = TRUE))) {
        stopf("Input task during prediction of %s does not match input task during training.", self$id)
      }
      intask = self$predict_task(intask)

      if (!isTRUE(all.equal(self$state$outtasklayout, intask$feature_types, ignore.row.order = TRUE))) {
        stopf("Processed output task during prediction of %s does not match output task during training.", self$id)
      }
      if (do_subset) {
        # FIXME: see train fixme: this fails if train_task added a column with the same name
        intask$set_col_role(remove_cols, "feature")
      }
      list(intask)
    },

    train_task = function(task) {
      dt_columns = self$select_cols(task)
      cols = dt_columns
      if (!length(cols)) {
        self$state = list(dt_columns = dt_columns)
        return(task)
      }
      dt = task$data(cols = cols)
      dt = as.data.table(self$train_dt(dt, task_levels(task, cols)))
      self$state$dt_columns = dt_columns
      task$select(setdiff(task$feature_names, cols))$cbind(dt)
    },

    predict_task = function(task) {
      cols = self$state$dt_columns
      if (!length(cols)) {
        return(task)
      }
      dt = task$data(cols = cols)
      dt = as.data.table(self$predict_dt(dt, task_levels(task, cols)))
      task$select(setdiff(task$feature_names, cols))$cbind(dt)
    },

    train_dt = function(dt, levels) stop("Abstract."),

    predict_dt = function(dt, levels) stop("Abstract."),

    select_cols = function(task) task$feature_names
  )
)

#' @title PipeOpTaskPreprocSimple
#'
#' @format Abstract [`R6Class`] inheriting from [`PipeOpTaskPreproc`].
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
#' This inherits from [`PipeOpTaskPreproc`] and behaves essentially the same.
#'
#' @section Methods:
#' * `get_state(task)` \cr
#'   ([`Task`]) -> `any` \cr
#'   Store create something that will be stored in `$state` during training phase of `PipeOpTaskPreprocSimple`.
#'   The state can then influence the `$transform()` function. Note that `$get_state()` must *return* the state, and
#'   should not store it in `$state`. If neither `$get_state()` nor `$get_state_dt()` are overloaded, the state will
#'   be stored as `list()`. \cr
#'   This method can optionally be overloaded when inheriting from `PipeOpTaskPreprocSimple`, together with `$transform()`;
#'   alternatively, `$get_state_dt()` (optional) and `$transform_dt()` (and possibly `$select_cols()`, from [`PipeOpTaskPreproc`])
#'   can be overloaded.
#' * `transform(task)` \cr
#'   ([`Task`]) -> [`Task`] \cr
#'   Predict on new data in `task`, possibly using the stored `$state`. `task` should not be cloned, instead it should be
#'   changed in-place. This method is called both during training and prediction phase, and should essentially behave the
#'   same independently of phase. (If this is incongruent with the design of the `PipeOp`, then it should inherit from
#'   `PipeOpTaskPreproc`, not from `PipeOpTaskPreprocSimple`.) \cr
#'   This method can be overloaded when inheriting from `PipeOpTaskPreprocSimple`, optionally with `$get_state()`;
#'   alternatively, `$get_state_dt()` (optional) and `$transform_dt()` (and possibly `$select_cols()`, from [`PipeOpTaskPreproc`])
#'   can be overloaded.
#' * `get_state_dt(dt)` \cr
#'   Store create something that will be stored in `$state` during training phase of `PipeOpTaskPreprocSimple`.
#'   The state can then influence the `$transform_dt()` function. Note that `$get_state_dt()` must *return* the state, and
#'   should not store it in `$state`. If neither `$get_state()` nor `$get_state_dt()` are overloaded, the state will
#'   be stored as `list()`. \cr
#'   This method can optionally be overloaded when inheriting from `PipeOpTaskPreprocSimple`, together with `$transform_dt()`
#'   (and optionally `$select_cols()`, from [`PipeOpTaskPreproc`]); Alternatively, `$get_state()` (optional) and `$transform()`
#'   can be overloaded.
#' * `transform_dt(dt)` \cr
#'   ([`data.table`]) -> `any` \cr
#'   Predict on new data in `dt`, possibly using the stored `$state`.  A transformed object must be returned
#'   that can be converted to a `data.table` using [`as.data.table`]. `dt` does not need to be copied deliberately, it
#'   is possible and encouraged to change it in-place. This method is called both during training and prediction phase,
#'   and should essentially behave the same independently of phase. (If this is incongruent with the design of the
#'   `PipeOp`, then it should inherit from `PipeOpTaskPreproc`, not from `PipeOpTaskPreprocSimple`.) \cr
#'   This method can optionally be overloaded when inheriting from `PipeOpTaskPreprocSimple`, together with `$transform_dt()`
#'   (and optionally `$select_cols()`, from [`PipeOpTaskPreproc`]); Alternatively, `$get_state()` (optional) and `$transform()`
#'   can be overloaded.
#' @family PipeOp
#' @family mlr3pipelines backend related
#' @export
PipeOpTaskPreprocSimple = R6Class("PipeOpTaskPreprocSimple",

  inherit = PipeOpTaskPreproc,

  public = list(
    train_task = function(task) {
      self$state = self$get_state(task)
      self$transform(task)
    },
    predict_task = function(task) self$transform(task),

    get_state = function(task) {
      private$.dt_columns = self$select_cols(task)
      cols = private$.dt_columns
      if (!length(cols)) {
        return(list())
      }
      dt = task$data(cols = cols)
      self$get_state_dt(dt, task_levels(task, cols))
    },

    transform = function(task) {
      cols = private$.dt_columns
      if (!length(cols)) {
        return(task)
      }
      dt = task$data(cols = cols)
      dt = as.data.table(self$transform_dt(dt, task_levels(task, cols)))
      task$select(setdiff(task$feature_names, cols))$cbind(dt)
    },

    get_state_dt = function(dt, levels) list(),

    transform_dt = function(dt, levels) stop("Abstract")
  )
)

# needs to be here because all.equal fails otherwise.
check_function_or_null = function(x) assert_function(x, null.ok = TRUE)
