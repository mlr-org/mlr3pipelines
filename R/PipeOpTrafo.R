#' @title PipeOpTargetTrafo
#'
#' @usage NULL
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Base class for handling target transformation operations that have to be inverted later. The
#' target is transformed during the training phase and information to invert this transformation can
#' be send along to [`PipeOpTargetInverter`] which then inverts this transformation during the
#' prediction phase. This inversion may need info about both the training and the prediction data.
#'
#' Users should overload four functions:
#'
#' * `set_state()` has a [`Task`][mlr3::Task] input and should return a `list`, which can be used to
#'   set the `$state`. `set_state()` will be called a single time during training right before
#'   `train_target()` will be called. Therefore, the `$state` may contain info needed in
#'   `train_target()`.
#'
#' * `train_target()` has a [`Task`][mlr3::Task] input and should return a modified
#'   [`Task`][mlr3::Task]. This typically consists of calculating a new target and modifying the
#'   task by using `private$.update_target()`. `train_target()` will be called during training and
#'   prediction because the target (and if needed also type) of the input task must be transformed
#'   both times.
#'
#' * `train_invert()`has a [`Task`][mlr3::Task] input and should return a `predict_phase_control`
#'   object (can be anything the user needs for the inversion later). This should not modify the input
#'   task.
#'
#' * `inverter()` has a [`Prediction`][mlr3::Prediction] input as well as one for a
#'   `predict_phase_control` object and should return a function that can later be used to invert the
#'   transformation done by `train_target()` and return a [`Prediction`][mlr3::Prediction] object.
#'
#' @section Construction:
#' ```
#' PipeOpTargetTrafo$new(id, param_set = ParamSet$new(),
#'   param_vals = list(), packages = character(0))
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. See `$id` slot of [`PipeOp`].
#' * `param_set` :: [`ParamSet`][paradox::ParamSet]\cr
#'   Parameter space description. This should be created by the subclass and given to
#'   `super$initialize()`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings given in `param_set`.
#'   The subclass should have its own `param_vals` parameter and pass it on to `super$initialize()`.
#'   Default `list()`.
#' * packages :: `character`\cr
#'   Set of all required packages for the [`PipeOp`]'s methods. See `$packages` slot. Default is
#'   `character(0)`.
#'
#' @section Input and Output Channels:
#' [`PipeOpTargetTrafo`] has one input channels named `"input"` taking a [`Task`][mlr3::Task] both
#' during training and prediction.
#'
#' [`PipeOpTargetTrafo`] has two output channels named `"fun"` and `"output"`. During training,
#' `"fun"` returns `NULL` and during prediction, `"fun"` returns a function that can later be used
#' to invert the transformation done during training according to the overloaded `train_invert()`
#' and `inverter()` functions. `"output"` returns the modified input [`Task`][mlr3::Task] according
#' to the overloaded `train_target()` function both during training and prediction.
#'
#' @section State:
#' The `$state` is a named `list` and should be returned explicitly by the user in the overloaded
#' `set_state()` function.
#'
#' @section Internals:
#' [`PipeOpTargetTrafo`] is an abstract class inheriting from [`PipeOp`]. It implements the
#' `private$.train()` and `private$.predict()` functions. These functions perform checks and go on
#' to call `set_state()`, `train_target()`, `train_invert()` and `inverter()`. A subclass of
#' [`PipeOpTargetTrafo`] should implement these functions and be used in combination with
#' [`PipeOpTargetInverter`].
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [`PipeOp`], as well as:
#' * `set_state(task)`\cr
#'   ([`Task`][mlr3::Task]) -> `list`\cr
#'   Called by [`PipeOpTargetTrafo`]'s implementation of `private$.train()`. Takes a single
#'   [`Task`][mlr3::Task] as input and returns a `list` to set the `$state`. Can be `list()`.
#' * `train_target(task)`\cr
#'   ([`Task`][mlr3::Task]) -> [`Task`][mlr3::Task]\cr
#'   Called by [`PipeOpTargetTrafo`]'s implementation of `private$.train()` and
#'   `private$.predict()`. Takes a single [`Task`][mlr3::Task] as input and modifies it. Note that
#'   unlike `$.train()`, the argument is *not* a list but a singular [`Task`][mlr3::Task], and the
#'   return object is also *not* a list but a singular [`Task`][mlr3::Task].
#' * `train_invert(task)`\cr
#'   ([`Task`][mlr3::Task]) -> `predict_phase_control` object\cr
#'   Called by [`PipeOpTargetTrafo`]'s implementation of `private$.predict()`. Takes a single
#'   [`Task`][mlr3::Task] as input and returns a `predict_phase_control` object (can be anything the
#'   user needs for the inversion later). This should not modify the input task.
#' * `inverter(prediction, predict_phase_control)`\cr
#'   ([`Prediction`][mlr3::Prediction], `predict_phase_control` object) -> `function`\cr
#'   Called by `private$.invert_help()` within [`PipeOpTargetTrafo`]'s implementation of
#'   `private$.predict()`. Takes a [`Prediction`][mlr3::Prediction] and a `predict_phase_control`
#'   object as input and returns a function that can later be used for the inversion.
#' * `.update_target(task, new_target, new_type = NULL, ...)`\cr
#'   ([`Task`][mlr3::Task], new_target, new_type, ...) -> [`Task`][mlr3::Task]\cr
#'   Typically called within `train_target()`. Updates the target of a task and also the task_type
#'   (if needed). Internally uses `convert_task()` and drops the original target from the task.
#' * `.invert_help(predict_phase_control)`\cr
#'   (`predict_phase_control` object) -> `function`\cr
#'   Helper function that returns a function that can later be used for the inversion.
#'
#' @family mlr3pipelines backend related
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpTargetTrafo = R6Class("PipeOpTargetTrafo",
  inherit = PipeOp,
  public = list(
    # FIXME: maybe this should get a `task` argument so you can define subclasses that only work for subtasks etc.
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), packages = character(0)) {
      super$initialize(id = id, param_set = param_set, param_vals = param_vals,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = c("fun", "output"), train = c("NULL", "Task"), predict = c("function", "Task")),
        packages = packages
      )
    },

    set_state = function(task) {
      # set the state, can be list(NULL)
      # this will internally be called a single time during .train prior to train_target
      stop("Abstract.")
    },

    train_target = function(task) {
      # return a modified task, make use of private$.update_target
      stop("Abstract.")
    },

    train_invert = function(task) {
      # return a predict_phase_control object (can be anything)
      stop("Abstract.")
    },

    inverter = function(prediction, predict_phase_control) {
      # function that inverts the predictions and returns a Prediction object
      stop("Abstract.")
    }
  ),
  private = list(
    .train = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      self$state = self$set_state(intask)
      intask = self$train_target(intask)
      list(NULL, intask)
    },

    .predict = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      intask = self$train_target(intask)
      predict_phase_control = self$train_invert(inputs[[1L]])
      list(
        fun = private$.invert_help(predict_phase_control),
        task = intask
      )
    },

    # updates the target of a task and also the task_type (if needed), uses convert_task
    .update_target = function(task, new_target, new_type = NULL, ...) {
      convert_task(task, new_target = new_target, new_type = new_type, drop_original_target = TRUE, ...)
    },

    .invert_help = function(predict_phase_control) {
      # provides a helper function for the inversion
      # PipeOpTargetInverter will later apply this function during prediction
      function(inputs) {
        assert_list(inputs, len = 1L, types = "Prediction")
        self$inverter(inputs[[1L]], predict_phase_control)
      }
    }
  )
)



#' @title PipeOpTargetInverter
#'
#' @usage NULL
#' @name mlr_pipeops_targetinverter
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' During prediction inverts transformations done during training based on a supplied inversion
#' function. Typically should be used in combination with a subclass of [`PipeOpTargetTrafo`].
#'
#' @section Construction:
#' ```
#' PipeOpTargetInverter$new(id = "targetinverter")
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"targetinverter"`.
#'
#' @section Input and Output Channels:
#' [`PipeOpTargetInverter`] has two input channels named `"fun"` and `"prediction"`. During
#' training, both take `NULL` as input. During prediction, `"fun"` takes a function and
#' `"prediction"` takes a [`Prediction`][mlr3::Prediction].
#'
#' [`PipeOpTargetInverter`] has one output channel named `"output"` and returns `NULL` during
#' training and a [`Prediction`][mlr3::Prediction] during prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' [`PipeOpTargetInverter`] has no parameters.
#'
#' @section Internals:
#' Should be used in combination with a subclass of [`PipeOpTargetTrafo`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpTargetInverter = R6Class("PipeOpTargetInverter",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "targetinverter") {
      super$initialize(id = id,
        input = data.table(name = c("fun", "prediction"), train = c("NULL", "NULL"), predict = c("function", "Prediction")),
        output = data.table(name = "output", train = "NULL", predict = "Prediction")
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      list(NULL)
    },

    .predict = function(inputs) {
      list(inputs[[1L]](inputs[-1L]))
    }
  )
)

mlr_pipeops$add("targetinverter", PipeOpTargetInverter)



#' @title PipeOpTargetTrafoSimple
#'
#' @usage NULL
#' @name mlr_pipeops_targettrafosimple
#' @format [`R6Class`] object inheriting from [`PipeOpTargetTrafo`]/[`PipeOp`]
#'
#' @description
#' Allows for target transformation operations of [`Task`][mlr3::Task] that have to be inverted
#' later, where the transformation function is simply given by a function of the target. Typically
#' this will be applied to a [`TaskRegr`][mlr3::TaskRegr].
#'
#' @section Construction:
#' ```
#' PipeOpTargetTrafoSimple$new(id = "targettrafosimple",
#'   param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"targettrafosimple"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTargetTrafo`].
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTargetTrafo`], as well as:
#' * `trafo` :: `function`\cr
#'   Transformation function for the target. Should only be a function of the target, i.e., taking a
#'   single argument. Default is `identity`.
#' * `inverter` :: `function`\cr
#'   Inversion of the transformation function for the target. Should only be a function of the
#'   target, i.e., taking a single argument. Default is `identity`.
#' * `new_target_name` :: `character(1)`\cr
#'   Optionally give the transformed target a new name. By default the original name is used.
#' * `new_task_type` :: `character(1)`\cr
#'   Optionally a new task type can be set. Legal types are listed in
#'   `mlr_reflections$task_types$type`.
#'
#' @section Internals:
#' Overloads [`PipeOpTargetTrafo`]'s `set_state()`, `train_target()`, `train_invert()` and
#' `inverter()`. Should be used in combination with [`PipeOpTargetInverter`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTargetTrafo`]/[`PipeOp`].
#'
#' @examples
#' library(mlr3)
#' task = tsk("boston_housing")
#' po = PipeOpTargetTrafoSimple$new("logtrafo", param_vals = list(
#'   trafo = function(x) log(x, base = 2),
#'   inverter = function(x) 2 ^ x)
#' )
#'
#' po$train(list(task))
#' po$predict(list(task))
#'
#' g = Graph$new()
#' g$add_pipeop(po)
#' g$add_pipeop(LearnerRegrRpart$new())
#' g$add_pipeop(PipeOpTargetInverter$new())
#' g$add_edge(src_id = "logtrafo", dst_id = "targetinverter",
#'   src_channel = 1, dst_channel = 1)
#' g$add_edge(src_id = "logtrafo", dst_id = "regr.rpart",
#'   src_channel = 2, dst_channel = 1)
#' g$add_edge(src_id = "regr.rpart", dst_id = "targetinverter",
#'   src_channel = 1, dst_channel = 2)
#' 
#' g$train(task)
#' g$predict(task)
#' 
#' #syntactic sugar using ppl():
#' tt = ppl("targettrafo", graph = PipeOpLearner$new(LearnerRegrRpart$new()))
#' tt$param_set$values$targettrafosimple.trafo = function(x) log(x, base = 2)
#' tt$param_set$values$targettrafosimple.inverter = function(x) 2 ^ x
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpTargetTrafoSimple = R6Class("PipeOpTargetTrafoSimple",
  inherit = PipeOpTargetTrafo,
  public = list(
    initialize = function(id = "targettrafosimple", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("trafo", default = identity, tags = c("train", "predict"), custom_check = function(x) check_function(x, nargs = 1L)),
        ParamUty$new("inverter", default = identity, tags = "predict", custom_check = function(x) check_function(x, nargs = 1L)),
        ParamUty$new("new_target_name", tags = c("train", "predict"), custom_check = function(x) check_character(x, any.missing = FALSE, len = 1L)),
        ParamUty$new("new_task_type", tags = c("train", "predict"), custom_check = function(x) check_choice(x, choices = mlr_reflections$task_types$type))
        )
      )
      ps$values = list(trafo = identity, inverter = identity)
      super$initialize(id = id, param_set = ps, param_vals = param_vals)
    },

    set_state = function(task) {
      list()
    },

    train_target = function(task) {
      new_target = self$param_set$values$trafo(task$data(cols = task$target_names))
      if (!is.null(self$param_set$values$new_target_name)) {
        colnames(new_target) = self$param_set$values$new_target_name
      }
      task$cbind(new_target)
      private$.update_target(task, new_target = colnames(new_target), new_type = self$param_set$values$new_task_type)
    },

    train_invert = function(task) {
      self$param_set$values$inverter
    },

    inverter = function(prediction, predict_phase_control) {
      # FIXME: probably should only work for predict_type = "response" and needs a check here,
      # could be extended for se/probs using delta method?
      assert_string(prediction$predict_types, pattern = "response")
      type = prediction$task_type
      get(mlr_reflections$task_types[type]$prediction)$new(row_ids = prediction$row_ids,
        truth = predict_phase_control(prediction$truth), response = predict_phase_control(prediction$response))
    }
  )
)

mlr_pipeops$add("targettrafosimple", PipeOpTargetTrafoSimple)



#' @title PipeOpTargetTrafoScaleRange
#'
#' @usage NULL
#' @name mlr_pipeops_targettrafoscalerange
#' @format [`R6Class`] object inheriting from [`PipeOpTargetTrafo`]/[`PipeOp`]
#'
#' @description
#' Linearly transforms a numeric target of a [`TaskRegr`][mlr3::TaskRegr] so it is between `lower`
#' and `upper`. The formula for this is \eqn{x' = a + x * b},
#' where \eqn{b} is \eqn{(upper - lower) / (max(x) - min(x))} and
#' \eqn{a} is \eqn{-min(x) * b + lower}. The same transformation is applied during training and
#' prediction.
#'
#' @section Construction:
#' ```
#' PipeOpTargetTrafoScaleRange$new(id = "targettrafoscalerange",
#'   param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"targettrafoscalerange"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTargetTrafo`].
#'
#' @section State:
#' The `$state` is a named `list` with a vector of the two transformation parameters \eqn{a} and
#' \eqn{b} for the numeric target.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTargetTrafo`], as well as:
#' * `lower`  :: `numeric(1)` \cr
#'   Target value of smallest item of input target. Default is 0.
#' * `upper` :: `numeric(1)` \cr
#'   Target value of greatest item of input target. Default is 1.
#'
#' @section Internals:
#' Overloads [`PipeOpTargetTrafo`]'s `set_state()`, `train_target()`, `train_invert()` and
#' `inverter()`. Should be used in combination with [`PipeOpTargetInverter`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTargetTrafo`]/[`PipeOp`].
#'
#' @examples
#' library(mlr3)
#' task = tsk("boston_housing")
#' po = PipeOpTargetTrafoScaleRange$new()
#' 
#' po$train(list(task))
#' po$predict(list(task))
#' 
#' #syntactic sugar for a graph using ppl():
#' ttscalerange = ppl("targettrafo", trafo_pipeop = PipeOpTargetTrafoScaleRange$new(),
#'   graph = PipeOpLearner$new(LearnerRegrRpart$new()))
#' ttscalerange$train(task)
#' ttscalerange$predict(task)
#' ttscalerange$state$regr.rpart
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpTargetTrafoScaleRange = R6Class("PipeOpTargetTrafoScaleRange",
  inherit = PipeOpTargetTrafo,
  public = list(
    initialize = function(id = "targettrafoscalerange", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("lower", tags = c("required", "train", "predict")),
        ParamDbl$new("upper", tags = c("required", "train", "predict"))
      ))
      ps$values = list(lower = 0, upper = 1)
      super$initialize(id = id, param_set = ps, param_vals = param_vals)
    },

    set_state = function(task) {
      assert_r6(task, classes = "TaskRegr")
      target_name = task$target_names 
      x = task$data(cols = task$target_names)[[1L]]
      rng = range(x, na.rm = TRUE, finite = TRUE)
      b = (self$param_set$values$upper - self$param_set$values$lower) / (rng[2L] - rng[1L])
      a = -rng[1L] * b + self$param_set$values$lower
      list(pars = c(a, b))
    },

    train_target = function(task) {
      assert_r6(task, classes = "TaskRegr")
      x = task$data(cols = task$target_names)[[1L]]
      trafo = self$state[[1L]]
      new_target = as.data.table(trafo[1L] + x * trafo[2L])
      colnames(new_target) = paste0(task$target_names, ".scaled")
      task$cbind(new_target)
      private$.update_target(task, new_target = colnames(new_target))
    },

    train_invert = function(task) {
      self$state[[1L]]
    },

    inverter = function(prediction, predict_phase_control) {
      type = prediction$task_type
      trafo = predict_phase_control
      truth = prediction$truth
      response = prediction$response
      truth = (truth - trafo[1L]) / trafo[2L]
      response = (response - trafo[1L]) / trafo[2L]
      get(mlr_reflections$task_types[type]$prediction)$new(row_ids = prediction$row_ids,
        truth = truth, response = response)
    }
  )
)

mlr_pipeops$add("targettrafoscalerange", PipeOpTargetTrafoScaleRange)
