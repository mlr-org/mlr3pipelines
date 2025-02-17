#' @title Target Transformation Base Class
#'
#' @usage NULL
#' @format Abstract [`R6Class`][R6::R6Class] inheriting from [`PipeOp`].
#'
#' @description
#' Base class for handling target transformation operations. Target transformations are different
#' from feature transformation because they have to be "inverted" after prediction. The
#' target is transformed during the training phase and information to invert this transformation
#' is sent along to [`PipeOpTargetInvert`] which then inverts this transformation during the
#' prediction phase. This inversion may need info about both the training and the prediction data.
#'
#' Users can overload up to four `private$`-functions: `.get_state()` (optional), `.transform()` (mandatory),
#' `.train_invert()` (optional), and `.invert()` (mandatory).
#'
#' @section Construction:
#' ```
#' PipeOpTargetTrafo$new(id, param_set = ps(), param_vals = list() packages = character(0), task_type_in = "Task", task_type_out = task_type_in, tags = NULL)
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
#' * `task_type_in` :: `character(1)`\cr
#'   The class of [`Task`][mlr3::Task] that should be accepted as input. This
#'   should generally be a `character(1)` identifying a type of [`Task`][mlr3::Task], e.g. `"Task"`, `"TaskClassif"` or
#'   `"TaskRegr"` (or another subclass introduced by other packages). Default is `"Task"`.
#' * `task_type_out` :: `character(1)`\cr
#'   The class of [`Task`][mlr3::Task] that is produced as output. This
#'   should generally be a `character(1)` identifying a type of [`Task`][mlr3::Task], e.g. `"Task"`, `"TaskClassif"` or
#'   `"TaskRegr"` (or another subclass introduced by other packages). Default is the value of `task_type_in`.
#' * packages :: `character`\cr
#'   Set of all required packages for the [`PipeOp`]'s methods. See `$packages` slot. Default is
#'   `character(0)`.
#' * tags :: `character` | `NULL`\cr
#'   Tags of the resulting `PipeOp`. This is added to the tag `"target transform"`. Default `NULL`.
#'
#' @section Input and Output Channels:
#' [`PipeOpTargetTrafo`] has one input channels named `"input"` taking a [`Task`][mlr3::Task] (or whatever class
#' was specified by the `task_type` during construction) both during training and prediction.
#'
#' [`PipeOpTargetTrafo`] has two output channels named `"fun"` and `"output"`. During training,
#' `"fun"` returns `NULL` and during prediction, `"fun"` returns a function that can later be used
#' to invert the transformation done during training according to the overloaded `.train_invert()`
#' and `.invert()` functions. `"output"` returns the modified input [`Task`][mlr3::Task] (or `task_type`)
#' according to the overloaded `transform()` function both during training and prediction.
#'
#' @section State:
#' The `$state` is a named `list` and should be returned explicitly by the user in the overloaded
#' `.get_state()` function.
#'
#' @section Internals:
#' [`PipeOpTargetTrafo`] is an abstract class inheriting from [`PipeOp`]. It implements the
#' `private$.train()` and `private$.predict()` functions. These functions perform checks and go on
#' to call `.get_state()`, `.transform()`, `.train_invert()`. `.invert()` is packaged and sent along
#' the `"fun"` output to be applied to a [`Prediction`][mlr3::Prediction] by [`PipeOpTargetInvert`].
#' A subclass of [`PipeOpTargetTrafo`] should implement these functions and be used in combination
#' with [`PipeOpTargetInvert`].
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [`PipeOp`], as well as:
#' * `.get_state(task)`\cr
#'   ([`Task`][mlr3::Task]) -> `list`\cr
#'   Called by [`PipeOpTargetTrafo`]'s implementation of `private$.train()`. Takes a single
#'   [`Task`][mlr3::Task] as input and returns a `list` to set the `$state`.
#'   `.get_state()` will be called a single time during *training* right before
#'   `.transform()` is called. The return value (i.e. the `$state`) should contain info needed in
#'   `.transform()` as well as in `.invert()`.\cr
#'   The base implementation returns `list()` and should be overloaded if setting the state is desired.
#' * `.transform(task, phase)`\cr
#'   ([`Task`][mlr3::Task], `character(1)`) -> [`Task`][mlr3::Task]\cr
#'   Called by [`PipeOpTargetTrafo`]'s implementation of `private$.train()` and
#'   `private$.predict()`. Takes a single [`Task`][mlr3::Task] as input and modifies it.
#'   This should typically consist of calculating a new target and modifying the
#'   [`Task`][mlr3::Task] by using the [`convert_task`][mlr3::convert_task] function. `.transform()` will be called during training and
#'   prediction because the target (and if needed also type) of the input [`Task`][mlr3::Task] must be transformed
#'   both times. Note that unlike `$.train()`, the argument is *not* a list but a singular
#'   [`Task`][mlr3::Task], and the return object is also *not* a list but a singular [`Task`][mlr3::Task].
#'   The `phase` argument is `"train"` during training phase and `"predict"` during prediction phase
#'   and can be used to enable different behaviour during training and prediction. When `phase` is
#'   `"train"`, the `$state` slot (as previously set by `.get_state()`) may also be modified, alternatively
#'   or in addition to overloading `.get_state()`.\cr
#'   The input should *not* be cloned and if possible should be changed in-place.\cr
#'   This function is abstract and should be overloaded by inheriting classes.
#' * `.train_invert(task)`\cr
#'   ([`Task`][mlr3::Task]) -> `any`\cr
#'   Called by [`PipeOpTargetTrafo`]'s implementation of `private$.predict()`. Takes a single
#'   [`Task`][mlr3::Task] as input and returns an arbitrary value that will be given as
#'   `predict_phase_state` to `.invert()`. This should not modify the input [`Task`][mlr3::Task] .\cr
#'   The base implementation returns a list with a single element, the `$truth` column of the [`Task`][mlr3::Task],
#'   and should be overloaded if a more training-phase-dependent state is desired.
#' * `.invert(prediction, predict_phase_state)`\cr
#'   ([`Prediction`][mlr3::Prediction], `any`) -> [`Prediction`][mlr3::Prediction]\cr
#'   Takes a [`Prediction`][mlr3::Prediction] and a `predict_phase_state`
#'   object as input and inverts the prediction. This function is sent as `"fun"` to
#'   [`PipeOpTargetInvert`].\cr
#'   This function is abstract and should be overloaded by inheriting classes. Care should be
#'   taken that the `predict_type` of the [`Prediction`][mlr3::Prediction] being inverted is handled well.
#' * `.invert_help(predict_phase_state)`\cr
#'   (`predict_phase_state` object) -> `function`\cr
#'   Helper function that packages `.invert()` that can later be used for the inversion.
#'
#' @family mlr3pipelines backend related
#' @family PipeOps
#' @family Target Trafo PipeOps
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
PipeOpTargetTrafo = R6Class("PipeOpTargetTrafo",
  inherit = PipeOp,
  public = list(
    initialize = function(id, param_set = ps(), param_vals = list(), packages = character(0), task_type_in = "Task", task_type_out = task_type_in, tags = NULL) {
      super$initialize(id = id, param_set = param_set, param_vals = param_vals,
        input = data.table(name = "input", train = task_type_in, predict = task_type_in),
        output = data.table(name = c("fun", "output"), train = c("NULL", task_type_out), predict = c("function", task_type_out)),
        packages = packages, tags = c(tags, "target transform")
      )
    }
  ),
  private = list(
    .get_state = function(task) {
      # this will internally be called a single time during .train prior to transform.
      # should be overloaded
      list()
    },

    .transform = function(task, phase) {
      # return a modified task, should make use of mlr3::convert_task()
      stop("Abstract.")
    },

    .train_invert = function(task) {
      # return a predict_phase_state object (can be anything)
      list(truth = task$truth())
    },

    .invert = function(prediction, predict_phase_state) {
      # function that inverts the predictions and returns a Prediction object
      stop("Abstract.")
    },

    .train = function(inputs) {
      intask = inputs[[1L]]$clone(deep = TRUE)
      self$state = private$.get_state(intask)
      intask = private$.transform(intask, "train")
      list(NULL, intask)
    },

    .predict = function(inputs) {
      intask = inputs[[1L]]$clone(deep = TRUE)
      predict_phase_state = private$.train_invert(intask)
      intask = private$.transform(intask, "predict")
      list(
        fun = private$.invert_help(predict_phase_state),
        task = intask
      )
    },

    .invert_help = function(predict_phase_state) {
      # provides a helper function for the inversion
      # PipeOpTargetInvert will later apply this function during prediction
      function(inputs) {
        assert_list(inputs, len = 1L, types = "Prediction")
        list(private$.invert(inputs[[1L]], predict_phase_state))
      }
    }
  )
)

#' @title Invert Target Transformations
#'
#' @usage NULL
#' @name mlr_pipeops_targetinvert
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`].
#'
#' @description
#' Inverts target-transformations done during training based on a supplied inversion
#' function. Typically should be used in combination with a subclass of [`PipeOpTargetTrafo`].
#'
#' During prediction phase the function supplied through `"fun"` is called with a `list` containing
#' the `"prediction"` as a single element, and should return a `list` with a single element
#' (a [`Prediction`][mlr3::Prediction]) that is returned by [`PipeOpTargetInvert`].
#'
#' @section Construction:
#' ```
#' PipeOpTargetInvert$new(id = "targetinvert", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"targetinvert"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpTargetInvert`] has two input channels named `"fun"` and `"prediction"`. During
#' training, both take `NULL` as input. During prediction, `"fun"` takes a function and
#' `"prediction"` takes a [`Prediction`][mlr3::Prediction].
#'
#' [`PipeOpTargetInvert`] has one output channel named `"output"` and returns `NULL` during
#' training and a [`Prediction`][mlr3::Prediction] during prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' [`PipeOpTargetInvert`] has no parameters.
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
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
PipeOpTargetInvert = R6Class("PipeOpTargetInvert",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "targetinvert", param_vals = list()) {
      super$initialize(id = id, param_vals = param_vals,
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
      inputs[[1]](inputs[-1])
    }
  )
)

mlr_pipeops$add("targetinvert", PipeOpTargetInvert)

#' @title Transform a Target by a Function
#'
#' @usage NULL
#' @name mlr_pipeops_targetmutate
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTargetTrafo`]/[`PipeOp`]
#'
#' @description
#' Changes the *target* of a [`Task`][mlr3::Task] according to a function given as hyperparameter.
#' An inverter-function that undoes the transformation during prediction must also be given.
#'
#' @section Construction:
#' ```
#' PipeOpTargetMutate$new(id = "targetmutate", param_vals = list(), new_task_type = NULL)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"targetmutate"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#' * `new_task_type` :: `character(1)` | `NULL`\cr
#'   The task type to which the output is converted, must be one of `mlr_reflections$task_types$type`.
#'   Defaults to `NULL`: no change in task type.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTargetTrafo`].
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTargetTrafo`], as well as:
#' * `trafo` :: `function` `data.table` -> `data.table`\cr
#'   Transformation function for the target. Should only be a function of the target, i.e., taking a
#'   single `data.table` argument, typically with one column. The return value is used as the new
#'   target of the resulting [`Task`][mlr3::Task]. To change target names, change the column name of the data
#'   using e.g. [`setnames()`][data.table::setnames].\cr
#'   Note that this function also gets called during prediction and should thus gracefully handle `NA` values.\cr
#'   Initialized to `identity()`.
#' * `inverter` :: `function` `data.table` -> `data.table` | named `list`\cr
#'   Inversion of the transformation function for the target. Called on a `data.table` created from a [`Prediction`][mlr3::Prediction]
#'   using `as.data.table()`, without the `$row_ids` and `$truth` columns,
#'   and should return a `data.table` or named `list` that contains the new relevant slots of a
#'   [`Prediction`][mlr3::Prediction] subclass (e.g., `$response`, `$prob`, `$se`, ...). Initialized to `identity()`.
#'
#' @section Internals:
#' Overloads [`PipeOpTargetTrafo`]'s `.transform()` and
#' `.invert()` functions. Should be used in combination with [`PipeOpTargetInvert`].
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`], as well as:
#' * `new_task_type` :: `character(1)`\cr
#'   `new_task_type` construction argument. Read-only.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTargetTrafo`]/[`PipeOp`].
#'
#' @examplesIf requireNamespace("rpart")
#' library(mlr3)
#' task = tsk("boston_housing")
#' po = PipeOpTargetMutate$new("logtrafo", param_vals = list(
#'   trafo = function(x) log(x, base = 2),
#'   inverter = function(x) list(response = 2 ^ x$response))
#' )
#' # Note that this example is ill-equipped to work with
#' # `predict_type == "se"` predictions.
#'
#' po$train(list(task))
#' po$predict(list(task))
#'
#' g = Graph$new()
#' g$add_pipeop(po)
#' g$add_pipeop(LearnerRegrRpart$new())
#' g$add_pipeop(PipeOpTargetInvert$new())
#' g$add_edge(src_id = "logtrafo", dst_id = "targetinvert",
#'   src_channel = 1, dst_channel = 1)
#' g$add_edge(src_id = "logtrafo", dst_id = "regr.rpart",
#'   src_channel = 2, dst_channel = 1)
#' g$add_edge(src_id = "regr.rpart", dst_id = "targetinvert",
#'   src_channel = 1, dst_channel = 2)
#'
#' g$train(task)
#' g$predict(task)
#'
#' #syntactic sugar using ppl():
#' tt = ppl("targettrafo", graph = PipeOpLearner$new(LearnerRegrRpart$new()))
#' tt$param_set$values$targetmutate.trafo = function(x) log(x, base = 2)
#' tt$param_set$values$targetmutate.inverter = function(x) list(response = 2 ^ x$response)
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
PipeOpTargetMutate = R6Class("PipeOpTargetMutate",
  inherit = PipeOpTargetTrafo,
  public = list(
    initialize = function(id = "targetmutate", param_vals = list(), new_task_type = NULL) {
      private$.new_task_type = assert_choice(new_task_type, mlr_reflections$task_types$type, null.ok = TRUE)
      ps = ps(
        trafo = p_uty(tags = c("train", "predict"), custom_check = crate(function(x) check_function(x, nargs = 1L))),
        inverter = p_uty(tags = "predict", custom_check = crate(function(x) check_function(x, nargs = 1L)))
      )
      # We could add a condition here for new_task_type on trafo and inverter when mlr-org/paradox#278 has an answer.
      # HOWEVER conditions are broken in paradox, it is a terrible idea to use them in PipeOps,
      # see https://github.com/mlr-org/paradox/issues/216 and related comment in PipeOpLearnerCV

      ps$values = list(trafo = identity, inverter = identity)
      super$initialize(id = id, param_set = ps, param_vals = param_vals)
    }
  ),
  active = list(
    new_task_type = function(lhs) {
      if (!missing(lhs)) {
        stop("new_task_type is read-only")
      }
      private$.new_task_type
    }
  ),
  private = list(
    .new_task_type = NULL,

    .transform = function(task, phase) {
      new_target = self$param_set$values$trafo(task$data(cols = task$target_names))
      task$cbind(new_target)
      convert_task(task, target = colnames(new_target), new_type = private$.new_task_type, drop_original_target = TRUE, drop_levels = FALSE)
    },

    .invert = function(prediction, predict_phase_state) {
      type = private$.new_task_type %??% prediction$task_type
      pred = as.data.table(prediction)
      pred$row_ids = NULL
      pred$truth = NULL
      invoke(get(mlr_reflections$task_types[type, mult = "first"]$prediction)$new, row_ids = prediction$row_ids,
        truth = predict_phase_state$truth, .args = self$param_set$values$inverter(pred))
    },
    .additional_phash_input = function() private$.new_task_type
  )
)

mlr_pipeops$add("targetmutate", PipeOpTargetMutate)

#' @title Linearly Transform a Numeric Target to Match Given Boundaries
#'
#' @usage NULL
#' @name mlr_pipeops_targettrafoscalerange
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTargetTrafo`]/[`PipeOp`]
#'
#' @description
#' Linearly transforms a numeric target of a [`TaskRegr`][mlr3::TaskRegr] so it is between `lower`
#' and `upper`. The formula for this is \eqn{x' = offset + x * scale},
#' where \eqn{scale} is \eqn{(upper - lower) / (max(x) - min(x))} and
#' \eqn{offset} is \eqn{-min(x) * scale + lower}. The same transformation is applied during training and
#' prediction.
#'
#' @section Construction:
#' ```
#' PipeOpTargetTrafoScaleRange$new(id = "targettrafoscalerange", param_vals = list())
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
#' The `$state` is a named `list` containing the slots `$offset` and `$scale`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTargetTrafo`], as well as:
#' * `lower` :: `numeric(1)`\cr
#'   Target value of smallest item of input target. Initialized to 0.
#' * `upper` :: `numeric(1)`\cr
#'   Target value of greatest item of input target. Initialized to 1.
#'
#' @section Internals:
#' Overloads [`PipeOpTargetTrafo`]'s `.get_state()`, `.transform()`, and
#' `.invert()`. Should be used in combination with [`PipeOpTargetInvert`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTargetTrafo`]/[`PipeOp`].
#'
#' @examplesIf requireNamespace("rpart")
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
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
PipeOpTargetTrafoScaleRange = R6Class("PipeOpTargetTrafoScaleRange",
  inherit = PipeOpTargetTrafo,
  public = list(
    initialize = function(id = "targettrafoscalerange", param_vals = list()) {
      ps = ps(
        lower = p_dbl(tags = c("required", "train")),
        upper = p_dbl(tags = c("required", "train"))
      )
      ps$values = list(lower = 0, upper = 1)
      super$initialize(id = id, param_set = ps, param_vals = param_vals, task_type_in = "TaskRegr")
    }
  ),
  private = list(
    .get_state = function(task) {
      rng = range(task$data(cols = task$target_names))
      scale = (self$param_set$values$upper - self$param_set$values$lower) / diff(rng)
      offset = -rng[1L] * scale + self$param_set$values$lower
      list(scale = scale, offset = offset)
    },

    .transform = function(task, phase) {
      x = task$data(cols = task$target_names)
      new_target = self$state$offset + x * self$state$scale
      setnames(new_target, paste0(colnames(new_target), ".scaled"))
      task$cbind(new_target)
      convert_task(task, target = colnames(new_target), drop_original_target = TRUE, drop_levels = FALSE)
    },

    .invert = function(prediction, predict_phase_state) {
      response = (prediction$response - self$state$offset) / self$state$scale
      PredictionRegr$new(row_ids = prediction$row_ids,
        truth = predict_phase_state$truth, response = response)
    }
  )
)

mlr_pipeops$add("targettrafoscalerange", PipeOpTargetTrafoScaleRange)

#' @title Transform a Target without an Explicit Inversion
#' @usage NULL
#' @name mlr_pipeops_updatetarget
#' @format Abstract [`R6Class`][R6::R6Class] inheriting from [`PipeOp`].
#'
#' @description
#' EXPERIMENTAL, API SUBJECT TO CHANGE
#'
#' Handles target transformation operations that do not need explicit inversion.
#' In case the new target is required during predict, creates a vector of `NA`.
#' Works similar to [`PipeOpTargetTrafo`] and [`PipeOpTargetMutate`], but forgoes the
#' inversion step.
#' In case target after the `trafo` is a factor, levels are saved to `$state`.\cr
#'
#' During prediction: Sets all target values to `NA` before calling the `trafo` again.
#' In case target after the `trafo` is a factor, levels saved in the `state` are
#' set during prediction.
#'
#' As a special case when `trafo` is `identity` and `new_target_name` matches an existing column
#' name of the data of the input [`Task`][mlr3::Task], this column is set as the new target. Depending on
#' `drop_original_target` the original target is then either dropped or added to the features.
#'
#' @section Construction:
#' ```
#' PipeOpUpdateTarget$new(id, param_set = ps(),
#'   param_vals = list(), packages = character(0))
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. See `$id` slot of [`PipeOp`].
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings given in `param_set`.
#'   The subclass should have its own `param_vals` parameter and pass it on to `super$initialize()`.
#'   Default `list()`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTargetTrafo`], as well as:
#' * `trafo` :: `function`\cr
#'   Transformation function for the target. Should only be a function of the target, i.e., taking a
#'   single argument. Default is `identity`.
#'   Note, that the data passed on to the target is a `data.table` consisting of all target column.
#' * `new_target_name` :: `character(1)`\cr
#'   Optionally give the transformed target a new name. By default the original name is used.
#' * `new_task_type` :: `character(1)`\cr
#'   Optionally a new task type can be set. Legal types are listed in
#'   `mlr_reflections$task_types$type`.
#' #' `drop_original_target` :: `logical(1)`\cr
#'   Whether to drop the original target column. Default: `TRUE`.
#'
#' @section State:
#' The `$state` is a list of class levels for each target after trafo.
#' `list()` if none of the targets have levels.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @examplesIf requireNamespace("rpart")
#' \dontrun{
#' # Create a binary class task from iris
#' library(mlr3)
#' trafo_fun = function(x) {factor(ifelse(x$Species == "setosa", "setosa", "other"))}
#' po = PipeOpUpdateTarget$new(param_vals = list(trafo = trafo_fun, new_target_name = "setosa"))
#' po$train(list(tsk("iris")))
#' po$predict(list(tsk("iris")))
#' }
#' @family mlr3pipelines backend related
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' # not yet exported
PipeOpUpdateTarget = R6Class("PipeOpUpdateTarget",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "update_target", param_vals = list()) {
      ps = ps(
        trafo = p_uty(tags = c("train", "predict"), custom_check = function(x) check_function(x, nargs = 1L)),
        new_target_name = p_uty(tags = c("train", "predict"), custom_check = crate(function(x) check_character(x, any.missing = FALSE, len = 1L))),
        new_task_type = p_uty(tags = c("train", "predict"), custom_check = crate(function(x) check_choice(x, choices = mlr_reflections$task_types$type))),
        drop_original_target = p_lgl(tags = c("train", "predict"))
      )
      ps$values = list(trafo = identity, drop_original_target = TRUE)
      super$initialize(id = id, param_set = ps, param_vals = param_vals,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      pv = self$param_set$values
      if (identical(pv$trafo, identity) && (pv$new_target_name %in% unlist(intask$col_roles, use.names = FALSE))) {
        self$state = list()
        return(list(private$.update_target(intask, drop_levels = TRUE)))  # early exit
      }
      if (!identical(pv$trafo, identity) || !is.null(pv$new_target_name)) {
        # Apply fun to target, rename, cbind and convert task if required
        new_target = data.table(pv$trafo(intask$data(cols = intask$target_names)))
        if (!is.null(pv$new_target_name)) {
          setnames(new_target, colnames(new_target), pv$new_target_name)
        }
        self$state = discard(map(new_target, levels), is.null)
        intask$cbind(new_target)
      } else {
        self$state = list()
      }
      list(private$.update_target(intask, drop_levels = TRUE))
    },

    .predict = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      pv = self$param_set$values
      if (identical(pv$trafo, identity) && (pv$new_target_name %in% unlist(intask$col_roles, use.names = FALSE))) {
        return(list(private$.update_target(intask, drop_levels = FALSE)))  # early exit
      }
      if (!identical(pv$trafo, identity) || !is.null(pv$new_target_name)) {
        new_target = intask$data(cols = intask$target_names)
        if (!pv$drop_original_target)
          # During predict, if original target is not dropped, set the new target to NA and then call the trafo
          new_target = set(new_target, j = intask$target_names, value = NA)
        new_target = data.table(pv$trafo(new_target))
        # Rename, cbind and convert
        setnames(new_target, colnames(new_target), self$param_set$values$new_target_name)
        # Make sure levels match target levels
        if (length(self$state))
          new_target = imap_dtc(new_target, function(x, nms) {
            if(nms %in% names(self$state))
              levels(x) = self$state[[nms]]
            return(x)
        })
        intask$cbind(new_target)
      }
      list(private$.update_target(intask, drop_levels = FALSE))
    },

    # Updates the target of a task and also the task_type (if needed), uses mlr3::convert_task()
    .update_target = function(task, drop_levels) {
      pv = self$param_set$values
      convert_task(task, target = pv$new_target_name,
        new_type = pv$new_task_type, drop_original_target = pv$drop_original_target, drop_levels = drop_levels)
    }
  )
)

# mlr_pipeops$add("update_target", PipeOpUpdateTarget)
