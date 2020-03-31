#' @title PipeOpColApply
#'
#' @usage NULL
#' @name mlr_pipeops_colapply
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Applies a function to each column of a task. Use the `affect_columns` parameter inherited from
#' [`PipeOpTaskPreproc`] to limit the columns this function should be applied to. This can be used
#' for simple parameter transformations or type conversions (e.g. `as.numeric`).
#'
#' The same function is applied during training and prediction. One important relationship for
#' machine learning preprocessing is that during the prediction phase, the preprocessing on each
#' data row should be independent of other rows. Therefore, the `applicator` function should always
#' return a vector / list where each result component only depends on the corresponding input component and
#' not on other components. As a rule of thumb, if the function `f` generates output different
#' from `Vectorize(f)`, it is not a function that should be used for `applicator`.
#'
#' @section Construction:
#' ```
#' PipeOpColApply$new(id = "colapply", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"colapply"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with features changed according to the `applicator` parameter.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `emptydt` :: [`data.table`]\cr
#'   An empty [`data.table`] with columns of names and types from *output* features after training. This is used
#'   to produce a correct type conversion during prediction, even when the input has zero length and
#'   `applicator` is therefore not called.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `applicator` :: `function`\cr
#'   Function to apply to each column of the task. The return value must have the
#'   same length as the input, i.e. vectorize over the input. A typical example would be `as.numeric`.
#'   Use [`Vectorize`][base::Vectorize] to create a vectorizing function from any function that
#'   ordinarily only takes one element input.\cr
#'   The `applicator` is not called during prediction if the input task has no rows; instead the
#'   types of affected features are changed to the result types of the `applicator` call during training.
#'   Initialized to the `identity()`-function.
#'
#' @section Internals:
#' [`PipeOpColApply`] can not inherit from [`PipeOpTaskPreprocSimple`], because if `applicator` is given
#' and the prediction data has 0 rows, then the resulting [`data.table`] does not know
#' what the column types should be. Column type conformity between training and prediction is enforced
#' by simply saving a copy of an empty [`data.table`] in the `$state$emptydt` slot.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' poca = po("colapply", applicator = as.character)
#' poca$train(list(task))[[1]]  # types are converted
#'
#' # function that does not vectorize
#' f = function(x) {
#'   # we could use `ifelse` here, but that is not the point
#'   if (x > 1) {
#'     "a"
#'   } else {
#'     "b"
#'   }
#' }
#' poca$param_set$values$applicator = Vectorize(f)
#' poca$train(list(task))[[1]]$data()
#'
#' # only affect Petal.* columns:
#' poca$param_set$values$affect_columns = selector_grep("^Petal")
#' poca$train(list(task))[[1]]$data()
PipeOpColApply = R6Class("PipeOpColApply",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "colapply", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("applicator", custom_check = check_function, tags = c("train", "predict"))
      ))
      ps$values = list(applicator = identity)
      super$initialize(id, ps, param_vals = param_vals)
    }
  ),
  private = list(

    .train_dt = function(dt, levels, target) {
      dt = self$transform_dt(dt, levels)
      self$state = list(emptydt = dt[integer(0)])
      dt
    },

    .predict_dt = function(dt, levels) {
      dt = self$transform_dt(dt, levels)
      if (!nrow(dt)) {
        dt = self$state$emptydt
      }
      dt
    },

    .transform_dt = function(task, task_levels) {
      applicator = self$param_set$values$applicator
      task[, names(task) := lapply(task, applicator)]
    }
  )
)

mlr_pipeops$add("colapply", PipeOpColApply)
