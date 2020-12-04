#' @title PipeOpColApply
#'
#' @usage NULL
#' @name mlr_pipeops_colapply
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Applies a function to each column of a task. Use the `affect_columns` parameter inherited from
#' [`PipeOpTaskPreprocSimple`] to limit the columns this function should be applied to. This can be used
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
#' Input and output channels are inherited from [`PipeOpTaskPreprocSimple`].
#'
#' The output is the input [`Task`][mlr3::Task] with features changed according to the `applicator` parameter.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreprocSimple`], as well as:
#' * `emptydt` :: [`data.table`]\cr
#'   An empty [`data.table`] with columns of names and types from *output* features after training. This is used
#'   to produce a correct type conversion during prediction, even when the input has zero length and
#'   `applicator` is therefore not called.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreprocSimple`], as well as:
#' * `applicator` :: `function`\cr
#'   Function to apply to each column of the task. The return value must have the
#'   same length as the input, i.e. vectorize over the input. A typical example would be `as.numeric`.
#'   Use [`Vectorize`][base::Vectorize] to create a vectorizing function from any function that
#'   ordinarily only takes one element input.\cr
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
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
  inherit = PipeOpTaskPreprocSimple,
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

    .get_state_dt = function(dt, levels, target) {
      list()
    },

    .transform_dt = function(dt, levels) {
      applicator = self$param_set$values$applicator
      dt[, names(dt) := lapply(dt, applicator)]
    }
  )
)

mlr_pipeops$add("colapply", PipeOpColApply)
