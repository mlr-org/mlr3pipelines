#' @title Apply a Function to each Column of a Task
#'
#' @usage NULL
#' @name mlr_pipeops_colapply
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
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
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreprocSimple`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreprocSimple`], as well as:
#' * `applicator` :: `function`\cr
#'   Function to apply to each column of the task.
#'   The return value should be a `vector` of the same length as the input, i.e., the function vectorizes over the input.
#'   A typical example would be `as.numeric`.\cr
#'   The return value can also be a `matrix`, `data.frame`, or [`data.table`][data.table::data.table].
#'   In this case, the length of the input must match the number of returned rows.
#'   The names of the resulting features of the output [`Task`][mlr3::Task] is based on the (column) name(s) of the return value of the applicator function,
#'   prefixed with the original feature name separated by a dot (`.`).
#'   Use [`Vectorize`][base::Vectorize] to create a vectorizing function from any function that ordinarily only takes one element input.\cr
#'
#' @section Internals:
#' Calls [`map`][mlr3misc::map] on the data, using the value of `applicator` as `f.` and coerces the output via [`as.data.table`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @template seealso_pipeopslist
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
#' f1 = function(x) {
#'   # we could use `ifelse` here, but that is not the point
#'   if (x > 1) {
#'     "a"
#'   } else {
#'     "b"
#'   }
#' }
#' poca$param_set$values$applicator = Vectorize(f1)
#' poca$train(list(task))[[1]]$data()
#'
#' # only affect Petal.* columns
#' poca$param_set$values$affect_columns = selector_grep("^Petal")
#' poca$train(list(task))[[1]]$data()
#'
#' # function returning multiple columns
#' f2 = function(x) {
#'   cbind(floor = floor(x), ceiling = ceiling(x))
#' }
#' poca$param_set$values$applicator = f2
#' poca$param_set$values$affect_columns = selector_all()
#' poca$train(list(task))[[1]]$data()
PipeOpRowApply = R6Class("PipeOpRowApply",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "rowapply", param_vals = list()) {
      ps = ps(
        applicator = p_uty(custom_check = check_function, tags = c("train", "predict")),
        col_prefix = p_uty(custom_check = check_string) # tags necessary?
      )
      ps$values = list(
        applicator = identity,
        col_prefix = ""
      )
      super$initialize(id, ps, param_vals = param_vals)
      if ("affect_columns" %nin% names(param_vals)) {
        # can't put this in `ps$values` because it is a PipeOpTaskPreproc param
        self$param_set$values$affect_columns = selector_type(c("numeric", "integer"))
        # copied from PipeOpMissingIndicators
        # decided for this (against param select_cols), since this seemed like the more natural solution
      }
    }
  ),
  private = list(

    .get_state_dt = function(dt, levels, target) {
      list()
    },

    .transform_dt = function(dt, levels) {
      applicator = self$param_set$values$applicator
      col_prefix = self$param_set$values$col_prefix
      cnames = colnames(dt) # better way, in ref. to tasK? this way: correct order guaranteed

      dt = t(apply(dt, 1, applicator))
      if (!(col_prefix == "")) { # could do nzchar, but we know it is of lenght 1
        cnames = paste(col_prefix, cnames, sep = ".") # is there a usual way this is done in mlr?
      }
      colnames(dt) = cnames # is setnames() better?
      dt # currently doesnt keep col order, undesired?
    }
  )
)

mlr_pipeops$add("rowapply", PipeOpRowApply)

#https://github.com/mlr-org/mlr3pipelines/issues/318

# Fragen:
# - What should this PO do exactly? Doesn't it change the column layout, so it couldn't be a TaskPreprocSimple?
# - how do we change the data in the task correctly? -> Examples
# - How do we initliaze the select_cols correctly? isnt it same as affect_col? -> Examples

applicator = function(x) scale(x, scale = FALSE)
applicator = function(x) mean(x, na.rm = TRUE)
# how would row-wise mean work? add a column? would change layout of task
pora <- po("rowapply", applicator = applicator)
task <- tsk("titanic")
pora$train(list(task))[[1]]$head()
task$head()

dt = task$data(cols = task$feature_names[c(1,4)], rows = 1:5)
t(apply(dt, 1, applicator))

# General question: When to use feature_type (implying .select_col), when to use affect_column?

