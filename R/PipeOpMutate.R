#' @title PipeOpMutate
#'
#' @usage NULL
#' @name mlr_pipeops_mutate
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Adds features according to expressions given as formulas that may depend on values of other features.
#' This can add new features, or can change existing features.
#'
#' @section Construction:
#' ```
#' PipeOpMutate$new(id = "mutate", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"mutate"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with added and/or mutated features according to the `mutation` parameter.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `mutation` :: named `list` of `formula`\cr
#'   Expressions for new features to create (or present features to change), in the form of `formula`.
#'   Each element of the list is a `formula` with the name of the element naming the feature to create or
#'   change, and the formula expression determining the result. This expression may reference
#'   other features, as well as variables visible at the creation of the `formula` (see examples).
#'   Initialized to `list()`.
#' * `delete_originals` :: `logical(1)` \cr
#'   Whether to delete original features. Even when this is `FALSE`,
#'   present features may still be overwritten. Initialized to `FALSE`.
#'
#' @section Internals:
#' A `formula` created using the `~` operator always contains a reference to the `environment` in which
#' the `formula` is created. This makes it possible to use variables in the `~`-expressions that both
#' reference either column names or variable names.
#'
#' Note that the `formula`s in `mutation` are evaluated sequentially. This
#' allows for using variables that were constructed during evaluation of a
#' previous formula. Therefore, be cautious when changing existing features.
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
#' constant = 1
#' pom = po("mutate")
#' pom$param_set$values$mutation = list(
#'   Sepal.Length_plus_constant = ~ Sepal.Length + constant,
#'   Sepal.Area = ~ Sepal.Width * Sepal.Length,
#'   Petal.Area = ~ Petal.Width * Petal.Length,
#'   Sepal.Area_plus_Petal.Area = ~ Sepal.Area + Petal.Area
#' )
#'
#' pom$train(list(tsk("iris")))[[1]]$data()
PipeOpMutate = R6Class("PipeOpMutate",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "mutate", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("mutation", custom_check = check_mutation_formulae, tags = c("train", "predict", "required")),
        ParamLgl$new("delete_originals", tags = c("train", "predict", "required"))
      ))
      ps$values = list(mutation = list(), delete_originals = FALSE)
      super$initialize(id, ps, param_vals = param_vals)
    }
  ),
  private = list(

    .transform = function(task) {
      taskdata = task$data(cols = task$feature_names)

      # sequentially evaluate the formulas
      # this allows us to use variables constructed/changed earlier in the loop
      nms = names(self$param_set$values$mutation)
      for (i in seq_along(nms)) {
        frm = self$param_set$values$mutation[[i]]
        set(taskdata, j = nms[i], value = eval(frm[[2L]], envir = taskdata, enclos = environment(frm)))
      }
      newdata = taskdata[, ..nms]

      keep_feats = character(0)
      if (!self$param_set$values$delete_originals) {
        keep_feats = setdiff(task$feature_names, colnames(newdata))
      }
      task = task$select(keep_feats)
      if (nrow(newdata) == 1) {
        # if the user gave us something like "one = ~1" to introduce a constant column, we will only
        # have a single row here and need to copy that.
        newdata = newdata[rep(1, task$nrow)]
      }
      if (ncol(newdata) && nrow(newdata) != task$nrow) {
        stopf("PipeOpMutate expression result has %s rows but must have %s rows.", nrow(newdata), task$nrow)
      }
      if (ncol(newdata)) task$cbind(newdata)  # TODO: test if we can live without the `if()` here, but there seems to be a problem with 0-row data.tables
      task
    }
  )
)

# check the `mutation` parameter of PipeOpMutate
# @param x [list] whatever `mutation` is being set to
# checks that `mutation` is
# * a named list of `formula`
# * that each element has only a lhs
check_mutation_formulae = function(x) {
  check_list(x, types = "formula", names = "unique") %&&%
    Reduce(`%&&%`, lapply(x, function(xel) {
      if (length(xel) != 2) {
        return(sprintf("formula %s must not have a left hand side.",
          deparse(xel, nlines = 1, width.cutoff = 500)))
      }
      TRUE
    }), TRUE)
}

mlr_pipeops$add("mutate", PipeOpMutate)
