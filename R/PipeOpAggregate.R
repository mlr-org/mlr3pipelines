#' @title Aggregate Features Row-Wise
#'
#' @usage NULL
#' @name mlr_pipeops_aggregate
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Aggregates features row-wise based on multiple observations indicated via a column of role `row_reference` according to expressions given as formulas.
#' Typically used after [`PipeOpLearnerCV`]  and prior to [`PipeOpFeatureUnion`] if the resampling method returned multiple predictions per row id.
#' However, note that not all [`Resampling`][mlr3::Resampling] methods result in at least one prediction per original row id.
#'
#' @section Construction:
#' ```
#' PipeOpAggregate$new(id = "aggregate", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"aggregate"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#
#' The output is a [`Task`][mlr3::Task] with the same target as the input [`Task`][mlr3::Task], with features aggregated as specified.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `aggregation` :: named `list` of `formula`\cr
#'   Expressions for how features should be aggregated, in the form of `formula`.
#'   Each element of the list is a `formula` with the name of the element naming the feature to aggregate and the formula expression determining the result.
#'   Each formula is evaluated within [`data.table`] environments of the [`Task`][mlr3::Task] that contain all features split via the `by` argument (see below).
#'   Initialized to `list()`, i.e., no aggregation is performed.
#' * `by` :: `character(1)` | `NULL`\cr
#'   Column indicating the `row_reference` column of the [`Task`][mlr3::Task] that should be the row-wise basis for the aggregation.
#'   Initialized to `NULL`, i.e., no aggregation is performed.
#'
#' @section Internals:
#' A `formula` created using the `~` operator always contains a reference to the `environment` in which
#' the `formula` is created. This makes it possible to use variables in the `~`-expressions that both
#' reference either column names or variable names.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
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
#' calculate_mode = function(x) {
#'   unique_x = unique(x)
#'   unique_x[which.max(tabulate(match(x, unique_x)))]
#' }
#'
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#'
#' lrnloo_po = po("learner_cv", learner, rsmp("loo"))
#' nop = mlr_pipeops$get("nop")
#' agg_po = po("aggregate",
#'   aggregation = list(
#'     classif.rpart.response = ~ calculate_mode(classif.rpart.response)
#'   ),
#'   by = "pre.classif.rpart")
#'
#' graph = gunion(list(
#'   lrnloo_po %>>% agg_po,
#'   nop
#' )) %>>% po("featureunion")
#'
#' graph$train(task)
#'
#' graph$pipeops$classif.rpart$learner$predict_type = "prob"
#' graph$param_set$values$aggregate.aggregation = list(
#'     classif.rpart.prob.setosa = ~ mean(classif.rpart.prob.setosa),
#'     classif.rpart.prob.versicolor = ~ mean(classif.rpart.prob.versicolor),
#'     classif.rpart.prob.virginica = ~ mean(classif.rpart.prob.virginica)
#' )
#' graph$train(task)
PipeOpAggregate = R6Class("Aggregate",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "aggregate", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("aggregation", tags = c("train", "predict", "required"), custom_check = check_aggregation_formulae),
        ParamUty$new("by", tags = c("train", "predict", "required"), custom_check = function(x) check_string(x, null.ok = TRUE))
      ))
      ps$values = list(aggregation = list(), by = NULL)
      super$initialize(id, ps, param_vals = param_vals, tags = "ensemble")
    }
  ),
  private = list(
    .transform = function(task) {

      if (length(self$param_set$values$aggregation) == 0L || is.null(self$param_set$values$by)) {
        return(task)  # early exit
      }

      assert_set_equal(names(self$param_set$values$aggregation), task$feature_names)
      assert_choice(self$param_set$values$by, choices = task$col_roles$row_reference)

      taskdata = task$data(cols = c(task$feature_names, task$col_roles$row_reference))
      taskdata_split = split(taskdata, by = self$param_set$values$by)

      newdata = unique(task$data(cols = c(task$target_names, task$col_roles$row_reference[match(task$col_roles$row_reference, self$param_set$values$by)])), by = self$param_set$values$by)

      nms = names(self$param_set$values$aggregation)
      for (i in seq_along(nms)) {
        frm = self$param_set$values$aggregation[[i]]
        set(newdata, j = nms[i], value = unlist(map(taskdata_split, .f = function(split) eval(frm[[2L]], envir = split, enclos = environment(frm)))))
      }
      setnames(newdata, old = self$param_set$values$by, new = task$backend$primary_key)

      # get task_type from mlr_reflections and call constructor
      constructor = get(mlr_reflections$task_types[["task"]][chmatch(task$task_type, table = mlr_reflections$task_types[["type"]], nomatch = 0L)][[1L]])
      newtask = invoke(constructor$new, id = task$id, backend = as_data_backend(newdata, primary_key = task$backend$primary_key), target = task$target_names, .args = task$extra_args)
      newtask$extra_args = task$extra_args

      newtask
    }
  )
)

mlr_pipeops$add("aggregate", PipeOpAggregate)

# check the `aggregation` parameter of PipeOpAggregate
# @param x [list] whatever `aggregation` is being set to
# checks that `aggregation` is
# * a named list of `formula`
# * that each element has only a rhs
check_aggregation_formulae = function(x) {
  check_list(x, types = "formula", names = "unique") %check&&%
    Reduce(`%check&&%`, lapply(x, function(xel) {
      if (length(xel) != 2L) {
        return(sprintf("formula %s must not have a left hand side.",
          deparse(xel, nlines = 1L, width.cutoff = 500L)))
      }
      TRUE
    }), TRUE)
}

