#' @title Subsampling
#'
#' @usage NULL
#' @name mlr_pipeops_subsample
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Subsamples a [`Task`][mlr3::Task] to use a fraction of the rows.
#'
#' Sampling happens only during training phase. Subsampling a [`Task`][mlr3::Task] may be
#' beneficial for training time at possibly (depending on original [`Task`][mlr3::Task] size)
#' negligible cost of predictive performance.
#'
#' @section Construction:
#' ```
#' PipeOpSubsample$new(id = "subsample", param_vals = list())
#' ```
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"subsample"`
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output during training is the input [`Task`][mlr3::Task] with added or removed rows according to the sampling.
#' The output during prediction is the unchanged input.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`]; however, the `affect_columns` parameter is *not* present. Further parameters are:
#' * `frac` :: `numeric(1)`\cr
#'   Fraction of rows in the [`Task`][mlr3::Task] to keep. May only be greater than 1 if `replace` is `TRUE`. Initialized to `(1 - exp(-1)) == 0.6321`.
#' * `stratify` :: `logical(1)`\cr
#'   Should the subsamples be stratified by target? Initialized to `FALSE`. May only be `TRUE` for [`TaskClassif`][mlr3::TaskClassif] input.
#' * `replace` :: `logical(1)`\cr
#'   Sample with replacement? Initialized to `FALSE`.
#'
#' @section Internals:
#' Uses `task$filter()` to remove rows. If `replace` is `TRUE` and identical rows are added, then the `task$row_roles$use` can *not* be used
#' to duplicate rows because of \[inaudible\]; instead the `task$rbind()` function is used, and
#' a new [`data.table`] is attached that contains all rows that are being duplicated exactly as many times as they are being added.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' pos = mlr_pipeops$get("subsample", param_vals = list(frac = 0.7, stratify = TRUE))
#'
#' pos$train(list(tsk("iris")))
#'
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpSubsample = R6Class("PipeOpSubsample",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "subsample", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("frac", lower = 0, upper = Inf, tags = "train"),
        ParamLgl$new("stratify", tags = "train"),
        ParamLgl$new("replace", tags = "train")
      ))
      ps$values = list(frac = 1 - exp(-1), stratify = FALSE, replace = FALSE)
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE)
    }
  ),
  private = list(

    .train_task = function(task) {
      if (!self$param_set$values$stratify) {
        keep = shuffle(task$row_roles$use,
          ceiling(self$param_set$values$frac * task$nrow),
          replace = self$param_set$values$replace)
      } else {
        if (!inherits(task, "TaskClassif")) {
          stopf("Stratification not supported for %s", class(task))
        }
        splt = split(task$row_roles$use, task$data(cols = task$target_names))
        keep = unlist(map(splt, function(x) {
          shuffle(x,
            ceiling(self$param_set$values$frac * length(x)),
            replace = self$param_set$values$replace)
        }))
      }
      self$state = list()
      task_filter_ex(task, keep)
    },

    .predict_task = identity
  )
)

mlr_pipeops$add("subsample", PipeOpSubsample)
