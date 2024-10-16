#' @title Subsampling
#'
#' @usage NULL
#' @name mlr_pipeops_subsample
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
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
#'   Should the subsamples be stratified by target? Initialized to `FALSE`. May only be `TRUE` for [`TaskClassif`][mlr3::TaskClassif] input and if `use_groups = FALSE`.
#' * `use_groups` :: `logical(1)`\cr
#'   If `TRUE` and if the [`Task`][mlr3::Task] has a column with role `group`, grouped observations are kept together during subsampling. May only be `TRUE` if `strafiy = FALSE`.
#'   Initialized to `TRUE`.
#' * `replace` :: `logical(1)`\cr
#'   Sample with replacement? Initialized to `FALSE`.
#'
#' @section Internals:
#' Uses `task$filter()` to remove rows. If `replace` is `TRUE` and identical rows are added, then the `task$row_roles$use` can *not* be used
#' to duplicate rows because of \[inaudible\]; instead the `task$rbind()` function is used, and
#' a new [`data.table`][data.table::data.table] is attached that contains all rows that are being duplicated exactly as many times as they are being added.
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
#' # Subsample with stratification
#' pop = po("subsample", frac = 0.7, stratify = TRUE, use_groups = FALSE)
#' pop$train(list(tsk("iris")))
#'
#' # Subsample, respecting grouping
#' df = data.frame(
#'   target = runif(3000),
#'   x1 = runif(3000),
#'   x2 = runif(3000),
#'   grp = sample(paste0("g", 1:100), 3000, replace = TRUE)
#' )
#' task = TaskRegr$new(id = "example", backend = df, target = "target")
#' task$set_col_roles("grp", "group")
#'
#' pop = po("subsample", frac = 0.7, use_groups = TRUE)
#' pop$train(list(task))
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpSubsample = R6Class("PipeOpSubsample",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "subsample", param_vals = list()) {
      ps = ps(
        frac = p_dbl(lower = 0, upper = Inf, tags = "train"),
        stratify = p_lgl(tags = "train"),
        use_groups = p_lgl(tags = "train"),
        replace = p_lgl(tags = "train")
      )
      ps$values = list(frac = 1 - exp(-1), stratify = FALSE, use_groups = TRUE, replace = FALSE)
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE)
    }
  ),
  private = list(

    .train_task = function(task) {
      pv = self$param_set$get_values(tags = "train")

      # error message for frac > 1 and replace = FALSE?
      # TODO: test what happens when we do this

      if (pv$stratify && pv$use_groups) {
        stop("Cannot combine stratification with grouping")
      } else if (pv$use_groups && !is.null(task$groups)) {
        # task$groups automatically removes rows not in task$row_roles$use and allows rows to be included multiple times
        grp_sizes = table(task$groups$group)

        if (pv$replace) {
          # Draw groups as long as the fraction of sampled rows is below the desired fraction
          shuffled = numeric(0)
          while (sum(shuffled) / sum(grp_sizes) < pv$frac) {
            shuffled = c(shuffled, shuffle(grp_sizes, 1, replace = pv$replace))
          }
          # Only need to check last two entries => possible improvement?
          cutoff_index = which.min(abs(cumsum(shuffled) / sum(grp_sizes) - pv$frac))
          keep_grps = names(shuffled[seq_len(cutoff_index)])
        } else {
          # We randomly shuffle the groups and keep all up to the group for
          # which the fraction of sampled rows is closest to the desired fraction.
          shuffled = shuffle(grp_sizes, replace = pv$replace)
          cutoff_index = which.min(abs(cumsum(shuffled) / sum(grp_sizes) - pv$frac))
          keep_grps = names(shuffled[seq_len(cutoff_index)])
        }
        group = NULL  # for binding
        keep_dt = data.table(group = keep_grps)
        keep = task$groups[keep_dt, on = "group", allow.cartesian = TRUE]$row_id
        #keep = task$groups[group %in% keep_grps]$row_id
      } else if (pv$stratify) {
        if (!inherits(task, "TaskClassif")) {
          stopf("Stratification not supported for %s", class(task))
        }
        splt = split(task$row_roles$use, task$data(cols = task$target_names))
        keep = unlist(map(splt, function(x) {
          shuffle(x, ceiling(pv$frac * length(x)), replace = pv$replace)
        }))
      } else {
        keep = shuffle(task$row_roles$use, ceiling(pv$frac * task$nrow), replace = pv$replace)
      }

      self$state = list()
      task_filter_ex(task, keep)
    },

    .predict_task = identity
  )
)

mlr_pipeops$add("subsample", PipeOpSubsample)
