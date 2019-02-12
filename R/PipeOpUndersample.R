#' @title PipeOpUndersample
#'
#' @name mlr_pipeop_undersample
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Undersamples (filters) a [`Task`] to keep only a fraction of the rows with majority class.
#'
#' Possible parameters are `$frac` and `$ratio`, only one of which must be given. The
#' classes are never "up"-sampled, so if `$ratio` times the mean minority class size turns out
#' larger than the initial majority class size, the number of rows is shuffled but not changed.
#'
#' @section Parameter Set:
#' * `frac`  :: `numeric(1)` \cr
#'   Fraction of rows of the majority class to keep in the task.
#' * `ratio` :: `numeric(1)` \cr
#'   Ratio of number of rows of majority class to keep, relative
#'   to average minority class row number. Default is 1.
#' * `all`   :: `logical(1)` \cr
#'   Whether to sub-sample all classes (instead of just the class with the most instances).
#'   If this is `TRUE` (the default), all classes with fewer instances than determined
#'   by `frac` or `ratio` are sub-sampled; otherwise, only the class with most instances
#'   is sub-sampled.
#'
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpUndersample = R6Class("PipeOpUndersample",
  inherit = PipeOpTaskPreproc,

  public = list(
    initialize = function(id = "undersample", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("frac", lower = 0, upper = 1, special_vals = list(NULL)),
        ParamDbl$new("ratio", lower = 0, upper = Inf, special_vals = list(NULL), default = 1),
        ParamLgl$new("all", default = TRUE)
      ))
      ps$values = list(ratio = 1, all = TRUE)
      super$initialize(id, param_set = ps, param_vals = param_vals)

    },

    train_task = function(task) {
      self$state = list()
      truth = task$truth()
      tbl = sort(table(truth), decreasing = TRUE)
      frac = self$param_set$values$frac
      ratio = self$param_set$values$ratio
      if (!is.null(frac)) {
        if (!is.null(ratio)) {
          stop("Only one of 'ratio' and 'frac' params must be given.")
        }
        to_keep = round(frac * tbl[1])
      } else {
        if (is.null(ratio)) {
          stop("One of 'ratio' or 'frac' params must be given.")
        }
        to_keep = round(ratio * mean(tbl[-1]))
      }
      keep_all = rep(TRUE, length(truth))
      for (tbl_idx in seq_along(tbl)) {
        keep_lgl = seq_len(tbl[tbl_idx]) <= to_keep
        keep_all[truth == names(tbl)[tbl_idx]] = shuffle(keep_lgl)
        if (!self$param_set$values$all) {
          break
        }
      }
      task$filter(task$row_ids[keep_all])
    },

    predict_task = identity
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("undersample", PipeOpUndersample)
