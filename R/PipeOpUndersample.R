#' @title PipeOpUndersample
#'
#' @name mlr_pipeop_undersample
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Undersamples (filters) a [`Task`] to keep only a fraction of the rows with majority class.
#'
#' Possible parameters are `$frac` and `$ratio`, only one of which must be given. The majority
#' class is never "up"-sampled, so if `$ratio` times the mean minority class size turns out
#' larger than the initial majority class size, the number of rows is shuffled but not changed.
#'
#' @section Parameter Set:
#' * `frac`  :: `numeric(1)` \cr
#'   Fraction of rows of the majority class to keep in the task.
#' * `ratio` :: `numeric(1)` \cr
#'   Ratio of number of rows of majority class to keep, relative
#'   to average minority class row number. Default is 1.
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpUndersample = R6Class("PipeOpUndersample",
  inherit = PipeOpTaskPreproc,

  public = list(
    initialize = function(id = "undersample") {
      ps = ParamSet$new(params = list(
        ParamDbl$new("frac", lower = 0, upper = 1, special_vals = list(NULL)),
        ParamDbl$new("ratio", lower = 0, upper = Inf, special_vals = list(NULL), default = 1)
      ))
      super$initialize(id, param_set = ps)
      self$param_vals = list(ratio = 1)
    },

    train_task = function(task) {
      self$state = list()
      classtbl = sort(table(task$truth()), decreasing = TRUE)
      frac = self$param_vals$frac
      ratio = self$param_vals$ratio
      if (!is.null(frac)) {
        if (!is.null(ratio)) {
          stop("Only one of 'ratio' and 'frac' params must be given.")
        }
        keep_major = seq_len(classtbl[1]) <= round(frac * classtbl[1])
      } else {
        if (is.null(ratio)) {
          stop("One of 'ratio' or 'frac' params must be given.")
        }
        keep_major = seq_len(classtbl[1]) <= round(ratio * mean(classtbl[-1]))
      }
      keep_all = rep(TRUE, task$nrow)
      keep_all[task$truth() == names(classtbl)[1]] = sample(keep_major)
      task$filter(which(keep_all))
    },

    predict_task = identity
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("undersample", PipeOpUndersample)
