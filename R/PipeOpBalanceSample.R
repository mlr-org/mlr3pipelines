#' @title PipeOpBalanceSample
#'
#' @name mlr_pipeop_balancesample
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Both undersamples (filters) a [`Task`] to keep only a fraction of the rows with majority class,
#' as well as oversamples (repeats datapoints) minorit-class rows.
#'
#' Possible parameters are `$ratio`, `$reference`, and `$adjust`.
#'
#' @section Parameter Set:
#' * `ratio` :: `numeric(1)` \cr
#'   Ratio of number of rows of classes to keep, relative
#'   to the `$reference` value.
#' * `reference` :: `numeric(1)` \cr
#'   What the `$ratio` value is measured against. Can be `"all"` (default, mean instance count of
#'   all classes), `"major"` (instance count of class with most instances), `"minor"`
#'   (instance count of class with fewest instances), `"nonmajor"` (average instance
#'   count of all classes except the major one), `"nonminor"` (average instance count
#'   of all classes except the minor one), and `"one"` (`$ratio` determines the number of
#'   instances to have, per class).
#' * `adjust` :: `numeric(1)` \cr
#'   Which classes to up / downsample. Can be `"all"` (default, up and downsample all to match required
#'   instance count), `"major"`, `"minor"`, `"nonmajor"`, `"nonminor"` (see respective values
#'   for `$reference`), `"upsample"` (only upsample), and `"downsample"`.
#' * `shuffle` :: `logical(1)` \cr
#'   Whether to shuffle the result. Otherwise, the resulting task will have the original items that
#'   were not removed in downsampling in-order, followed by all newly sampled items ordered by target class.
#'   Default is `TRUE`.
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpBalanceSample = R6Class("PipeOpBalanceSample",
  inherit = PipeOpTaskPreproc,

  public = list(
    initialize = function(id = "balancesample", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("ratio", lower = 0, upper = Inf, default = 1),
        ParamFct$new("reference",
          levels = c("all", "major", "minor", "nonmajor", "nonminor", "one"),
          default = "all"),
        ParamFct$new("adjust",
          levels = c("all", "major", "minor", "nonmajor", "nonminor", "upsample", "downsample"),
          default = "all"),
        ParamLgl$new("shuffle", default = TRUE)
      ))
      ps$values = list(ratio = 1, reference = "all", adjust = "all", shuffle = TRUE)
      super$initialize(id, param_set = ps, param_vals = param_vals)
    },

    train_task = function(task) {
      self$state = list()
      truth = task$truth()
      tbl = sort(table(truth), decreasing = TRUE)
      reference = switch(self$param_set$values$reference,
        all = mean(tbl),
        major = tbl[1],
        minor = tbl[length(tbl)],
        nonmajor = mean(tbl[-1]),
        nonminor = mean(tbl[-length(tbl)]),
        one = 1)
      target_size = round(self$param_set$values$ratio * reference)

      adjustable = switch(self$param_set$values$adjust,
        all = names(tbl),
        major = names(tbl)[1],
        minor = names(tbl)[length(tbl)],
        nonmajor = names(tbl)[-1],
        nonminor = names(tbl)[-length(tbl)],
        upsample = names(tbl)[tbl < target_size],
        downsample = names(tbl)[tbl > target_size])

      keep_all = rep(TRUE, length(truth))
      orig_ids = task$row_ids
      add_ids = integer(0)
      for (adjusting in adjustable) {
        if (tbl[adjusting] >= target_size) {
          # downsampling
          keep_lgl = seq_len(tbl[adjusting]) <= target_size
          keep_all[truth == adjusting] = shuffle(keep_lgl)
        } else {
          # upsampling
          add_ids = c(add_ids, rep_len(shuffle(orig_ids[truth == adjusting]), target_size - tbl[adjusting]))
        }
      }
      new_ids = c(orig_ids[keep_all], add_ids)
      if (self$param_set$values$shuffle) {
        new_ids = shuffle(new_ids)
      }
      task_filter_ex(task, new_ids)
    },

    predict_task = identity
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("balancesample", PipeOpBalanceSample)
