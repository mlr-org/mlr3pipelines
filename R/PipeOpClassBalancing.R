#' @title PipeOpClassBalancing
#'
#' @usage NULL
#' @name mlr_pipeops_classbalancing
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Both undersamples a [`Task`][mlr3::Task] to keep only a fraction of the rows of the majority class,
#' as well as oversamples (repeats data points) rows of the minority class.
#'
#' Sampling happens only during training phase. Class-balancing a [`Task`][mlr3::Task] by sampling may be
#' beneficial for classification with imbalanced training data.
#'
#' @section Construction:
#' ```
#' PipeOpClassBalancing$new(id = "classbalancing", param_vals = list())
#' ```
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"classbalancing"`
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`]. Instead of a [`Task`][mlr3::Task], a
#' [`TaskClassif`][mlr3::TaskClassif] is used as input and output during training and prediction.
#'
#' The output during training is the input [`Task`][mlr3::Task] with added or removed rows to balance target classes.
#' The output during prediction is the unchanged input.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`]; however, the `affect_columns` parameter is *not* present. Further parameters are:
#' * `ratio` :: `numeric(1)` \cr
#'   Ratio of number of rows of classes to keep, relative
#'   to the `$reference` value. Initialized to 1.
#' * `reference` :: `numeric(1)` \cr
#'   What the `$ratio` value is measured against. Can be `"all"` (mean instance count of
#'   all classes), `"major"` (instance count of class with most instances), `"minor"`
#'   (instance count of class with fewest instances), `"nonmajor"` (average instance
#'   count of all classes except the major one), `"nonminor"` (average instance count
#'   of all classes except the minor one), and `"one"` (`$ratio` determines the number of
#'   instances to have, per class). Initialized to `"all"`.
#' * `adjust` :: `numeric(1)` \cr
#'   Which classes to up / downsample. Can be `"all"` (up and downsample all to match required
#'   instance count), `"major"`, `"minor"`, `"nonmajor"`, `"nonminor"` (see respective values
#'   for `$reference`), `"upsample"` (only upsample), and `"downsample"`. Initialized to `"all"`.
#' * `shuffle` :: `logical(1)` \cr
#'   Whether to shuffle the rows of the resulting task.
#'   In case the data is upsampled and `shuffle = FALSE`, the resulting task will have the original
#'   rows (which were not removed in downsampling) in the original order, followed by all newly added rows
#'   ordered by target class.
#'   Initialized to `TRUE`.
#'
#' @section Internals:
#' Up / downsampling happens as follows: At first, a "target class count" is calculated, by taking the mean
#' class count of all classes indicated by the `reference` parameter (e.g. if `reference` is `"nonmajor"`:
#' the mean class count of all classes that are not the "major" class, i.e. the class with the most samples)
#' and multiplying this with the value of the `ratio` parameter. If `reference` is `"one"`, then the "target
#' class count" is just the value of `ratio` (i.e. `1 * ratio`).
#'
#' Then for each class that is referenced by the `adjust` parameter (e.g. if `adjust` is `"nonminor"`:
#' each class that is not the class with the fewest samples), [`PipeOpClassBalancing`] either throws out
#' samples (downsampling), or adds additional rows that are equal to randomly chosen samples (upsampling),
#' until the number of samples for these classes equals the "target class count".
#'
#' Uses `task$filter()` to remove rows. When identical rows are added during upsampling, then the `task$row_roles$use` can *not* be used
#' to duplicate rows because of \[inaudible\]; instead the `task$rbind()` function is used, and
#' a new [`data.table`] is attached that contains all rows that are being duplicated exactly as many times as they are being added.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("spam")
#' opb = po("classbalancing")
#'
#' # target class counts
#' table(task$truth())
#'
#' # double the instances in the minority class (spam)
#' opb$param_set$values = list(ratio = 2, reference = "minor",
#'   adjust = "minor", shuffle = FALSE)
#' result = opb$train(list(task))[[1L]]
#' table(result$truth())
#'
#' # up or downsample all classes until exactly 20 per class remain
#' opb$param_set$values = list(ratio = 20, reference = "one",
#'   adjust = "all", shuffle = FALSE)
#' result = opb$train(list(task))[[1]]
#' table(result$truth())
PipeOpClassBalancing = R6Class("PipeOpClassBalancing",
  inherit = PipeOpTaskPreproc,

  public = list(
    initialize = function(id = "classbalancing", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("ratio", lower = 0, upper = Inf, tags = "train"),
        ParamFct$new("reference",
          levels = c("all", "major", "minor", "nonmajor", "nonminor", "one"), tags = "train"),
        ParamFct$new("adjust",
          levels = c("all", "major", "minor", "nonmajor", "nonminor", "upsample", "downsample"), tags = "train"),
        ParamLgl$new("shuffle", default = TRUE, tags = "train")
      ))
      ps$values = list(ratio = 1, reference = "all", adjust = "all", shuffle = TRUE)
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE, task_type = "TaskClassif", tags = "imbalanced data")
    }
  ),
  private = list(

    .train_task = function(task) {

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

    .predict_task = identity
  )
)

mlr_pipeops$add("classbalancing", PipeOpClassBalancing)
