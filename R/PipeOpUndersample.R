#' @title PipeOpUndersample
#'
#' @name mlr_pipeop_undersample
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Undersamples (filters) a [`Task`] to keep only a fraction of the rows with majority class.
#'
#' @section Parameter Set:
#' * `frac` :: `numeric(1)` \cr
#'   Fraction of rows of the majority class to keep in the task. Default 1.
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpUndersample = R6Class("PipeOpUndersample",
  inherit = PipeOpTaskPreproc,

  public = list(
    initialize = function(id = "undersample") {
      ps = ParamSet$new(params = list(
        ParamDbl$new("frac", default = 1, lower = 0, upper = 1)
      ))
      super$initialize(id, param_set = ps)
      self$param_vals = list(frac = 1)
    },

    train_task = function(task) {
      row_ids = task$row_ids
      tab = data.table(row_ids = row_ids, label = task$truth(row_ids))[, list(N = .N, ids = list(row_ids)), by = "label"]
      majority = tab[which_max(get("N"))]$label
      tab[get("label") == majority, ids := list(list(sample(ids[[1L]], length(ids[[1L]]) * frac)))]
      self$state = list()
      task$filter(unlist(tab$ids, use.names = FALSE))
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("undersample", PipeOpUndersample)
