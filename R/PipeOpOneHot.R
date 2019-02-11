#' @title PipeOpOneHot
#'
#' @name mlr_pipeop_onehot
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' One-hot encodes all factors
#'
#' Possible parameters are `$frac` and `$ratio`, only one of which must be given. The majority
#' class is never "up"-sampled, so if `$ratio` times the mean minority class size turns out
#' larger than the initial majority class size, the number of rows is shuffled but not changed.
#'
#' @section Parameter Set:
#' * `method`  :: `character(1)` \cr
#'   If set to "1-of-n", creates a new column for each factor level.
#'   If set to "reference", creates $n-1$ columns leaving out the first factor level of each factor variable.
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpOneHot = R6Class("PipeOpOneHot",
  inherit = PipeOpTaskPreproc,

  public = list(
    initialize = function(id = "onehot", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamFct$new("method", levels = c("1-of-n", "reference"), default = "1-of-n")
        ))
      ps$values = list(method = "1-of-n")
      super$initialize(id, param_set = ps, param_vals = param_vals)
    },

    train_task = function(task) {
      self$state = list()
      one_hot(task, self$param_set$param_vals$method)
    },

    predict_task = function(task) {
      one_hot(task, self$param_set$param_vals$method)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("onehot", PipeOpOneHot)


one_hot = function(task, method) {
  info = task$col_info[task$feature_names, on = "id"][get("type") == "factor" & lengths(levels) > 0L, c("id", "levels")]
  if (method == "reference")
    info[, "levels" := lapply(get("levels"), tail, -1L)]
  data = task$data(col = info$id)

  new_cols = pmap_dtc(info, function(id, levels) {
    x = data[[id]]
    tab = as.data.table(outer(x, levels, FUN = "=="))
    setnames(tab, names(tab), make.names(sprintf("%s.%s", id, levels), unique = TRUE))
  })
  task$select(setdiff(task$feature_names, info$id))$cbind(new_cols)
}
