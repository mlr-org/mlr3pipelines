#' @title Conditional Target Value Impact Encoding
#'
#' @usage NULL
#' @name mlr_pipeops_encodeimpact
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Encodes columns of type `factor`, `character` and `ordered`.
#'
#' Impact coding for [classification Tasks][mlr3::TaskClassif] converts factor levels of each (factorial) column
#' to the difference between each target level's conditional log-likelihood
#' given this level, and the target level's global log-likelihood.
#'
#' Impact coding for [regression Tasks][mlr3::TaskRegr] converts factor levels of each (factorial) column
#' to the difference between the target's conditional mean given
#' this level, and the target's global mean.
#'
#' Treats new levels during prediction like missing values.
#'
#' @section Construction:
#' ```
#' PipeOpEncodeImpact$new(id = "encodeimpact", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"encodeimpact"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would
#'   otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected `factor`, `character` or
#' `ordered` parameters encoded.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `impact` :: a named `list`\cr
#'   A list with an element for each affected feature:\cr
#'   For regression each element is a single column matrix of impact values for each level of that feature.\cr
#'   For classification, it is a list with an element for each *feature level*, which is a vector giving the impact of
#'   this feature level on each *outcome level*.
#'
#' @section Parameters:
#' * `smoothing`  :: `numeric(1)` \cr
#'   A finite positive value used for smoothing. Mostly relevant for [classification Tasks][mlr3::TaskClassif] if
#'   a factor does not coincide with a target factor level (and would otherwise give an infinite logit value).
#'   Initialized to `1e-4`.
#' * `impute_zero` :: `logical(1)`\cr
#'   If `TRUE`, impute missing values as impact 0; otherwise the respective impact is coded as `NA`. Default `FALSE`.
#'
#' @section Internals:
#' Uses Laplace smoothing, mostly to avoid infinite values for [classification Task][mlr3::TaskClassif].
#'
#' @section Methods:
#' Only methods inherited [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#' poe = po("encodeimpact")
#'
#' task = TaskClassif$new("task",
#'   data.table::data.table(
#'     x = factor(c("a", "a", "a", "b", "b")),
#'     y = factor(c("a", "a", "b", "b", "b"))),
#'   "x")
#'
#' poe$train(list(task))[[1]]$data()
#'
#' poe$state
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpEncodeImpact = R6Class("PipeOpEncodeImpact",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "encodeimpact", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("smoothing", 0, Inf, tags = c("train", "required")),
        ParamLgl$new("impute_zero", tags = c("train", "required"))
      ))
      ps$values = list(smoothing = 1e-4, impute_zero = FALSE)
      super$initialize(id, param_set = ps, param_vals = param_vals, tags = "encode", feature_types = c("factor", "ordered"),
        label = "Conditional Target Value Impact Encoding", man = "mlr3pipelines::mlr_pipeops_encodeimpact")
    }
  ),
  private = list(

    .get_state_dt = function(dt, levels, target) {
      task_type = if (is.numeric(target)) "regr" else "classif"
      state = list()

      smoothing = self$param_set$values$smoothing

      # different funs depending on task.type
      list(impact = switch(task_type,
        classif = sapply(dt, function(col)
          sapply(levels(target), function(tl) {
            tprop = (sum(target == tl) + smoothing) / (length(target) + 2 * smoothing)
            tplogit = log(tprop / (1 - tprop))
            map_dbl(c(stats::setNames(levels(col), levels(col)), c(.TEMP.MISSING = NA)),
              function(cl) {
                if (!self$param_set$values$impute_zero && is.na(cl)) return(NA_real_)
                condprob = (sum(target[is.na(cl) | col == cl] == tl, na.rm = TRUE) + smoothing) /
                  (sum(is.na(cl) | col == cl, na.rm = TRUE) + 2 * smoothing)
                cplogit = log(condprob / (1 - condprob))
                cplogit - tplogit
              })
          }), simplify = FALSE),
        regr = {
          meanimp = mean(target)
          sapply(dt, function(col)
            t(t(c(sapply(levels(col), function(lvl) {
              (sum(target[col == lvl], na.rm = TRUE) + smoothing * meanimp) /
                (sum(col == lvl, na.rm = TRUE) + smoothing) - meanimp
            }), if (self$param_set$values$impute_zero) c(.TEMP.MISSING = 0) else c(.TEMP.MISSING = NA)))), simplify = FALSE)
        }))
    },

    .transform_dt = function(dt, levels) {
      impact = self$state$impact
      imap(dt, function(curdat, idx) {
        curdat = as.character(curdat)
        curdat[is.na(curdat)] = ".TEMP.MISSING"
        curdat[curdat %nin% rownames(impact[[idx]])] = ".TEMP.MISSING"
        # we only want to "drop" if there are no column names.
        # otherwise we want the naming scheme <original feature name>.<target level>
        impact[[idx]][match(curdat, rownames(impact[[idx]])), , drop = is.null(colnames(impact[[idx]]))]
      })
    }
  )
)

mlr_pipeops$add("encodeimpact", PipeOpEncodeImpact)
