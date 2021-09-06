#' @title Project Numeric Features onto a Randomly Sampled Subspace
#'
#' @usage NULL
#' @name mlr_pipeops_randomprojection
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Projects numeric features onto a randomly sampled subspace. All numeric features
#' (or the ones selected by `affect_columns`) are replaced by numeric features
#' `PR1`, `PR2`, ... `PRn`
#'
#' Samples with features that contain missing values result in all `PR1`..`PRn` being
#' NA for that sample, so it is advised to do imputation *before* random projections
#' if missing values can be expected.
#'
#' @section Construction:
#' ```
#' PipeOpRandomProjection$new(id = "randomprojection", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"randomprojection"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that
#'   would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with affected numeric features
#' projected onto a random subspace.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`],
#' as well as an element `$projection`, a `matrix`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `rank` :: `integer(1)`\cr
#'   The dimension of the subspace to project onto. Initialized to 1.
#'
#' @section Internals:
#' If there are `n` (affected) numeric features in the input [`Task`][mlr3::Task],
#' then `$state$projection` is a `rank` x `m` `matrix`. The output is calculated as
#' `input %*% state$projection`.
#'
#' The random projection matrix is obtained through Gram-Schmidt orthogonalization
#' from a matrix with values standard normally distributed, which gives a distribution
#' that is rotation invariant, as per Eaton: Multivariate Statistics, A Vector Space
#' Approach, Pg. 234.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("randomprojection", rank = 2)
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpRandomProjection = R6Class("PipeOpRandomProjection",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "randomprojection", param_vals = list()) {
      ps = ParamSet$new(list(
        ParamInt$new("rank", lower = 0, tags = "train")
      ))
      ps$values = list(rank = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(
    .get_state_dt = function(dt, levels, target) {
      m = ncol(dt)
      r = self$param_set$values$rank
      # we want to cope with a possible case of rank > m
      # In that case, we need a square matrix here.
      if (r > 0) {
        projection = qr.Q(qr(matrix(stats::rnorm(max(m, r) * r), ncol = r)))[seq_len(m), ]
        projection = as.matrix(projection, nrow = m) # for rank 1 case
      } else {
        projection = matrix(0, nrow = m, ncol = r)
      }
      colnames(projection) = rep_suffix("PR", ncol(projection))
      rownames(projection) = colnames(dt)
      list(projection = projection)
    },
    .transform_dt = function(dt, levels) {
      as.matrix(dt) %*% self$state$projection
    }
  )
)

mlr_pipeops$add("randomprojection", PipeOpRandomProjection)
