#' @title Algorithm for Dimensionality Reduction
#'
#' @usage NULL
#' @name mlr_pipeops_isomap
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]
#'
#'
#'
#' @section Construction:
#' ```
#' PipeOpIsomap$new(id = "isomap", ...)
#' ```
#'
#' * `ìd` :: `character(1)`\cr
#'   Identifier of resulting object, default `"isomap"`
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with the data projected on the lower dimension.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `embed_result` :: `dimRedResult`\cr
#'   The resulting object after applying the "Isomap"-method from the dimRed package to the data.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `knn` :: `numeric(1)`\cr
#'   The number of nearest neighbors in the graph.
#'   Initialized to 50.
#' * `ndim` :: `numeric(1)`\cr
#'   The number of embedding dimensions.
#'   Initialized to 2.
#' * `get_geod` :: `logical(1)`\cr
#'   Determines whether the distance matrix should be kept in the `$state`
#'   Initialized to `FALSE`.
#' * `.mute` :: `character`\cr
#'   A character vector containing the elements you want to mute during training (c("message", "output")).
#'   Initialized to `character(0)`.
#'
#'
#' @section Internals:
#' Applies the Isomap Embedding from the `dimRed`-package.
#'
#' @section Fields:
#' Only fields inherited from `PipeOp`.
#'
#' @section Methods:
#'
#'
#' @examplesIf requireNamespace("dimRed")
#' library("mlr3")
#' po = po("isomap")
#' po$train(list(tsk("iris")))[[1]]$data()
#' po$predict(list(tsk("iris")))[[1]]$data()
#'
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#'

PipeOpIsomap = R6Class("PipeOpIsomap",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "isomap", param_vals = list()) {
      ps = ps(
        knn = p_int(default = 50, lower = 1, upper = Inf, tags = c("train", "isomap")),
        ndim = p_int(default = 2, lower = 1, upper = Inf, tags = c("train", "isomap")),
        get_geod = p_lgl(default = FALSE, tags = c("train", "isomap")),
        .mute = p_uty(init = c("message", "output"), tags = c("train", "isomap"))
      )
      super$initialize(id = id, param_set = ps, param_vals = param_vals, packages = c("dimRed", "stats"))
    }
  ),
  private = list(
    .train_dt = function(dt, levels, target) {
      embed_result = mlr3misc::invoke(.f = dimRed::embed, .data = dt, .method = "Isomap", .args = self$param_set$get_values(tags = "isomap"))
      self$state = list(embed_result = embed_result)
      embed_result@data@data
    },
    .predict_dt = function(dt, levels) {
      dimRed::predict(self$state$embed_result, as.data.frame(dt))@data
    }
  )
)

mlr_pipeops$add("isomap", PipeOpIsomap)
