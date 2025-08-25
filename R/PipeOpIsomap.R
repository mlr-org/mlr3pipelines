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
#'   A character vector containing the elements you want to mute (c("message", "output")).
#'   Initialized to `character(0)`.
#'
#'
#' @section Internals:
#'
#' @section Fields:
#'
#' @section Methods:
#'
#' @examples NULL
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
        knn = p_int(default = 50, lower = 1, upper = Inf, tags = "train"), # tag isomap?
        ndim = p_int(default = 2, lower = 1, upper = Inf, tags = "train"), #tag isomap?
        get_geod = p_lgl(default = FALSE, tags = "train"),
        .mute = p_uty(tags = "train")
      )
      ps$values = list(knn = 50, ndim = 2, get_geod = FALSE)
      super$initialize(id = id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer", "factor", "logical"))
    }
  ),
  private = list(
    .train_dt = function(dt, levels, target) {
      browser()
      pv = self$param_set$get_values(tags = "train")
      embed_result = dimRed::embed(dt, "Isomap", knn = pv$knn, ndim = pv$ndim, get_geod = pv$get_geod, .mute = NULL)
      self$state = list(embed_result = embed_result)
      embed_result@data@data
    },
    .predict_dt = function(dt, levels) {
      selectMethod("predict", "dimRedResult")(self$state$embed_result, as.data.frame(dt))@data
    }
  )
)

mlr_pipeops$add("isomap", PipeOpIsomap)

# po = po("isomap", knn = 50, ndim = 2, get_geod = TRUE)

# po$train(list(tsk("iris")))[[1]]$data()
# po$predict(list(tsk("iris")))[[1]]$data()

# po$train(list(tsk("mtcars")))
# po$predict(list(tsk("mtcars")))[[1]]$data()

# emb2 <- embed(iris[1:4], "Isomap", .mute = NULL, knn = 25)
# emb3 <- predict(emb2, iris[1:4])


