#' @title Algorithm for Dimensionality Reduction
#'
#' @usage
#' @name mlr_pipeops_isomap
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]
#'
#' @description
#'
#'
#' [additional information]
#'
#' @section Construction:
#' ```
#' PipeOpIsomap$new(id = "isomap", ...)
#' ```
#' * `ìd` :: `character(1)`\cr
#'   Identifier of resulting object, default "isomap"
#'
#' @section Input and Output Channels:
#'
#' @section State:
#'
#' @section Parameters:
#'
#' @section Internals:
#'
#' @section Fields:
#'
#' @section Methods:
#'
#' @examples
#'
#'
#' @references
#'
#' @family PipeOps
#' @template
#' @include
#' @export
#'
#'

PipeOpIsomap = R6Class("PipeOpIsomap",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "isomap", param_vals = list(), get_geod = FALSE, keep_org_data = TRUE, diag = FALSE) {
      ps = ps(
        knn = p_int(default = 50, lower = 1, upper = Inf, tags = "train"), # tag isomap?
        ndim = p_int(default = 2, lower = 1, upper = Inf, tags = "train"), #tag isomap?
        get_geod = p_lgl(default = FALSE, tags = "train"),
        .mute = p_uty(default = NULL, tags = "train")
      )
      ps$values = list(knn = 50, ndim = 2)
      super$initialize(id = id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(
    .train_dt = function(dt, levels, target) {
      #browser()
      pv = self$param_set$get_values(tags = "train")
      embed_result = dimRed::embed(dt, "Isomap", knn = pv$knn, ndim = pv$ndim, .mute = NULL)
      self$state = list(embed_result = embed_result)
      embed_result@data@data
    },
    .predict_dt = function(dt, levels) {
      #browser()
      pv = self$param_set$get_values(tags = "train")
      predict(self$state$embed_result, dt)
    }
  )
)

mlr_pipeops$add("isomap", PipeOpIsomap)

#po = po("isomap", knn = 40, ndim = 2)
#po$train(list(tsk("iris")))
#po$predict(list(tsk("iris")))

#samp <- sample(nrow(dat), size = 70)
#emb2 <- embed(dat[samp,], "Isomap", .mute = NULL, knn = 30)
#emb3 <- predict(emb2, dat[-samp,])
