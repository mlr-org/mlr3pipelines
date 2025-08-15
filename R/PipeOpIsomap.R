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
      #ps$values = list(k = 50, ndim = 2, eps = 0)
      ps = ps(
        k = p_int(default = 50, lower = 1, upper = Inf, tags = "train"), # tag isomap?
        ndim = p_int(default = 2, lower = 1, upper = Inf, tags = "train"), #tag isomap?
        eps = p_dbl(default = 0, tags = "train")) # tag isomap?
      super$initialize(id = id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"))
      if(length(self$param_set$values) == 0) self$param_set$values = self$param_set$default
      private$.get_geod = get_geod
      private$.keep_org_data = keep_org_data
      private$.diag = diag
    }
  ),
  active = list(),
  private = list(
    .get_geod = NULL,
    .keep_org_data = NULL,
    .diag = NULL,
    .make_knn_graph = function(x) {
      pv = self$param_set$get_values(tags = "train")
      INF_VAL = 1.340781e+15
      NA_IDX  = 0
      ## select parameters
      M = nrow(x)
      if (pv$eps == 0) searchtype = "standard" else searchtype = "priority"
      ## RANN::nn2 returns the points in data with respect to query
      ## e.g. the rows in the output are the points in query and the
      ## columns the points in data.
      nn2res = RANN::nn2(data = x, query = x, k = pv$k + 1, treetype = "kd",
                          searchtype = searchtype, eps = pv$eps)
      ## create graph: the first ny nodes will be y, the last nx nodes
      ## will be x, if x != y
      g = igraph::make_empty_graph(M, directed = TRUE)
      g[from = if (private$.diag) rep(seq_len(M), times = pv$k + 1)
        else      rep(seq_len(M), times = pv$k),
        to   = if (private$.diag) as.vector(nn2res$nn.idx)
        else      as.vector(nn2res$nn.idx[, -1]),
        attr = "weight"] =
        if (private$.diag)  as.vector(nn2res$nn.dists)
      else as.vector(nn2res$nn.dists[, -1])
      igraph::as_undirected(g, mode = "collapse", edge.attr.comb = "first")
    },
    .train_dt = function(dt, levels, target) {
      browser()
      pv = self$param_set$get_values(tags = "train")
      knn_graph = private$.make_knn_graph(dt)
      geodist = igraph::distances(knn_graph, algorithm = "dijkstra")
      k = geodist ^ 2
      k = .Call(stats:::C_DoubleCentre, k)
      k = - k / 2
      ## TODO: explicit symmetrizing
      ## TODO: return eigenvectors?
      e = RSpectra::eigs_sym(k, pv$ndim, which = "LA",
                              opts = list(retvec = TRUE))
      e_values = e$values
      e_vectors = e$vectors
      neig = sum(e_values > 0)
      if (neig < pv$ndim) {
        warning("Isomap: eigenvalues < 0, returning less dimensions!")
        e_values = e_values[seq_len(neig)]
        e_vectors = e_vectors[, seq_len(neig), drop = FALSE]
      }
      e_vectors = e_vectors * rep(sqrt(e_values), each = nrow(e_vectors))
      colnames(e_vectors) = paste("iso", seq_len(neig))
      self$state = list(geodist = geodist, e_vectors = e_vectors, e_values = e_values, orgdata = dt, target = target)
      dt
    },
    .predict_dt = function(dt, levels) {
      browser()
      pv = self$param_set$get_values(tags = "train")
      if (ncol(self$state$orgdata) != ncol(dt))
        stop("x must have the same number of dimensions as the original data")
      nindata = nrow(dt)
      norg = nrow(self$state$orgdata)
      lknng = private$.make_knn_graph(rbind(dt, self$state$orgdata))
      lgeodist = igraph::distances(lknng,
                                    seq_len(nindata),
                                    nindata + seq_len(norg))
      dammu = sweep(lgeodist ^ 2, 2, colMeans(self$state$geodist ^ 2), "-")
      Lsharp = sweep(self$state$e_vectors, 2, self$state$e_values, "/")
      out = -0.5 * (dammu %*% Lsharp)
      dt
    }
  )
)

mlr_pipeops$add("isomap", PipeOpIsomap)

