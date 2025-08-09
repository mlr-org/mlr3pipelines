PipeOpIsomap = R6Class("PipeOpIsomap",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "isomap", param_vals = list(), get_geod = FALSE, keep_org_data = TRUE, diag = FALSE) {
      ps = ps(
        k = p_int(lower = 1, upper = Inf, tags = c("train", "isomap")),
        ndim = p_int(lower = 1, upper = Inf, tags = c("train", "isomap")),
        eps = p_dbl(default = 0, tags = c("train", "isomap")))
      ps$values = list(k = 50, ndim = 2, eps = 0)
      super$initialize(id = id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"))
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
      INF_VAL <- 1.340781e+15
      NA_IDX  <- 0
      ## select parameters
      M <- nrow(x)
      searchtype <- if (pv$eps == 0) "standard" else "priority"
      ## RANN::nn2 returns the points in data with respect to query
      ## e.g. the rows in the output are the points in query and the
      ## columns the points in data.
      nn2res <- RANN::nn2(data = x, query = x, k = pv$k + 1, treetype = "kd",
                          searchtype = searchtype, eps = pv$eps)
      ## create graph: the first ny nodes will be y, the last nx nodes
      ## will be x, if x != y
      g <- igraph::make_empty_graph(M, directed = TRUE)
      g[from = if (private$.diag) rep(seq_len(M), times = pv$k + 1)
        else      rep(seq_len(M), times = pv$k),
        to   = if (private$.diag) as.vector(nn2res$nn.idx)
        else      as.vector(nn2res$nn.idx[, -1]),
        attr = "weight"] <-
        if (private$.diag)  as.vector(nn2res$nn.dists)
      else as.vector(nn2res$nn.dists[, -1])
      return(igraph::as_undirected(g, mode = "collapse", edge.attr.comb = "first"))
    },
    .train_dt = function(dt, ndim, get_geod, keep_org_data) {
      browser()
      pv = self$param_set$get_values(tags = "train")
      knn_graph = private$.make_knn_graph(dt)
      geodist = igraph::distances(knn_graph, algorithm = "dijkstra")
      k <- geodist ^ 2
      k <- .Call(stats:::C_DoubleCentre, k)
      k <- - k / 2
      ## TODO: explicit symmetrizing
      ## TODO: return eigenvectors?
      e <- RSpectra::eigs_sym(k, pv$ndim, which = "LA",
                              opts = list(retvec = TRUE))
      e_values <- e$values
      e_vectors <- e$vectors
      neig <- sum(e_values > 0)
      if (neig < pv$ndim) {
        warning("Isomap: eigenvalues < 0, returning less dimensions!")
        e_values <- e_values[seq_len(neig)]
        e_vectors <- e_vectors[, seq_len(neig), drop = FALSE]
      }

      e_vectors <- e_vectors * rep(sqrt(e_values), each = nrow(e_vectors))
      colnames(e_vectors) <- paste("iso", seq_len(neig))
      self$state = list(geodist = geodist, e_vectors = e_vectors, e_values = e_values)
      dt
    }
    #.predict_dt = function() {}
  )
)

mlr_pipeops$add("isomap", PipeOpIsomap)
po = PipeOpIsomap$new("isomap")

# if (pars$eps) pars$eps$value =

makeKNNgraph <- function (x, k, eps = 0, diag = FALSE){
  INF_VAL <- 1.340781e+15
  NA_IDX  <- 0
  ## select parameters
  M <- nrow(x)
  searchtype <- if (eps == 0) "standard" else "priority"
  ## RANN::nn2 returns the points in data with respect to query
  ## e.g. the rows in the output are the points in query and the
  ## columns the points in data.
  nn2res <- RANN::nn2(data = x, query = x, k = k + 1, treetype = "kd",
                      searchtype = searchtype, eps = eps)
  ## create graph: the first ny nodes will be y, the last nx nodes
  ## will be x, if x != y
  g <- igraph::make_empty_graph(M, directed = TRUE)
  g[from = if (diag) rep(seq_len(M), times = k + 1)
    else      rep(seq_len(M), times = k),
    to   = if (diag) as.vector(nn2res$nn.idx)
    else      as.vector(nn2res$nn.idx[, -1]),
    attr = "weight"] <-
    if (diag)  as.vector(nn2res$nn.dists)
  else as.vector(nn2res$nn.dists[, -1])
  return(igraph::as_undirected(g, mode = "collapse", edge.attr.comb = "first"))
}
