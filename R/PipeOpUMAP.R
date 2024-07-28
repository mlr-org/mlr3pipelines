#' @title Uniform Manifold Approximation and Projection (UMAP)
#'
#' @usage NULL
#' @name mlr_pipeops_umap
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Carry out dimensionality reduction of a dataset using the Uniform Manifold Approximation and Projection (UMAP).
#' See [uwot::umap2()] for details.
#'
#' @section Construction:
#' ```
#' PipeOpUMAP$new(id = "umap", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"umap"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features replaced by their principal components.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as the elements of the class [stats::prcomp],
#' with the exception of the `$x` slot. These are in particular:
#' * `sdev` :: `numeric`\cr
#'   The standard deviations of the principal components.
#' * `rotation` :: `matrix`\cr
#'   The matrix of variable loadings.
#' * `center` :: `numeric` | `logical(1)`\cr
#'   The centering used, or `FALSE`.
#' * `scale` :: `numeric` | `logical(1)`\cr
#'   The scaling used, or `FALSE`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `n_neighbors` :: `integer(1)`\cr
#'   Blah
#' * `n_components` :: `integer(1)`\cr
#'   Blah
#' * `metric` :: `character(1)`\cr
#'   Blah
#' * `n_epochs` :: `integer(1)`\cr
#'   Blah
#' * `learning_rate` :: `numeric(1)`\cr
#'   Blah
#' * `init` :: `character(1)`\cr
#'   Blah
#' * `init_sdev` :: `character(1)`\cr
#'   Blah
#' * `spread` :: `character(1)`\cr
#'   Blah
#' * `min_dist` :: `character(1)`\cr
#'   Blah
#' * `set_op_mix_ratio` :: `character(1)`\cr
#'   Blah
#' * `local_connectivity` :: `character(1)`\cr
#'   Blah
#' * `bandwidth` :: `character(1)`\cr
#'   Blah
#' * `repulsion_strength` :: `character(1)`\cr
#'   Blah
#' * `a` :: `character(1)`\cr
#'   Blah
#' * `b` :: `character(1)`\cr
#'   Blah
#' * `nn_method` :: `character(1)`\cr
#'   Blah
#' * `n_trees` :: `character(1)`\cr
#'   Blah
#' * `search_k` :: `character(1)`\cr
#'   Blah
#' * `approx_pow` :: `character(1)`\cr
#'   Blah
#' * `y` :: `character(1)`\cr
#'   Blah
#' * `target_n_neighbors` :: `character(1)`\cr
#'   Blah
#' * `target_metric` :: `character(1)`\cr
#'   Blah
#' * `target_weight` :: `character(1)`\cr
#'   Blah
#' * `pca` :: `character(1)`\cr
#'   Blah
#' * `pca_center` :: `character(1)`\cr
#'   Blah
#' * `pca_rand` :: `character(1)`\cr
#'   Blah
#' * `fast_sgd` :: `character(1)`\cr
#'   Blah
#' * `n_threads` :: `character(1)`\cr
#'   Blah
#' * `n_sgd_threads` :: `character(1)`\cr
#'   Blah
#' * `grain_size` :: `character(1)`\cr
#'   Blah
#' * `verbose` :: `character(1)`\cr
#'   Blah
#' * `batch` :: `character(1)`\cr
#'   Blah
#' * `opt_args` :: `character(1)`\cr
#'   Blah
#' * `epoch_callback` :: `character(1)`\cr
#'   Blah
#' * `pca_method` :: `character(1)`\cr
#'   Blah
#' * `binary_edge_weights` :: `character(1)`\cr
#'   Blah
#' * `dens_scale` :: `character(1)`\cr
#'   Blah
#' * `seed` :: `character(1)`\cr
#'   Blah
#' * `nn_args` :: `character(1)`\cr
#'   Blah
#'
#' @section Internals:
#' Uses the [`umap()`][uwot::umap] function.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("umap")
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpUMAP = R6Class("PipeOpUMAP",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "umap", param_vals = list()) {
      ps = ps(
        n_neighbors = p_int(2L, 100L, default = 15L, tags = c("train", "umap")),
        n_components = p_int(1L, 100L, default = 2L, tags = c("train", "umap")),
        metric = p_fct(
          c("euclidean", "cosine", "manhattan", "hamming", "correlation", "categorical"),
          default = "euclidean",
          tags = c("train", "umap"),
          depends = quote(nn_method == "hnsw")
        ),
        n_epochs = p_int(1L, default = NULL, special_vals = list(NULL), tags = c("train", "umap")),
        learning_rate = p_dbl(0, default = 1, tags = c("train", "umap")),
        scale = p_lgl(default = FALSE, special_vals = list("none", "Z", "maxabs", "range", "colrange", NULL), tags = c("train", "umap")),
        init = p_uty(
          default = "spectral",
          tags = c("train", "umap"),
          custom_check = crate(function(x) {
            choices = c("spectral", "normlaplacian", "random", "lvrandom", "laplacian", "pca", "spca", "agspectral")
            check_choice(x, choices) %check||% check_matrix(x)
          })
        ),
        init_sdev = p_uty(default = "range", tags = c("train", "umap")),
        spread = p_dbl(default = 1, tags = c("train", "umap")),
        min_dist = p_dbl(default = 0.01, tags = c("train", "umap")),
        set_op_mix_ratio = p_dbl(0, 1, default = 1, tags = c("train", "umap")),
        local_connectivity = p_dbl(1, default = 1L, tags = c("train", "umap")),
        bandwidth = p_dbl(default = 1, tags = c("train", "umap")),
        repulsion_strength = p_dbl(default = 1, tags = c("train", "umap")),
        negative_sample_rate = p_dbl(default = 5L, tags = c("train", "umap")),
        a = p_uty(default = NULL, tags = c("train", "umap")),
        b = p_uty(default = NULL, tags = c("train", "umap")),
        nn_method = p_uty(
          default = NULL,
          tags = c("train", "umap"),
          custom_check = crate(function(x) {
            check_choice(x, c("fnn", "annoy", "hnsw", "nndescent"), null.ok = TRUE) %check||%
              check_list(x, types = "matrix", len = 2L, names = "idx", "dist") %check||%
              check_class(x, "dgCMatrix")
          })
        ),
        n_trees = p_int(10L, 100L, default = 50L, tags = c("train", "umap")),
        search_k = p_int(tags = c("train", "umap")),
        approx_pow = p_lgl(default = FALSE, tags = c("train", "umap")),
        y = p_uty(default = NULL, tags = c("train", "umap")),
        target_n_neighbors = p_int(tags = c("train", "umap")),
        target_metric = p_fct(c("euclidean", "cosine", "correlation"), default = "euclidean", tags = c("train", "umap")),
        target_weight = p_dbl(0, 1, default = 0.5, tags = c("train", "umap")),
        pca = p_int(1, default = NULL, special_vals = list(NULL), tags = c("train", "umap")),
        pca_center = p_lgl(default = TRUE, tags = c("train", "umap")),
        pca_rand = p_lgl(default = TRUE, tags = c("train", "umap")),
        fast_sgd = p_lgl(default = FALSE, tags = c("train", "umap")),
        n_threads = p_int(1L, default = NULL, special_vals = list(NULL), tags = c("train", "umap")),
        n_sgd_threads = p_int(0L, default = 0L, special_vals = list("auto"), tags = c("train", "umap")),
        grain_size = p_int(1L, default = 1L, tags = c("train", "umap")),
        verbose = p_lgl(default = TRUE, tags = c("train", "umap")),
        batch = p_lgl(default = FALSE, tags = c("train", "umap")),
        opt_args = p_uty(default = NULL, tags = c("train", "umap"), custom_check = crate(function(x) check_list(x, null.ok = TRUE))),
        epoch_callback = p_uty(default = NULL, tags = c("train", "umap"), custom_check = check_function_or_null),
        pca_method = p_fct(c("irlba", "rsvd", "bigstatsr", "svd", "auto"), default = NULL, special_vals = list(NULL), tags = c("train", "umap")),
        binary_edge_weights = p_lgl(default = FALSE, tags = c("train", "umap")),
        dens_scale = p_dbl(0, 1, default = NULL, special_vals = list(NULL), tags = c("train", "umap")),
        seed = p_int(default = NULL, special_vals = list(NULL), tags = c("train", "umap")),
        nn_args = p_uty(default = NULL, tags = c("train", "umap"), custom_check = crate(function(x) check_list(x, null.ok = TRUE)))
      )
      ps$set_values(verbose = FALSE)

      super$initialize(id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(
    .train_dt = function(dt, levels, target) {
      params = insert_named(self$param_set$get_values(tags = "umap"), list(ret_model = TRUE))
      umap = invoke(uwot::umap2, dt, .args = params)
      self$state = umap
      umap$embedding
    },

    .predict_dt = function(dt, levels) {
      invoke(uwot::umap_transform, dt, self$state)
    }
  )
)

mlr_pipeops$add("umap", PipeOpUMAP)
