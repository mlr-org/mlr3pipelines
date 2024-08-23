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
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as the elements of the list
#' returned from [uwot::umap2].
#' These are in particular:
#' * `embedding` :: `matrix`\cr
#'   Matrix of embedded coordinates.
#' * `scale_info` :: named `list()`\cr
#'   If `scale`is `TRUE`, this gives the scaling attributes (`center`, `scale`, `nzvcols`) of the scaled data.
#' * `search_k` :: `numeric(1)`\cr
#'   Number of nodes searched during the neighbor retrieval. Only used if the `nn_method` is `"annoy"`.
#'   For details, see [uwot::umap2()].
#' * `local_connectivity` :: `numeric(1)`\cr
#'   Used local connectivity – i.e. the number of nearest neighbors that should be
#'   assumed to be connected at a local level. For details, see [uwot::umap2()].
#' * `n_epochs` :: `numeric(1)`\cr
#'   Number of epochs used during the optimization of the embedded coordinates. For details, see [uwot::umap2()].
#' * `alpha` :: `numeric(1)`\cr
#'   Initial learning rate. For details, see [uwot::umap2()].
#' * `negative_sample_rate` :: `numeric(1)`\cr
#'   The number of negative edge/1-simplex samples used per positive edge/1-simplex sample
#'   in optimizing the low dimensional embedding. For details, see [uwot::umap2()].
#' * `method` :: `character(1)`\cr
#'   General method used for dimensionality reduction, is always `"umap"` for this PipeOp.
#' * `a` :: named `numeric(1)`\cr
#'   More specific parameters controlling the embedding. For details, see [uwot::umap2()].
#' * `b` :: named `numeric(1)`\cr
#'   More specific parameters controlling the embedding. For details, see [uwot::umap2()].
#' * `gamma` :: `numeric(1)`\cr
#'   Repulsion strength. Weighting applied to negative samples in low dimensional embedding optimization.
#'   For details, see [uwot::umap2()].
#' * `approx_pow` :: `logical(1)`\cr
#'   If `TRUE`, use an approximation to the power function in the UMAP gradient. For details, see [uwot::umap2()].
#' * `metric` :: named `list()`\cr
#'   Type of distance metric used to find nearest neighbors. For details, see [uwot::umap2()].
#' * `norig_col` :: `integer(1)`\cr
#'   Number of original columns.
#' * `pcg_rand` :: `logical(1)`\cr
#'   `TRUE`, if the PCG random number generator (O'Neill, 2014) was used during optimization.
#'   Otherwise, Tausworthe "taus88" generator was used. For details, see [uwot::umap2()].
#' * `batch` :: `logical(1)`\cr
#'   `TRUE`, if embedding coordinates were updated at the end of each epoch rather
#'   than during the epoch. For details, see [uwot::umap2()].
#' * `opt_args` :: named `list()`\cr
#'   Optimizer parameters, used when `batch = TRUE`. For details, see [uwot::umap2()].
#' * `num_precomputed_nns` :: `numeric(1)`\cr
#'   Number of precomputed nearest neighbors, via `nn_method`.
#' * `min_dist` :: `numeric(1)`\cr
#'   The effective minimum distance between embedded points. For details, see [uwot::umap2()].
#' * `spread` :: `numeric(1)`\cr
#'   The effective scale of embedded points. For details, see [uwot::umap2()].
#' * `binary_edge_weights` :: `logical(1)`\cr
#'   If `TRUE` then edge weights in the input graph were treated as binary (0/1) rather than real valued.
#'   For details, see [uwot::umap2()].
#' * `seed` :: `integer(1)`\cr
#'   Integer seed to use to initialize the random number generator state. For details, see [uwot::umap2()].
#' * `nn_method` :: `any`\cr
#'   Method for finding nearest neighbors. For details, see [uwot::umap2()].
#' * `nn_args` :: `list()`\cr
#'   A list containing additional arguments to pass to the nearest neighbor method. For details, see [uwot::umap2()].
#' * `n_neighbors` :: `numeric(1)`\cr
#'   The size of the neighborhood used for manifold approximation. For details, see [uwot::umap2()].
#' * `nn_index` :: named `list()`\cr
#'   Nearest neighbor index that can be used for transformation of new data points.
#' * `pca_models` :: `list()`\cr
#'   Used PCA models for initialization, `pca` is specified. For details, see [uwot::umap2()].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `n_neighbors` :: `integer(1)`\cr
#'   The size of the neighborhood used for manifold approximation. Default is `15`.
#'   For details, see [uwot::umap2()].
#' * `n_components` :: `integer(1)`\cr
#'   The dimension of the space to embed into. Default is `2`. For details, see [uwot::umap2()].
#' * `metric` :: `character(1)`\cr
#'   Type of distance metric to use to find nearest neighbors. Default is `"euclidean"`.
#'   For details, see [uwot::umap2()].
#' * `n_epochs` :: `integer(1)`\cr
#'   Number of epochs to use during the optimization of the embedded coordinates. Default is `NULL`.
#'   For details, see [uwot::umap2()].
#' * `learning_rate` :: `numeric(1)`\cr
#'   Initial learning rate used in optimization of the coordinates. Default is `1`.
#'   For details, see [uwot::umap2()].
#' * `scale` :: `logical(1)` / `character(1)`\cr
#'   Scaling to apply to the data. If `TRUE`, data is standardized. Default is `FALSE`. For details, see [uwot::umap2()].
#' * `init` :: `character(1)`\cr
#'   Type of initialization for the coordinates. May be set to `"custom"`, in which case the `matrix` of initial
#'   coordinates passed to `init_custom` is used. Default is `"spectral"`. For details, see [uwot::umap2()].
#' * `init_custom` :: `matrix`\cr
#'   Matrix of initial coordinates. Only used, if `init` is `"custom"`.
#' * `init_sdev` :: `character(1)` | `numeric(1)`\cr
#'   Scales each dimension of the initialized coordinates to this standard deviation.
#'   Default is `"range"`. For details, see [uwot::umap2()].
#' * `spread` :: `numeric(1)`\cr
#'   The effective scale of embedded points. Default is `1`. For details, see [uwot::umap2()].
#' * `min_dist` :: `numeric(1)`\cr
#'   The effective minimum distance between embedded points. Default is `0.01`.
#'   For details, see [uwot::umap2()].
#' * `set_op_mix_ratio` :: `numeric(1)`\cr
#'   Interpolate between (fuzzy) union and intersection as the set operation used to
#'   combine local fuzzy simplicial sets to obtain a global fuzzy simplicial sets. Default is `1`.
#'   For details, see [uwot::umap2()].
#' * `local_connectivity` :: `numeric(1)`\cr
#'   The local connectivity required – i.e. the number of nearest neighbors that should be
#'   assumed to be connected at a local level. Default is `1`. For details, see [uwot::umap2()].
#' * `bandwidth` :: `numeric(1)`\cr
#'   The effective bandwidth of the kernel if we view the algorithm as similar to Laplacian Eigenmaps.
#'   Default is `1`. For details, see [uwot::umap2()].
#' * `repulsion_strength` :: `numeric(1)`\cr
#'   Weighting applied to negative samples in low dimensional embedding optimization.
#'   Default is `1`. For details, see [uwot::umap2()].
#' * `negative_sample_rate` :: `numeric(1)`\cr
#'   The number of negative edge/1-simplex samples to use per positive edge/1-simplex sample
#'   in optimizing the low dimensional embedding. Default is `5`. For details, see [uwot::umap2()].
#' * `a` :: `numeric(1)`\cr
#'   More specific parameters controlling the embedding. Default is `NULL`. For details, see [uwot::umap2()].
#' * `b` :: `numeric(1)`\cr
#'   More specific parameters controlling the embedding. Default is `NULL`. For details, see [uwot::umap2()].
#' * `nn_method` :: `character(1)`\cr
#'   Method for finding nearest neighbors. Note that only values compatible with [uwot::umap_transform()] are allowed.
#'   Default is `NULL`. For details, see [uwot::umap2()].
#' * `n_trees` :: `integer(1)`\cr
#'   Number of trees to build when constructing the nearest neighbor index. Default is `50`.
#'   For details, see [uwot::umap2()].
#' * `search_k` :: `integer(1)`\cr
#'   Number of nodes to search during the neighbor retrieval. Only used if the `nn_method` is `"annoy"`.
#'   For details, see [uwot::umap2()].
#' * `approx_pow` :: `logical(1)`\cr
#'   If `TRUE`, use an approximation to the power function in the UMAP gradient. Default is `FALSE`.
#'   For details, see [uwot::umap2()].
#'   `use_supervised` :: `logical(1)`\cr
#'   If `TRUE`, perform supervised dimension reduction. This is done by passing the task's target to [uwot::umap2()]'s `y` argument.
#'   For details, see there. Initialized to `FALSE`.
#' * `target_n_neighbors` :: `integer(1)`\cr
#'   Number of nearest neighbors to use to construct the target simplicial set. Only used when performing supervised dimension reduction.
#'   Default is `n_neighbors`. For details, see [uwot::umap2()].
#' * `target_metric` :: `character(1)`\cr
#'   The metric used to measure distance for the task's target when performing supervised dimension reduction.
#'   For details, see [uwot::umap2()].
#' * `target_weight` :: `numeric(1)`\cr
#'   Weighting factor between data topology and target topology. Only used when performing supervised dimension reduction.
#'   Default is `0.5`. For details, see [uwot::umap2()].
#' * `pca` :: `integer(1)`\cr
#'   Reduce data to this number of columns using PCA. Default is `NULL`.
#'   For details, see [uwot::umap2()].
#' * `pca_center` :: `logical(1)`\cr
#'   If `TRUE`, center the columns of X before carrying out PCA. Default is `TRUE`.
#'   For details, see [uwot::umap2()].
#' * `pcg_rand` :: `logical(1)`\cr
#'   If `TRUE`, use the PCG random number generator (O'Neill, 2014) during optimization.
#'   Otherwise, use the faster (but probably less statistically good) Tausworthe "taus88" generator.
#'   Default is `TRUE`. For details, see [uwot::umap2()].
#' * `fast_sgd` :: `logical(1)`\cr
#'   If `TRUE`, then the following combination of parameters is set:
#'   * `pcg_rand = TRUE`
#'   * `n_sgd_threads = "auto"`
#'   * `approx_pow = TRUE`
#'   Default is `FALSE`. For details, see [uwot::umap2()].
#' * `n_threads` :: `integer(1)`\cr
#'   Number of threads to use. Default is `NULL`. For details, see [uwot::umap2()].
#' * `n_sgd_threads` :: `integer(1)`\cr
#'   Number of threads to use during stochastic gradient descent. Default is `0`.
#'   For details, see [uwot::umap2()].
#' * `grain_size` :: `integer(1)`\cr
#'   The minimum amount of work to do on each thread. Default is `1`.
#'   For details, see [uwot::umap2()].
#' * `verbose` :: `logical(1)`\cr
#'   Should details be printed? Initialized to `FALSE`. For details, see [uwot::umap2()].
#' * `batch` :: `logical(1)`\cr
#'   If `TRUE`, then embedding coordinates are updated at the end of each epoch rather
#'   than during the epoch. Default is `TRUE`. For details, see [uwot::umap2()].
#' * `opt_args` :: named `list()`\cr
#'   A list of optimizer parameters, used when `batch = TRUE`. Default is `NULL`.
#'   For details, see [uwot::umap2()].
#' * `epoch_callback` :: `function`\cr
#'   A function which will be invoked at the end of every epoch. Default is `NULL`.
#'   For details, see [uwot::umap2()].
#' * `pca_method` :: `character(1)`\cr
#'   Method to carry out any PCA dimensionality reduction when the `pca` is specified.
#'   Default is `NULL`. For details, see [uwot::umap2()].
#' * `binary_edge_weights` :: `logical(1)`\cr
#'   If `TRUE` then edge weights in the input graph are treated as binary (0/1) rather than real valued.
#'   Default is `FALSE`. For details, see [uwot::umap2()].
#' * `dens_scale` :: `numeric(1)`\cr
#'   A scaling factor to apply to the density of the input data. Default is `NULL`.
#'   For details, see [uwot::umap2()].
#' * `seed` :: `integer(1)`\cr
#'   Integer seed to use to initialize the random number generator state.
#'   Default is `NULL`. For details, see [uwot::umap2()].
#' * `nn_args` :: named `list()`\cr
#'   A list containing additional arguments to pass to the nearest neighbor method.
#'   Default is `NULL`. For details, see [uwot::umap2()].
#'
#' Additionally, there are several parameters that may be used to overwrite parameter values for prediction:
#' * `search_k_transform` :: `integer(1)`\cr
#'   Number of nodes to search during the neighbor retrieval when predicting.
#'   Only used if `nn_method` is `"annoy"`. If `NULL`, `search_k` is used instead. Default is `NULL`. For details, see [uwot::umap_transform()].
#' * `n_epochs_transform` :: `integer(1)`\cr
#'   Number of epochs used during the optimization of the embedded coordinates when predicting.
#'   If `NULL`, `n_epochs` is used instead. Default is `NULL`. For details, see [uwot::umap_transform()].
#' * `init_transform` :: `character(1)`\cr
#'   Type of initialization for the coordinates when predicting. May be set to `"custom"`, in which case the `matrix` of initial
#'   coordinates passed to `init_transform_custom` is used. Default is `"weighted"`. For details, see [uwot::umap_transform()].
#' * `init_transform_custom` :: `matrix`\cr
#'   Matrix of initial coordinates when predicting Only used, if `init_transform` is `"custom"`.
#' * `batch_transform` :: `logical(1)`\cr
#'   If `TRUE`, embedding coordinates are updated at the end of each epoch rather than during the epoch when predicting.
#'   If `NULL`, `batch` is used instead. Default is `FALSE`. For details, see [uwot::umap_transform()].
#' * `learning_rate_transform` :: `numeric(1)`\cr
#'   Initial learning rate used in optimization of the coordinates when predicting.
#'   If `NULL`, `learning_rate` is used instead. Default is `NULL`. For details, see [uwot::umap_transform()].
#' * `epoch_callback_transform` :: `function`\cr
#'   A function which will be invoked at the end of every epoch when predicting.
#'   Default is `NULL`. For details, see [uwot::umap_transform()].
#'
#' @section Internals:
#' Uses the [umap2()][uwot::umap2] function.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @references
#' `r format_bib("mcinnes_2018")`
#'
#' @examples
#' \dontshow{ if (requireNamespace("uwot")) \{ }
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("umap")
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' \dontshow{ \} }
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpUMAP = R6Class("PipeOpUMAP",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "umap", param_vals = list()) {
      ps = ps(
        n_neighbors = p_int(lower = 1L, default = 15L, tags = c("train", "umap")),
        n_components = p_int(lower = 1L, default = 2L, tags = c("train", "umap")),
        metric = p_fct(
          levels = c(
            "euclidean", "cosine", "manhattan", "hamming", "correlation",
            "braycurtis", "canberra", "chebyshev", "dice", "hellinger", "jaccard",
            "jensenshannon", "kulsinski", "rogerstanimoto", "russellrao", "sokalmichener",
            "sokalsneath", "spearmanr", "symmetrickl", "tsss", "yule"
          ),
          default = "euclidean",
          tags = c("train", "umap")
        ),
        n_epochs = p_int(lower = 1L, default = NULL, special_vals = list(NULL), tags = c("train", "umap")),
        learning_rate = p_dbl(lower = 0, default = 1, tags = c("train", "umap")),
        scale = p_fct(
          levels = c("none", "scale", "maxabs", "range", "colrange"),
          special_vals = list(FALSE, NULL, "Z", TRUE),
          default = FALSE,
          tags = c("train", "umap")
        ),
        init = p_fct(
          levels = c("spectral", "normlaplacian", "random", "lvrandom", "laplacian", "pca", "spca", "agspectral"),
          special_vals = list("custom"),
          default = "spectral",
          tags = c("train", "umap")
        ),
        init_custom = p_uty(custom_check = check_matrix, tags = "train", depends = quote(init == "custom")),
        init_sdev = p_dbl(default = "range", special_vals = list("range"), tags = c("train", "umap")),
        spread = p_dbl(default = 1, tags = c("train", "umap")),
        min_dist = p_dbl(default = 0.01, tags = c("train", "umap")),
        set_op_mix_ratio = p_dbl(lower = 0, upper = 1, default = 1, tags = c("train", "umap")),
        local_connectivity = p_dbl(lower = 1, default = 1, tags = c("train", "umap")),
        bandwidth = p_dbl(default = 1, tags = c("train", "umap")),
        repulsion_strength = p_dbl(default = 1, tags = c("train", "umap")),
        negative_sample_rate = p_dbl(default = 5, tags = c("train", "umap")),
        a = p_dbl(default = NULL, special_vals = list(NULL), tags = c("train", "umap")),
        b = p_dbl(default = NULL, special_vals = list(NULL), tags = c("train", "umap")),
        nn_method = p_fct(levels = c("annoy", "hnsw", "nndescent"), default = NULL, special_vals = list(NULL), tags = c("train", "umap")),
        n_trees = p_int(lower = 1L, default = 50L, tags = c("train", "umap"), depends = quote(nn_method == "annoy")),
        search_k = p_int(tags = c("train", "umap"), depends = quote(nn_method == "annoy")),
        # approx_pow is only used if dens_scale is non-NULL
        approx_pow = p_lgl(default = FALSE, tags = c("train", "umap")),
        use_supervised = p_lgl(default = FALSE, tags = c("train")),
        target_n_neighbors = p_int(tags = c("train", "umap"), depends = quote(use_supervised == TRUE)),
        target_metric =  p_fct(
          levels = c(
            "euclidean", "cosine", "manhattan", "hamming", "correlation",
            "braycurtis", "canberra", "chebyshev", "dice", "hellinger", "jaccard",
            "jensenshannon", "kulsinski", "rogerstanimoto", "russellrao", "sokalmichener",
            "sokalsneath", "spearmanr", "symmetrickl", "tsss", "yule"
          ),
          default = "euclidean",
          tags = c("train", "umap"),
          depends = quote(use_supervised == TRUE)
        ),
        target_weight = p_dbl(lower = 0, upper = 1, default = 0.5, tags = c("train", "umap"), depends = quote(use_supervised == TRUE)),
        # pca is ignored if metric is "hamming"
        pca = p_int(lower = 1L, default = NULL, special_vals = list(NULL), tags = c("train", "umap"),
                    depends = quote(metric %in% c(
                      "euclidean", "cosine", "manhattan", "correlation",
                      "braycurtis", "canberra", "chebyshev", "dice", "hellinger", "jaccard",
                      "jensenshannon", "kulsinski", "rogerstanimoto", "russellrao", "sokalmichener",
                      "sokalsneath", "spearmanr", "symmetrickl", "tsss", "yule"
                    ))),
        # pca_center is only used if pca is specified
        pca_center = p_lgl(default = TRUE, tags = c("train", "umap")),
        pcg_rand = p_lgl(default = TRUE, tags = c("train", "umap")),
        fast_sgd = p_lgl(default = FALSE, tags = c("train", "umap")),
        n_threads = p_int(lower = 1L, default = NULL, special_vals = list(NULL), tags = c("train", "predict", "umap")),
        n_sgd_threads = p_int(lower = 0L, default = 0L, special_vals = list("auto"), tags = c("train", "predict", "umap")),
        grain_size = p_int(lower = 1L, default = 1L, tags = c("train", "predict", "umap")),
        verbose = p_lgl(default = TRUE, tags = c("train", "predict", "umap")),
        batch = p_lgl(default = TRUE, tags = c("train", "umap")),
        opt_args = p_uty(
          default = NULL,
          tags = c("train", "umap"),
          custom_check = crate(function(x) check_list(x, types = c("numeric", "character"), min.len = 1, max.len = 5,
                                                      names = "unique", null.ok = TRUE)),
          depends = quote(batch == TRUE)
        ),
        epoch_callback = p_uty(
          default = NULL,
          tags = c("train", "umap"),
          custom_check = crate(function(x) check_function(x, args = c("epochs", "n_epochs", "coords"), null.ok = TRUE))
        ),
        # pca_method is only used if pca is specified
        pca_method = p_fct(c("irlba", "rsvd", "bigstatsr", "svd", "auto"), default = NULL, special_vals = list(NULL), tags = c("train", "umap")),
        binary_edge_weights = p_lgl(default = FALSE, tags = c("train", "umap")),
        dens_scale = p_dbl(lower = 0, upper = 1, default = NULL, special_vals = list(NULL), tags = c("train", "umap")),
        seed = p_int(default = NULL, special_vals = list(NULL), tags = c("train", "umap")),
        nn_args = p_uty(
          default = NULL,
          tags = c("train", "umap"),
          custom_check = crate(function(x) check_list(x, types = c("integer", "numeric", "character"),
                                                      min.len = 1, max.len = 8, names = "unique", null.ok = TRUE))
        ),
        # Parameters that are passed to umap_transform to overwrite parameters from training for prediction
        search_k_transform = p_int(default = NULL, special_vals = list(NULL), tags = c("predict", "overwrite"), depends = quote(nn_method == "annoy")),
        n_epochs_transform = p_int(lower = 1L, default = NULL, special_vals = list(NULL), tags = c("predict", "overwrite")),
        init_transform = p_fct(levels = c("weighted", "average"), special_vals = list("custom"), default = "weighted", tags = c("predict", "overwrite")),
        init_transform_custom = p_uty(custom_check = check_matrix, tags = "predict", depends = quote(init_transform == "custom")),
        batch_transform = p_lgl(default = FALSE, special_vals = list(NULL), tags = c("predict", "overwrite")),
        learning_rate_transform = p_dbl(default = NULL, special_vals = list(NULL), tags = c("predict", "overwrite")),
        epoch_callback_transform = p_uty(
          default = NULL,
          tags = c("predict", "overwrite"),
          custom_check = crate(function(x) check_function(x, args = c("epochs", "n_epochs", "coords", "fixed_coords"), null.ok = TRUE))
        )
      )
      ps$values = list(verbose = FALSE, use_supervised = FALSE)

      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "uwot", feature_types = c("numeric", "integer"))
    }
  ),
  private = list(
    .train_dt = function(dt, levels, target) {
      pv = self$param_set$values
      pv_args = self$param_set$get_values(tags = c("umap", "train"))
      # Indicate that umap2() should return the full model which we need for prediction
      pv_args = insert_named(pv_args, list(ret_model = TRUE))
      # Use target for supervised dimension reduction when specified
      if (!is.null(pv$use_supervised) && pv$use_supervised) {
        pv_args = insert_named(pv_args, list(y = target))
      }
      # Use matrix passed to init_custom for initialization when specified
      if (!is.null(pv$init) && pv$init == "custom") {
        pv_args = insert_named(pv_args, list(init = pv$init_custom))
      }
      umap = invoke(uwot::umap2, dt, .args = pv_args)
      self$state = umap
      umap$embedding
    },

    .predict_dt = function(dt, levels) {
      pv = self$param_set$values
      pv_args = self$param_set$get_values(tags = c("umap", "predict"))
      # Get overwriting params and rename them to the correct argument names for uwot::umap_transform()
      overwrite_pv_args = self$param_set$get_values(tags = c("overwrite", "predict"))
      names(overwrite_pv_args) <- sub("_transform$", "", names(overwrite_pv_args))
      pv_args = insert_named(pv_args, overwrite_pv_args)
      # Use matrix passed to init_transform_custom for initialization when specified
      if (!is.null(pv$init_transform) && pv$init_transform == "custom") {
        pv_args = insert_named(pv_args, list(init = pv$init_transform_custom))
      }
      invoke(uwot::umap_transform, dt, self$state, .args = pv_args)
    },

    # We need to overload deep_clone since state$nn_index$ann is a RefClass if nn_method is "annoy" or "hnsw"
    deep_clone = function(name, value) {
      if (name == "state" && "NO_OP" %nin% class(value)) {
        if (!is.null(value$nn_index)) {
          if (methods::is(value$nn_index$ann, "envRefClass")) {
            state = value
            state$nn_index$ann = value$nn_index$ann$copy()
            state
          } else {
            super$deep_clone(name, value)
          }
        } else {
          super$deep_clone(name, value)
        }
      } else {
        super$deep_clone(name, value)
      }
    }
  )
)

mlr_pipeops$add("umap", PipeOpUMAP)
