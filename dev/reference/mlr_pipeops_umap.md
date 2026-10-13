# Uniform Manifold Approximation and Projection (UMAP)

Carry out dimensionality reduction using Uniform Manifold Approximation
and Projection (UMAP). Training uses
[`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html)
and prediction embeds new observations using
[`uwot::umap_transform()`](https://jlmelville.github.io/uwot/reference/umap_transform.html).

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Construction

    PipeOpUMAP$new(id = "umap", param_vals = list())

- `id` :: `character(1)`  
  Identifier of resulting object, default `"umap"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md).

The output is the input
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) with all affected
numeric and integer features replaced by their UMAP embedding
coordinates.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
as well as the elements of the model returned from
[`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).
The model's internal structure is determined by `uwot` and may change
between versions. These are in particular:

- `embedding` :: `matrix`  
  Matrix of embedded coordinates.

- `scale_info` :: named [`list()`](https://rdrr.io/r/base/list.html)  
  If `scale` is `TRUE`, this gives the scaling attributes (`center`,
  `scale`, `nzvcols`) of the scaled data.

- `search_k` :: `numeric(1)`  
  Number of nodes searched during the neighbor retrieval. Only used if
  the `nn_method` is `"annoy"`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `local_connectivity` :: `numeric(1)`  
  Used local connectivity – i.e. the number of nearest neighbors that
  should be assumed to be connected at a local level. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `n_epochs` :: `numeric(1)`  
  Number of epochs used during the optimization of the embedded
  coordinates. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `alpha` :: `numeric(1)`  
  Initial learning rate. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `negative_sample_rate` :: `numeric(1)`  
  The number of negative edge/1-simplex samples used per positive
  edge/1-simplex sample in optimizing the low dimensional embedding. For
  details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `method` :: `character(1)`  
  Method used for dimensionality reduction. Usually `"umap"`;
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html)
  uses `"tumap"` when `a = 1`, `b = 1`, and `dens_scale = NULL`.

- `a` :: named `numeric(1)`  
  More specific parameters controlling the embedding. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `b` :: named `numeric(1)`  
  More specific parameters controlling the embedding. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `gamma` :: `numeric(1)`  
  Repulsion strength. Weighting applied to negative samples in low
  dimensional embedding optimization. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `approx_pow` :: `logical(1)`  
  If `TRUE`, use an approximation to the power function in the UMAP
  gradient. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `metric` :: named [`list()`](https://rdrr.io/r/base/list.html)  
  Type of distance metric used to find nearest neighbors. For details,
  see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `norig_col` :: `integer(1)`  
  Number of original columns.

- `pcg_rand` :: `logical(1)`  
  Value of the legacy `pcg_rand` parameter. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `batch` :: `logical(1)`  
  `TRUE`, if embedding coordinates were updated at the end of each epoch
  rather than during the epoch. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `opt_args` :: named [`list()`](https://rdrr.io/r/base/list.html)  
  Optimizer parameters, used when `batch = TRUE`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `num_precomputed_nns` :: `numeric(1)`  
  Number of precomputed nearest neighbors, via `nn_method`.

- `min_dist` :: `numeric(1)`  
  The effective minimum distance between embedded points. For details,
  see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `spread` :: `numeric(1)`  
  The effective scale of embedded points. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `binary_edge_weights` :: `logical(1)`  
  If `TRUE` then edge weights in the input graph were treated as binary
  (0/1) rather than real valued. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `seed` :: `integer(1)`  
  Integer seed to use to initialize the random number generator state.
  For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `nn_method` :: `any`  
  Method for finding nearest neighbors. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `nn_args` :: [`list()`](https://rdrr.io/r/base/list.html)  
  A list containing additional arguments to pass to the nearest neighbor
  method. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `n_neighbors` :: `numeric(1)`  
  The size of the neighborhood used for manifold approximation. For
  details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `nn_index` :: named [`list()`](https://rdrr.io/r/base/list.html)  
  Nearest neighbor index that can be used for transformation of new data
  points. Not copied by `$clone(deep = TRUE)`, see below.

- `pca_models` :: [`list()`](https://rdrr.io/r/base/list.html)  
  PCA models used for preprocessing when `pca` is specified. For
  details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

If `nn_method` is `"annoy"` or `"hnsw"`, `nn_index$ann` is an external
C++ object that cannot be copied in memory. Therefore,
`$clone(deep = TRUE)` does not copy it, and the clone shares the nearest
neighbor index with the original. This is safe for regular use, because
prediction only reads the index and training replaces the `$state` as a
whole. However, modifying the shared index directly, e.g. with
[`uwot::unload_uwot()`](https://jlmelville.github.io/uwot/reference/unload_uwot.html),
affects all clones. This is the same behavior as for the `$model` of a
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html), which
`$clone(deep = TRUE)` does not copy either.

## Parameters

The parameters are the parameters inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
as well as:

- `n_neighbors` :: `integer(1)`  
  The size of the neighborhood used for manifold approximation. Default
  is `15`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `n_components` :: `integer(1)`  
  The dimension of the space to embed into. Default is `2`. For details,
  see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `metric` :: `character(1)`  
  Type of distance metric to use to find nearest neighbors. Default is
  `"euclidean"`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `n_epochs` :: `integer(1)`  
  Number of epochs to use during the optimization of the embedded
  coordinates. Default is `NULL`, which
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html)
  resolves to `500`. Set to `0` to return the initialized coordinates
  without optimization. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `learning_rate` :: `numeric(1)`  
  Initial learning rate used in optimization of the coordinates. Default
  is `1`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `scale` :: `logical(1)` / `character(1)`  
  Scaling to apply to the data. If `TRUE`, data is standardized. Default
  is `FALSE`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `init` :: `character(1)`  
  Type of initialization for the coordinates. May be set to `"custom"`,
  in which case the `matrix` of initial coordinates passed to
  `init_custom` is used. Default is `"spectral"`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `init_custom` :: `matrix`  
  Matrix of initial coordinates. Only used, if `init` is `"custom"`.

- `init_sdev` :: `character(1)` \| `numeric(1)`  
  Scales each dimension of the initialized coordinates to this standard
  deviation. Default is `"range"`, which scales each column to the range
  0 to 10. Set to `NULL` to disable scaling of the initial coordinates.
  For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `spread` :: `numeric(1)`  
  The effective scale of embedded points. Default is `1`. For details,
  see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `min_dist` :: `numeric(1)`  
  The effective minimum distance between embedded points. Default is
  `0.1`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `set_op_mix_ratio` :: `numeric(1)`  
  Interpolate between (fuzzy) union and intersection as the set
  operation used to combine local fuzzy simplicial sets to obtain a
  global fuzzy simplicial sets. Default is `1`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `local_connectivity` :: `numeric(1)`  
  The local connectivity required – i.e. the number of nearest neighbors
  that should be assumed to be connected at a local level. Default is
  `1`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `bandwidth` :: `numeric(1)`  
  The effective bandwidth of the kernel if we view the algorithm as
  similar to Laplacian Eigenmaps. Default is `1`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `repulsion_strength` :: `numeric(1)`  
  Weighting applied to negative samples in low dimensional embedding
  optimization. Default is `1`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `negative_sample_rate` :: `numeric(1)`  
  The number of negative edge/1-simplex samples to use per positive
  edge/1-simplex sample in optimizing the low dimensional embedding.
  Default is `5`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `a` :: `numeric(1)`  
  More specific parameters controlling the embedding. Default is `NULL`.
  For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `b` :: `numeric(1)`  
  More specific parameters controlling the embedding. Default is `NULL`.
  For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `nn_method` :: `character(1)`  
  Method for finding nearest neighbors. Note that only values compatible
  with
  [`uwot::umap_transform()`](https://jlmelville.github.io/uwot/reference/umap_transform.html)
  are allowed. Choose `"annoy"`, `"hnsw"`, or `"nndescent"`. Default is
  `NULL`, which lets
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html)
  select a method based on the installed packages and metrics. The
  `"hnsw"` and `"nndescent"` methods require the optional packages
  `RcppHNSW` and `rnndescent`, respectively.

- `n_trees` :: `integer(1)`  
  Number of trees to build when constructing the nearest neighbor index.
  Default is `50`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `search_k` :: `integer(1)`  
  Number of nodes to search during the neighbor retrieval. Only used if
  the `nn_method` is `"annoy"`. Defaults to `2 * n_neighbors * n_trees`
  in
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).
  For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `approx_pow` :: `logical(1)`  
  If `TRUE`, use an approximation to the power function in the UMAP
  gradient. Default is `FALSE`. Ignored when `dens_scale` is not `NULL`.
  For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `use_supervised` :: `logical(1)`  
  If `TRUE`, perform supervised dimension reduction by passing the
  task's target to
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html)'s
  `y` argument. For details, see there. Initialized to `FALSE`.

- `target_n_neighbors` :: `integer(1)`  
  Number of nearest neighbors to use to construct the target simplicial
  set. Only used when performing supervised dimension reduction. Default
  is `n_neighbors`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `target_metric` :: `character(1)`  
  The metric used to measure distance for a numeric target when
  performing supervised dimension reduction. Factor targets are handled
  as categorical data by `uwot`. Default is `"euclidean"`. For details,
  see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `target_weight` :: `numeric(1)`  
  Weighting factor between data topology and target topology. Only used
  when performing supervised dimension reduction. Default is `0.5`. For
  details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `pca` :: `integer(1)`  
  Reduce data to this number of columns using PCA. Default is `NULL`.
  For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `pca_center` :: `logical(1)`  
  If `TRUE`, center the columns of X before carrying out PCA. Default is
  `TRUE`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `rng_type` :: `character(1)`  
  Random number generator used during optimization: `"pcg"`,
  `"tausworthe"`, or `"deterministic"`. Default is `NULL`, which uses
  the upstream default of `"pcg"`, or `"tausworthe"` when
  `fast_sgd = TRUE`. The `"deterministic"` setting applies to
  optimization, not to neighbor search or initialization. For details,
  see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `fast_sgd` :: `logical(1)`  
  If `TRUE`, uses `n_sgd_threads = "auto"`, `approx_pow = TRUE`, and the
  Tausworthe generator unless `rng_type` is set explicitly. Default is
  `FALSE`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `n_threads` :: `integer(1)`  
  Number of threads used for neighbor search and other work outside
  embedding optimization. Default is `NULL`, which lets `uwot` choose
  the thread count. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `n_build_threads` :: `integer(1)`  
  Number of threads used to build the neighbor index for `"hnsw"` and
  `"nndescent"`. Default is `NULL`, which uses `n_threads`. Set to `1`
  to improve reproducibility of index construction. Annoy always builds
  its index with one thread. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `n_sgd_threads` :: `integer(1)`  
  Number of threads to use during stochastic gradient descent. Default
  is `0`. During training,
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html)
  uses `n_threads` when `batch = TRUE` and `n_sgd_threads = 0`. The
  special value `"auto"` also uses `n_threads`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `grain_size` :: `integer(1)`  
  The minimum amount of work to do on each thread. Default is `1`. For
  details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `verbose` :: `logical(1)`  
  Should details be printed? Initialized to `FALSE`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `batch` :: `logical(1)`  
  If `TRUE`, then embedding coordinates are updated at the end of each
  epoch rather than during the epoch. Default is `TRUE`. For details,
  see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `opt_args` :: named [`list()`](https://rdrr.io/r/base/list.html)  
  A list of optimizer parameters, used when `batch = TRUE`. Default is
  `NULL`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `epoch_callback` :: `function`  
  A function which will be invoked at the end of every epoch. Default is
  `NULL`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `pca_method` :: `character(1)`  
  Method to carry out any PCA dimensionality reduction when the `pca` is
  specified. Choose `"irlba"`, `"svdr"`, `"bigstatsr"`, `"svd"`, or
  `"auto"`. Default is `NULL`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `binary_edge_weights` :: `logical(1)`  
  If `TRUE` then edge weights in the input graph are treated as binary
  (0/1) rather than real valued. Default is `FALSE`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `dens_scale` :: `numeric(1)`  
  A scaling factor to apply to the density of the input data. Default is
  `NULL`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `seed` :: `integer(1)`  
  Integer seed to use to initialize the random number generator state.
  Default is `NULL`. For details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

- `nn_args` :: named [`list()`](https://rdrr.io/r/base/list.html)  
  A list containing additional arguments to pass to the nearest neighbor
  method. Default is [`list()`](https://rdrr.io/r/base/list.html). For
  details, see
  [`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html).

Additionally, there are several parameters that may be used to overwrite
parameter values for prediction:

- `search_k_transform` :: `integer(1)`  
  Number of nodes to search during the neighbor retrieval when
  predicting. Only used if `nn_method` is `"annoy"`. If `NULL`,
  `search_k` is used instead. Default is `NULL`. For details, see
  [`uwot::umap_transform()`](https://jlmelville.github.io/uwot/reference/umap_transform.html).

- `n_epochs_transform` :: `integer(1)`  
  Number of epochs used during the optimization of the embedded
  coordinates when predicting. Default is `NULL`, which uses one third
  of the training epochs, rounded and with a minimum of two. Set to `0`
  to return the initialized coordinates without optimization. For
  details, see
  [`uwot::umap_transform()`](https://jlmelville.github.io/uwot/reference/umap_transform.html).

- `init_transform` :: `character(1)`  
  Type of initialization for the coordinates when predicting. May be set
  to `"custom"`, in which case the matrix passed to
  `init_transform_custom` is used. Default is `"weighted"`. For details,
  see
  [`uwot::umap_transform()`](https://jlmelville.github.io/uwot/reference/umap_transform.html).

- `init_transform_custom` :: `matrix`  
  Matrix of initial coordinates when predicting. Only used if
  `init_transform` is `"custom"`.

- `batch_transform` :: `logical(1)`  
  If `TRUE`, embedding coordinates are updated at the end of each epoch
  rather than during the epoch when predicting. Default is `NULL`, which
  uses the value of `batch` stored in the trained model. For details,
  see
  [`uwot::umap_transform()`](https://jlmelville.github.io/uwot/reference/umap_transform.html).

- `learning_rate_transform` :: `numeric(1)`  
  Initial learning rate used in optimization of the coordinates when
  predicting. If `NULL`, `learning_rate` is used instead. Default is
  `NULL`. For details, see
  [`uwot::umap_transform()`](https://jlmelville.github.io/uwot/reference/umap_transform.html).

- `opt_args_transform` :: named
  [`list()`](https://rdrr.io/r/base/list.html)  
  Optimizer parameters used when predicting in batch mode. Default is
  `NULL`, which uses the optimizer parameters stored in the trained
  model. For details, see
  [`uwot::umap_transform()`](https://jlmelville.github.io/uwot/reference/umap_transform.html).

- `epoch_callback_transform` :: `function`  
  A function which will be invoked at the end of every epoch when
  predicting. Default is `NULL`. For details, see
  [`uwot::umap_transform()`](https://jlmelville.github.io/uwot/reference/umap_transform.html).

- `seed_transform` :: `integer(1)`  
  Random seed used when predicting. Default is `NULL`, which uses the
  seed stored in the trained model, if any. Set to `FALSE` to use the
  current random number generator state without resetting the seed. For
  details, see
  [`uwot::umap_transform()`](https://jlmelville.github.io/uwot/reference/umap_transform.html).

## Internals

Uses
[`uwot::umap2()`](https://jlmelville.github.io/uwot/reference/umap2.html)
with `ret_model = TRUE` for training and
[`uwot::umap_transform()`](https://jlmelville.github.io/uwot/reference/umap_transform.html)
for prediction.

## Fields

Only fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Methods

Only methods inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## References

McInnes L, Healy J, Melville J, Grossberger L (2018). “UMAP: Uniform
Manifold Approximation and Projection for Dimension Reduction.” *Journal
of Open Source Software*, **3**(29).
[doi:10.21105/joss.00861](https://doi.org/10.21105/joss.00861) .

## See also

https://mlr-org.com/pipeops.html

Other PipeOps:
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEncodePL.md),
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md),
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreprocSimple.md),
[`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops.md),
[`mlr_pipeops_adas`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_adas.md),
[`mlr_pipeops_blsmote`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_blsmote.md),
[`mlr_pipeops_boxcox`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_boxcox.md),
[`mlr_pipeops_branch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_branch.md),
[`mlr_pipeops_chunk`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_chunk.md),
[`mlr_pipeops_classbalancing`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classbalancing.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_classweights`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classweights.md),
[`mlr_pipeops_classweightsex`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classweightsex.md),
[`mlr_pipeops_colapply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_colapply.md),
[`mlr_pipeops_collapsefactors`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_collapsefactors.md),
[`mlr_pipeops_colroles`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_colroles.md),
[`mlr_pipeops_copy`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_copy.md),
[`mlr_pipeops_datefeatures`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_datefeatures.md),
[`mlr_pipeops_decode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_decode.md),
[`mlr_pipeops_encode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encode.md),
[`mlr_pipeops_encodeimpact`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encodeimpact.md),
[`mlr_pipeops_encodelmer`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encodelmer.md),
[`mlr_pipeops_encodeplquantiles`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encodeplquantiles.md),
[`mlr_pipeops_encodepltree`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encodepltree.md),
[`mlr_pipeops_featureunion`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_featureunion.md),
[`mlr_pipeops_filter`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_filter.md),
[`mlr_pipeops_fixfactors`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_fixfactors.md),
[`mlr_pipeops_histbin`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_histbin.md),
[`mlr_pipeops_ica`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ica.md),
[`mlr_pipeops_imputeconstant`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeconstant.md),
[`mlr_pipeops_imputehist`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputehist.md),
[`mlr_pipeops_imputelearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputelearner.md),
[`mlr_pipeops_imputemean`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemean.md),
[`mlr_pipeops_imputemedian`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemedian.md),
[`mlr_pipeops_imputemode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemode.md),
[`mlr_pipeops_imputeoor`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeoor.md),
[`mlr_pipeops_imputesample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputesample.md),
[`mlr_pipeops_info`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_info.md),
[`mlr_pipeops_isomap`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_isomap.md),
[`mlr_pipeops_kernelpca`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_kernelpca.md),
[`mlr_pipeops_learner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner.md),
[`mlr_pipeops_learner_pi_cvplus`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_pi_cvplus.md),
[`mlr_pipeops_learner_quantiles`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_quantiles.md),
[`mlr_pipeops_materialize`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_materialize.md),
[`mlr_pipeops_missind`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_missind.md),
[`mlr_pipeops_modelmatrix`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_modelmatrix.md),
[`mlr_pipeops_multiplicityexply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_multiplicityexply.md),
[`mlr_pipeops_multiplicityimply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_multiplicityimply.md),
[`mlr_pipeops_mutate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_mutate.md),
[`mlr_pipeops_nearmiss`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_nearmiss.md),
[`mlr_pipeops_nmf`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_nmf.md),
[`mlr_pipeops_nop`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_nop.md),
[`mlr_pipeops_ovrsplit`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrsplit.md),
[`mlr_pipeops_ovrunite`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrunite.md),
[`mlr_pipeops_pca`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_pca.md),
[`mlr_pipeops_proxy`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_proxy.md),
[`mlr_pipeops_quantilebin`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_quantilebin.md),
[`mlr_pipeops_randomprojection`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_randomprojection.md),
[`mlr_pipeops_randomresponse`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_randomresponse.md),
[`mlr_pipeops_regravg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_regravg.md),
[`mlr_pipeops_removeconstants`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_removeconstants.md),
[`mlr_pipeops_renamecolumns`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_renamecolumns.md),
[`mlr_pipeops_replicate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_replicate.md),
[`mlr_pipeops_rowapply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_rowapply.md),
[`mlr_pipeops_scale`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_scale.md),
[`mlr_pipeops_scalemaxabs`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_scalemaxabs.md),
[`mlr_pipeops_scalerange`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_scalerange.md),
[`mlr_pipeops_select`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_select.md),
[`mlr_pipeops_smote`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_smote.md),
[`mlr_pipeops_smotenc`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_smotenc.md),
[`mlr_pipeops_spatialsign`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_spatialsign.md),
[`mlr_pipeops_splines`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_splines.md),
[`mlr_pipeops_subsample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_subsample.md),
[`mlr_pipeops_targetinvert`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targetinvert.md),
[`mlr_pipeops_targetmutate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targetmutate.md),
[`mlr_pipeops_targettrafoscalerange`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targettrafoscalerange.md),
[`mlr_pipeops_textvectorizer`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_textvectorizer.md),
[`mlr_pipeops_threshold`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_threshold.md),
[`mlr_pipeops_tomek`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_tomek.md),
[`mlr_pipeops_tunethreshold`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_tunethreshold.md),
[`mlr_pipeops_unbranch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_unbranch.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_updatetarget.md),
[`mlr_pipeops_vtreat`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_vtreat.md),
[`mlr_pipeops_yeojohnson`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_yeojohnson.md)

## Examples

``` r
library("mlr3")

task = tsk("iris")
train_rows = c(1:40, 51:90, 101:140)
test_rows = setdiff(task$row_ids, train_rows)
pop = po("umap", nn_method = "annoy", seed = 42L, n_threads = 1L)

embedded_train = pop$train(list(task$clone()$filter(train_rows)))[[1L]]
embedded_test = pop$predict(list(task$clone()$filter(test_rows)))[[1L]]

head(embedded_train$data())
#>    Species        V1        V2
#>     <fctr>     <num>     <num>
#> 1:  setosa -6.811571 0.7999146
#> 2:  setosa -8.576679 1.2044388
#> 3:  setosa -8.277728 0.7480515
#> 4:  setosa -8.430686 0.8573458
#> 5:  setosa -7.048654 0.5664133
#> 6:  setosa -6.036652 0.4650353
head(embedded_test$data())
#>    Species        V1        V2
#>     <fctr>     <num>     <num>
#> 1:  setosa -6.937847 1.4724518
#> 2:  setosa -8.967123 1.3046246
#> 3:  setosa -9.136282 0.9788051
#> 4:  setosa -6.885417 1.6396841
#> 5:  setosa -6.812132 0.1067286
#> 6:  setosa -8.946117 1.4151392
```
