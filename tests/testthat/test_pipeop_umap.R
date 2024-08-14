context("PipeOpUMAP")

test_that("PipeOpUMAP - basic properties", {
  skip_if_not_installed("uwot")
  skip_if_not_installed("RcppAnnoy")
  skip_if_not_installed("RcppHNSW")
  skip_if_not_installed("rnndescent")

  task = mlr_tasks$get("iris")$filter(1:30)

  # Test for different nn_methods since they are relying on different packages and deep clone is impleneted differently
  expect_datapreproc_pipeop_class(PipeOpUMAP, constargs = list(param_vals = list(nn_method = "annoy")),
                                  deterministic_train = FALSE, deterministic_predict = FALSE, task = task)
  expect_datapreproc_pipeop_class(PipeOpUMAP, constargs = list(param_vals = list(nn_method = "hnsw")),
                                  deterministic_train = FALSE, deterministic_predict = FALSE, task = task)
  expect_datapreproc_pipeop_class(PipeOpUMAP, constargs = list(param_vals = list(nn_method = "nndescent")),
                                  deterministic_train = FALSE, deterministic_predict = FALSE, task = task)

})

test_that("PipeOpUMAP - Compare to uwot::umap2 and uwot::umap_transform; Default Params, nn_method = annoy", {
  skip_if_not_installed("uwot")
  task = mlr_tasks$get("iris")$filter(1:30)

  op = PipeOpUMAP$new()
  pv = list(seed = 1234L)
  op$param_set$set_values(.values = pv)

  train_out = train_pipeop(op, list(task))[[1L]]
  umap_out = invoke(uwot::umap2, X = task$data()[, 2:5], ret_model = TRUE, .args = pv)

  state_names = c("embedding", "scale_info", "search_k", "local_connectivity", "n_epochs", "alpha", "negative_sample_rate", "method", "a", "b",
                  "gamma", "approx_pow", "metric", "norig_col", "pcg_rand", "batch", "opt_args", "num_precomputed_nns", "min_dist", "spread",
                  "binary_edge_weights", "seed", "nn_method", "nn_args", "n_neighbors", "nn_index", "pca_models")
  expect_true(all(state_names %in% names(op$state)))
  state_names_wo_pointers = setdiff(state_names, "nn_index") #  since address in state$nn_index$ann will not be equal
  expect_identical(op$state[state_names_wo_pointers], umap_out[state_names_wo_pointers])
  expect_equal(train_out$data()[, 2:3], as.data.table(umap_out[["embedding"]]))

  predict_out = predict_pipeop(op, list(task))[[1L]]
  umap_transform_out = invoke(uwot::umap_transform, X = task$data()[, 2:5], model = umap_out)
  expect_equal(predict_out$data()[, 2:3], as.data.table(umap_transform_out))

})


test_that("PipeOpUMAP - Compare to uwot::umap2 and uwot::umap_transform; Changed Params, nn_method = annoy", {
  skip_if_not_installed("uwot")
  task = mlr_tasks$get("iris")$filter(1:30)

  op = PipeOpUMAP$new()
  pv = list(
    seed = 1234L,
    nn_method = "annoy",
    n_neighbors = 10L,
    metric = "correlation",
    n_epochs = 100L,
    learning_rate = 0.5,
    scale = FALSE,
    init = "pca",
    init_sdev = 1e-4,
    set_op_mix_ratio = 0.5,
    local_connectivity = 1.1,
    bandwidth = 0.9,
    repulsion_strength = 1.1,
    negative_sample_rate = 6,
    y = task$data()[, 1]
  )
  op$param_set$set_values(.values = pv)

  train_out = train_pipeop(op, list(task))[[1L]]
  umap_out = invoke(uwot::umap2, X = task$data()[, 2:5], ret_model = TRUE, .args = pv)

  state_names = c("embedding", "scale_info", "search_k", "local_connectivity", "n_epochs", "alpha", "negative_sample_rate", "method", "a", "b",
                  "gamma", "approx_pow", "metric", "norig_col", "pcg_rand", "batch", "opt_args", "num_precomputed_nns", "min_dist", "spread",
                  "binary_edge_weights", "seed", "nn_method", "nn_args", "n_neighbors", "nn_index", "pca_models")
  expect_true(all(state_names %in% names(op$state)))
  state_names = setdiff(state_names, "nn_index") #  since address in state$nn_index$ann will not be equal
  expect_identical(op$state[state_names], umap_out[state_names])
  expect_equal(train_out$data()[, 2:3], as.data.table(umap_out[["embedding"]]))

  predict_out = predict_pipeop(op, list(task))[[1L]]
  umap_transform_out = invoke(uwot::umap_transform, X = task$data()[, 2:5], model = umap_out)
  expect_equal(predict_out$data()[, 2:3], as.data.table(umap_transform_out))

})

# weitere tests für nn_methods
# for these use options that are specific to that method
