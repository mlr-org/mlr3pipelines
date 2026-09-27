context("PipeOpUMAP")

test_that("PipeOpUMAP - defaults describe uwot without initializing its parameters", {
  skip_if_not_installed("uwot", "0.2.5")

  op = PipeOpUMAP$new()
  expect_valid_pipeop_param_set(op)
  expect_set_equal(names(op$param_set$values), c("verbose", "use_supervised"))
  expect_identical(op$param_set$values[c("verbose", "use_supervised")],
    list(verbose = FALSE, use_supervised = FALSE))
  expect_false("pcg_rand" %in% op$param_set$ids())

  train_params = c("min_dist", "nn_args", "rng_type", "n_build_threads", "n_epochs", "init_sdev", "pca_method")
  expect_equal(op$param_set$default[train_params], lapply(formals(uwot::umap2)[train_params], eval))
  predict_params = c("batch", "opt_args", "seed")
  defaults = op$param_set$default[paste0(predict_params, "_transform")]
  names(defaults) = predict_params
  expect_equal(defaults, as.list(formals(uwot::umap_transform))[predict_params])

  expect_identical(op$param_set$get_values(tags = c("train", "umap")), list(verbose = FALSE))
  expect_identical(op$param_set$get_values(tags = c("predict", "umap")), list(verbose = FALSE))
  expect_length(op$param_set$get_values(tags = c("predict", "overwrite")), 0L)

  op = PipeOpUMAP$new(param_vals = list(verbose = TRUE, use_supervised = TRUE))
  expect_true(op$param_set$values$verbose)
  expect_true(op$param_set$values$use_supervised)
})

test_that("PipeOpUMAP - new parameters match uwot training and prediction", {
  skip_if_not_installed("uwot", "0.2.5")

  task = tsk("iris")$filter(1:30)
  dt = task$data(cols = task$feature_names)
  predict_params = list(opt_args = list(method = "sgd"), seed = 5678L)

  for (rng_type in c("pcg", "tausworthe", "deterministic")) {
    train_params = list(nn_method = "annoy", seed = 1234L, n_epochs = 20L, n_threads = 1L,
      n_build_threads = 1L, rng_type = rng_type, nn_args = list(), metric = "euclidean", pca = 2L,
      pca_method = "svdr", verbose = FALSE)
    op = PipeOpUMAP$new(param_vals = c(train_params,
      list(opt_args_transform = predict_params$opt_args, seed_transform = predict_params$seed)))

    trained = op$train(list(task))[[1L]]
    model = invoke(uwot::umap2, X = dt, ret_model = TRUE, .args = train_params)
    expect_equal(trained$data(cols = trained$feature_names), as.data.table(model$embedding))

    predicted = op$predict(list(task))[[1L]]
    expected = invoke(uwot::umap_transform, X = dt, model = model, n_threads = 1L, .args = predict_params)
    expect_equal(predicted$data(cols = predicted$feature_names), as.data.table(expected))

    op$param_set$set_values(seed_transform = FALSE)
    set.seed(42)
    predicted = op$predict(list(task))[[1L]]
    set.seed(42)
    expected = uwot::umap_transform(dt, model, n_threads = 1L, opt_args = predict_params$opt_args, seed = FALSE)
    expect_equal(predicted$data(cols = predicted$feature_names), as.data.table(expected))
  }
})

test_that("PipeOpUMAP - zero epochs and unscaled custom initialization match uwot", {
  skip_if_not_installed("uwot", "0.2.5")

  task = tsk("iris")$filter(1:30)
  init = matrix(seq_len(60L) / 60, ncol = 2L)
  op = PipeOpUMAP$new(param_vals = list(nn_method = "annoy", n_epochs = 0L, n_epochs_transform = 0L,
    init = "custom", init_custom = init, init_sdev = NULL, seed = 1234L, n_threads = 1L))
  trained = op$train(list(task))[[1L]]
  model = uwot::umap2(task$data(cols = task$feature_names), nn_method = "annoy", n_epochs = 0L,
    init = init, init_sdev = NULL, seed = 1234L, n_threads = 1L, ret_model = TRUE, verbose = FALSE)
  expect_equal(trained$data(cols = trained$feature_names), as.data.table(model$embedding))

  predicted = op$predict(list(task))[[1L]]
  expected = uwot::umap_transform(task$data(cols = task$feature_names), model, n_epochs = 0L, n_threads = 1L)
  expect_equal(predicted$data(cols = predicted$feature_names), as.data.table(expected))
})

test_that("PipeOpUMAP - basic properties", {
  skip_if_not_installed("uwot", "0.2.5")
  skip_if_not_installed("RcppAnnoy")
  skip_if_not_installed("RcppHNSW")
  skip_if_not_installed("rnndescent")

  task = mlr_tasks$get("iris")$filter(1:30)

  # Test for different nn_methods since they are relying on different packages and deep clone is implemented differently
  expect_datapreproc_pipeop_class(PipeOpUMAP, constargs = list(param_vals = list(nn_method = "annoy")),
                                  deterministic_train = FALSE, deterministic_predict = FALSE, task = task)
  expect_datapreproc_pipeop_class(PipeOpUMAP, constargs = list(param_vals = list(nn_method = "hnsw")),
                                  deterministic_train = FALSE, deterministic_predict = FALSE, task = task)
  expect_datapreproc_pipeop_class(PipeOpUMAP, constargs = list(param_vals = list(nn_method = "nndescent")),
                                  deterministic_train = FALSE, deterministic_predict = FALSE, task = task)

})

test_that("PipeOpUMAP - Compare to uwot::umap2 and uwot::umap_transform; Default Params, nn_method = annoy", {
  skip_if_not_installed("uwot", "0.2.5")
  skip_if_not_installed("RcppAnnoy")
  task = mlr_tasks$get("iris")$filter(1:30)

  op = PipeOpUMAP$new()
  pv = list(seed = 1234L, nn_method = "annoy")
  op$param_set$set_values(.values = pv)

  train_out = train_pipeop(op, list(task))[[1L]]
  umap_out = invoke(uwot::umap2, X = task$data()[, 2:5], ret_model = TRUE, .args = pv)

  state_names = c("embedding", "scale_info", "search_k", "local_connectivity", "n_epochs", "alpha", "negative_sample_rate", "method", "a", "b",
                  "gamma", "approx_pow", "metric", "norig_col", "pcg_rand", "batch", "opt_args", "num_precomputed_nns", "min_dist", "spread",
                  "binary_edge_weights", "seed", "nn_method", "nn_args", "n_neighbors", "nn_index", "pca_models")
  expect_true(all(state_names %in% names(op$state)))
  state_names_wo_pointers = setdiff(state_names, "nn_index") #  since RefClass in state$nn_index$ann will not be equal
  expect_identical(op$state[state_names_wo_pointers], umap_out[state_names_wo_pointers])
  expect_equal(train_out$data()[, 2:3], as.data.table(umap_out[["embedding"]]))

  predict_out = predict_pipeop(op, list(task))[[1L]]
  umap_transform_out = invoke(uwot::umap_transform, X = task$data()[, 2:5], model = umap_out)
  expect_equal(predict_out$data()[, 2:3], as.data.table(umap_transform_out))

})


test_that("PipeOpUMAP - Compare to uwot::umap2 and uwot::umap_transform; Changed Params, nn_method = annoy", {
  skip_if_not_installed("uwot", "0.2.5")
  skip_if_not_installed("RcppAnnoy")
  task = mlr_tasks$get("iris")$filter(1:30)

  op = PipeOpUMAP$new()

  # BUild list of param with same names for PipeOpUMAP and uwot::umap2() / uwot::umap_transform()
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
    negative_sample_rate = 6
  )
  # Handle parameters that are differently named for PipeOpUMAP and uwot::umap2() / uwot::umap_transform()
  pv_po = insert_named(pv, list(use_supervised = TRUE,
                                batch_transform = TRUE,
                                init_transform = "average",
                                search_k_transform = 1000L))
  op$param_set$set_values(.values = pv_po)
  args_umap2 = insert_named(pv, list(ret_model = TRUE, y = task$data()[, 1]))
  args_umap_transform = list(init = "average", search_k = 1000L, batch = TRUE)

  train_out = train_pipeop(op, list(task))[[1L]]
  umap_out = invoke(uwot::umap2, X = task$data()[, 2:5], .args = args_umap2)

  state_names = c("embedding", "scale_info", "search_k", "local_connectivity", "n_epochs", "alpha", "negative_sample_rate", "method", "a", "b",
                  "gamma", "approx_pow", "metric", "norig_col", "pcg_rand", "batch", "opt_args", "num_precomputed_nns", "min_dist", "spread",
                  "binary_edge_weights", "seed", "nn_method", "nn_args", "n_neighbors", "nn_index", "pca_models")
  expect_true(all(state_names %in% names(op$state)))
  state_names = setdiff(state_names, "nn_index") #  since RefClass in state$nn_index$ann will not be equal
  expect_identical(op$state[state_names], umap_out[state_names])
  expect_equal(train_out$data()[, 2:3], as.data.table(umap_out[["embedding"]]))

  predict_out = predict_pipeop(op, list(task))[[1L]]
  umap_transform_out = invoke(uwot::umap_transform, X = task$data()[, 2:5], model = umap_out, .args = args_umap_transform)

  expect_equal(predict_out$data()[, 2:3], as.data.table(umap_transform_out))

})


test_that("PipeOpUMAP - Compare to uwot::umap2 and uwot::umap_transform; Changed Params, nn_method = hnsw", {
  skip_if_not_installed("uwot", "0.2.5")
  skip_if_not_installed("RcppHNSW")
  task = mlr_tasks$get("iris")$filter(1:30)

  op = PipeOpUMAP$new()

  # BUild list of param with same names for PipeOpUMAP and uwot::umap2() / uwot::umap_transform()
  pv = list(
    seed = 1234L,
    nn_method = "hnsw",
    n_threads = 1L,
    n_build_threads = 1L,
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
    nn_args = list(M = 10L, ef_construction = 100L, ef = 20L)
  )
  # Handle parameters that are differently named for PipeOpUMAP and uwot::umap2() / uwot::umap_transform()
  pv_po = insert_named(pv, list(use_supervised = TRUE, init_transform = "average"))
  op$param_set$set_values(.values = pv_po)
  args_umap2 = insert_named(pv, list(ret_model = TRUE, y = task$data()[, 1]))
  args_umap_transform = list(init = "average", n_threads = 1L)

  train_out = train_pipeop(op, list(task))[[1L]]
  umap_out = invoke(uwot::umap2, X = task$data()[, 2:5], .args = args_umap2)

  state_names = c("embedding", "scale_info", "search_k", "local_connectivity", "n_epochs", "alpha", "negative_sample_rate", "method", "a", "b",
                  "gamma", "approx_pow", "metric", "norig_col", "pcg_rand", "batch", "opt_args", "num_precomputed_nns", "min_dist", "spread",
                  "binary_edge_weights", "seed", "nn_method", "nn_args", "n_neighbors", "nn_index", "pca_models")
  expect_true(all(state_names %in% names(op$state)))
  state_names = setdiff(state_names, "nn_index") #  since RefClass in state$nn_index$ann will not be equal
  expect_identical(op$state[state_names], umap_out[state_names])
  expect_equal(train_out$data()[, 2:3], as.data.table(umap_out[["embedding"]]))

  predict_out = predict_pipeop(op, list(task))[[1L]]
  umap_transform_out = invoke(uwot::umap_transform, X = task$data()[, 2:5], model = umap_out, .args = args_umap_transform)
  expect_equal(predict_out$data()[, 2:3], as.data.table(umap_transform_out))

})


test_that("PipeOpUMAP - Compare to uwot::umap2 and uwot::umap_transform; Changed Params, nn_method = rnndescent", {
  skip_if_not_installed("uwot", "0.2.5")
  skip_if_not_installed("rnndescent")
  task = mlr_tasks$get("iris")$filter(1:30)

  op = PipeOpUMAP$new()

  # BUild list of param with same names for PipeOpUMAP and uwot::umap2() / uwot::umap_transform()
  pv = list(
    seed = 1234L,
    nn_method = "nndescent",
    n_threads = 1L,
    n_build_threads = 1L,
    n_neighbors = 10L,
    metric = "symmetrickl",
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
    nn_args = list(n_trees = 15L, max_candidates = 15L, pruning_degree_multiplier = 1.4, epsilon = 0.05)
  )
  # Handle parameters that are differently named for PipeOpUMAP and uwot::umap2() / uwot::umap_transform()
  pv_po = insert_named(pv, list(use_supervised = TRUE, init_transform = "average"))
  op$param_set$set_values(.values = pv_po)
  args_umap2 = insert_named(pv, list(ret_model = TRUE, y = task$data()[, 1]))
  args_umap_transform = list(init = "average", n_threads = 1L)

  train_out = train_pipeop(op, list(task))[[1L]]
  umap_out = invoke(uwot::umap2, X = task$data()[, 2:5], .args = args_umap2)

  state_names = c("embedding", "scale_info", "search_k", "local_connectivity", "n_epochs", "alpha", "negative_sample_rate", "method", "a", "b",
                  "gamma", "approx_pow", "metric", "norig_col", "pcg_rand", "batch", "opt_args", "num_precomputed_nns", "min_dist", "spread",
                  "binary_edge_weights", "seed", "nn_method", "nn_args", "n_neighbors", "nn_index", "pca_models")
  expect_true(all(state_names %in% names(op$state)))

  state_names = setdiff(state_names, "nn_index") #  since RefClass in state$nn_index$ann will not be equal
  expect_identical(op$state[state_names], umap_out[state_names])
  expect_equal(train_out$data()[, 2:3], as.data.table(umap_out[["embedding"]]))

  predict_out = predict_pipeop(op, list(task))[[1L]]
  umap_transform_out = invoke(uwot::umap_transform, X = task$data()[, 2:5], model = umap_out, .args = args_umap_transform)
  expect_equal(predict_out$data()[, 2:3], as.data.table(umap_transform_out))

})
