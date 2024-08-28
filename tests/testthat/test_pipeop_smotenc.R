context("PipeOpSmoteNC")

test_that("PipeOpSmoteNC - basic properties", {
  skip_if_not_installed("themis")

  task = mlr_tasks$get("german_credit")

  expect_datapreproc_pipeop_class(PipeOpSmoteNC, task = task, predict_like_train = FALSE, deterministic_train = FALSE)

})

test_that("PipeOpSmoteNC - train works as intended", {
  skip_if_not_installed("themis")

  op = PipeOpSmoteNC$new()
  df_unbalanced = data.frame(
    class = factor(rep(c("pos", "neg"), times = c(150, 50))),
    x1 = rnorm(200),
    x2 = factor(sample(c("a", "b"), replace = TRUE, size = 200))
  )
  task_unbalanced = TaskClassif$new(id = "test_unbalanced", backend = df_unbalanced, target = "class")

  df_balanced = data.frame(
    class = factor(rep(c("pos", "neg"), each = 100)),
    x1 = rnorm(200),
    x2 = factor(sample(c("a", "b"), replace = TRUE, size = 200))
  )
  task_balanced = TaskClassif$new(id = "test_balanced", backend = df_balanced, target = "class")

  df_feat = data.frame(
    class = factor(rep(c("pos", "neg"), times = c(185, 15))),
    x1 = rnorm(200),
    x2 = factor(sample(c("a", "b"), replace = TRUE, size = 200), ordered = TRUE),
    x3 = as.integer(rbinom(200, 20, 0.3))
  )
  task_feat = TaskClassif$new(id = "test_different_feature_types", backend = df_feat, target = "class")

  # PipeOp does not accept tasks with no numeric or integer feature
  task = tsk("breast_cancer")
  expect_error(op$train(list(task)))

  # Empty task is returned unchanged
  task$select(character(0))
  expect_equal(
    op$train(list(task))[[1L]],
    task
  )

  # Compare to themis::smotenc for unbalanced data with default params
  set.seed(1234L)
  train_out = op$train(list(task_unbalanced))[[1]]$data()
  set.seed(1234L)
  smotenc_out = setDT(invoke(themis::smotenc, df = df_unbalanced, var = "class"))

  expect_equal(train_out, smotenc_out)

  # Compare to themis::smotenc for unbalanced data with changed params
  op$param_set$set_values(k = 10, over_ratio = 0.5)

  set.seed(1234L)
  train_out = op$train(list(task_unbalanced))[[1]]$data()
  set.seed(1234L)
  smotenc_out = setDT(invoke(themis::smotenc, df = df_unbalanced, var = "class", k = 10, over_ratio = 0.5))

  expect_equal(train_out, smotenc_out)

  # Compare to themis::smotenc for balanced data with over_ratio = 1 (i.e. no new data)
  op$param_set$set_values(k = 5, over_ratio = 1)

  expect_equal(
    op$train(list(task_balanced))[[1]]$data(),
    setDT(invoke(themis::smotenc, df = df_balanced, var = "class"))
  )

  # Compare to themis::smotenc for tasks with different feature types
  set.seed(1234L)
  train_out = op$train(list(task_feat))[[1]]$data()
  set.seed(1234L)
  smotenc_out = setDT(invoke(themis::smotenc, df = df_feat, var = "class"))
  # We do not expect exact equality for integer columns as we round them to keep the same feature type while themis::smotenc
  # treats them purely as numeric
  smotenc_out$x3 = as.integer(round(smotenc_out$x3))

  expect_equal(train_out, smotenc_out)

})
