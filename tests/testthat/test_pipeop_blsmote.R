context("PipeOpBLSmote")

test_that("PipeOpBLSmote - basic properties", {
  skip_if_not_installed("smotefamily")

  task = mlr_tasks$get("spam")

  expect_datapreproc_pipeop_class(PipeOpBLSmote, task = task, predict_like_train = FALSE, deterministic_train = FALSE)
})

test_that("PipeOpBLSmote - train works as intended", {
  skip_if_not_installed("smotefamily")

  op = PipeOpBLSmote$new()

  df = data.frame(
    class = factor(sample(c("c1", "c2"), size = 200, replace = TRUE, prob = c(0.1, 0.9))),
    x1 = rnorm(200),
    x2 = rnorm(200)
  )
  task = TaskClassif$new(id = "test", backend = df, target = "class")

  # Compare to smotefamily::BLSMOTE with default params
  set.seed(1234L)
  train_out = op$train(list(task))[[1]]$data()
  set.seed(1234L)
  blsmote_out = setDT(rbind(
    df,
    invoke(smotefamily::BLSMOTE, X = task$data(cols = task$feature_names), target = task$truth())$syn_data
  ))

  expect_equal(train_out, blsmote_out)

  # Compare to smotefamily::BLSMOTE with changed params
  # method = "type2"
  op$param_set$set_values(K = 10, C = 8, dupSize = 0, method = "type1")

  set.seed(1234L)
  train_out = op$train(list(task))[[1]]$data()
  set.seed(1234L)
  blsmote_out = setDT(rbind(
    df,
    invoke(smotefamily::BLSMOTE, X = task$data(cols = task$feature_names), target = task$truth(),
           K = 10, C = 8, dupSize = 0, method = "type1")$syn_data
  ))

  expect_equal(train_out, blsmote_out)

  # method = "type1"
  op$param_set$set_values(K = 10, C = 8, dupSize = 0, method = "type2")

  set.seed(1234L)
  train_out = op$train(list(task))[[1]]$data()
  set.seed(1234L)
  blsmote_out = setDT(rbind(
    df,
    invoke(smotefamily::BLSMOTE, X = task$data(cols = task$feature_names), target = task$truth(),
           K = 10, C = 8, dupSize = 0, method = "type2")$syn_data
  ))

  expect_equal(train_out, blsmote_out)

  # Empty task is returned unchanged
  task$select(character(0))
  expect_equal(
    op$train(list(task))[[1L]],
    task
  )

})
