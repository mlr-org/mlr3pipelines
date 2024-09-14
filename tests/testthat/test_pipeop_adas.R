context("PipeOpADAS")

test_that("PipeOpADAS - basic properties", {
  skip_if_not_installed("smotefamily")

  task = mlr_tasks$get("spam")

  expect_datapreproc_pipeop_class(PipeOpADAS, task = task, predict_like_train = FALSE, deterministic_train = FALSE)
})

test_that("PipeOpADAS - train works as intended", {
  skip_if_not_installed("smotefamily")

  op = PipeOpADAS$new()

  df = data.frame(
    class = factor(sample(c("c1", "c2"), size = 200, replace = TRUE, prob = c(0.1, 0.9))),
    x1 = rnorm(200),
    x2 = rnorm(200)
  )
  task = TaskClassif$new(id = "test", backend = df, target = "class")

  # Compare to smotefamily::ADAS with default params
  set.seed(1234L)
  train_out = op$train(list(task))[[1]]$data()
  set.seed(1234L)
  # rbind for same row (and col) order
  adas_out = setDT(rbind(
    df,
    invoke(smotefamily::ADAS, X = task$data(cols = task$feature_names), target = task$truth())$syn_data
  ))

  expect_equal(train_out, adas_out)

  # Compare to smotefamily::ADAS with changed params
  op$param_set$set_values(K = 10)

  set.seed(1234L)
  train_out = op$train(list(task))[[1]]$data()
  set.seed(1234L)
  adas_out = setDT(rbind(
    df,
    invoke(smotefamily::ADAS, X = task$data(cols = task$feature_names), target = task$truth(), K = 10)$syn_data
  ))

  expect_equal(train_out, adas_out)

  # Empty task is returned unchanged
  task$select(character(0))
  expect_equal(
    op$train(list(task))[[1L]],
    task
  )

})
