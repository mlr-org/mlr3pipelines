context("PipeOpBLSmote")

test_that("PipeOpBLSmote - basic properties", {
  skip_if_not_installed("smotefamily")

  task = mlr_tasks$get("spam")

  expect_datapreproc_pipeop_class(PipeOpBLSmote, task = task, predict_like_train = FALSE, deterministic_train = FALSE)
})

test_that("PipeOpBLSmote - train works as intended", {
  skip_if_not_installed("smotefamily")

  op = PipeOpBLSmote$new()

  df = smotefamily::sample_generator(500, 0.8)
  df$result = factor(df$result)
  setnames(df, "result", "class")  # we do this to avoid renaming later
  df = df[, c(3L, 1L, 2L)]  # we do this to avoid reordering later
  task = TaskClassif$new(id = "test", backend = df, target = "class")

  # Compare to smotefamily::BLSMOTE with default params
  set.seed(1234L)
  train_out = op$train(list(task))[[1L]]$data()
  set.seed(1234L)
  blsmote_out = setDT(rbind(
    df,
    invoke(smotefamily::BLSMOTE, X = task$data(cols = task$feature_names), target = task$truth(),
           .opts = list(warnPartialMatchArgs = FALSE))$syn_data
  ))

  expect_equal(train_out, blsmote_out)

  # Compare to smotefamily::BLSMOTE with changed params
  # method = "type1"
  pv = list(K = 4L, C = 8L, dupSize = 0, method = "type1")
  op$param_set$set_values(.values = pv)

  set.seed(1234L)
  train_out = op$train(list(task))[[1L]]$data()
  set.seed(1234L)
  blsmote_out = setDT(rbind(
    df,
    invoke(smotefamily::BLSMOTE, X = task$data(cols = task$feature_names), target = task$truth(),
           .args = pv, .opts = list(warnPartialMatchArgs = FALSE))$syn_data
  ))

  expect_equal(train_out, blsmote_out)

  # method = "type1"
  pv = list(K = 4L, C = 8L, dupSize = 0, method = "type2")
  op$param_set$set_values(.values = pv)

  set.seed(1234L)
  train_out = op$train(list(task))[[1L]]$data()
  set.seed(1234L)
  blsmote_out = setDT(rbind(
    df,
    invoke(smotefamily::BLSMOTE, X = task$data(cols = task$feature_names), target = task$truth(),
      .args = pv, .opts = list(warnPartialMatchArgs = FALSE))$syn_data
  ))

  expect_equal(train_out, blsmote_out)

  # Empty task is returned unchanged
  task$select(character(0L))
  expect_equal(
    op$train(list(task))[[1L]],
    task
  )

})
