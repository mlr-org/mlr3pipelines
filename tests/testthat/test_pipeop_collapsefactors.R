context("PipeOpCollapseFactors")

test_that("PipeOpCollapseFactors - basic properties", {
  task = mlr_tasks$get("penguins")

  expect_datapreproc_pipeop_class(PipeOpCollapseFactors, task = task)
})

test_that("PipeOpCollapseFactors - train and predict work", {
  op = PipeOpCollapseFactors$new()
  df = data.frame(
    target = runif(100),
    fct = factor(rep(LETTERS[1:6], times = c(25, 30, 5, 15, 5, 20))),
    ord = factor(rep(1:6, times = c(20, 25, 30, 5, 5, 15)), ordered = TRUE)
  )
  task = TaskRegr$new(df, target = "target", id = "test")

  # test (default): levels are reduced to target_count, correct levels are chosen for this
  train_out = op$train(list(task))[[1]]
  expect_equal(train_out$data(cols = c("fct"))[[1]], factor(rep(c("A", "B", "A"), times = c(25, 30, 45))))
  expect_equal(train_out$data(cols = c("ord"))[[1]], factor(rep(c("2", "3"), times = c(45, 55)), ordered = TRUE))

  predict_out = op$predict(list(task))[[1]]
  expect_equal(predict_out$data(cols = c("fct"))[[1]], factor(rep(c("A", "B", "A"), times = c(25, 30, 45))))
  expect_equal(predict_out$data(cols = c("ord"))[[1]], factor(rep(c("2", "3"), times = c(45, 55)), ordered = TRUE))

  # test: target_count works
  op$param_set$values$target_level_count = 4
  train_out = op$train(list(task))[[1]]
  expect_equal(train_out$data(cols = c("fct"))[[1]], factor(rep(c("A", "B", "D", "F"), times = c(25, 30, 25, 20))))
  expect_equal(train_out$data(cols = c("ord"))[[1]], factor(rep(c("1", "2", "3", "6"), times = c(20, 25, 30, 25)), ordered = TRUE))

  predict_out = op$predict(list(task))[[1]]
  expect_equal(predict_out$data(cols = c("fct"))[[1]], factor(rep(c("A", "B", "D", "F"), times = c(25, 30, 25, 20))))
  expect_equal(predict_out$data(cols = c("ord"))[[1]], factor(rep(c("1", "2", "3", "6"), times = c(20, 25, 30, 25)), ordered = TRUE))
  op$param_set$values$target_level_count = 2

  # test: absolute works
  op$param_set$values$no_collapse_above_absolute = 15
  train_out = op$train(list(task))[[1]]
  expect_equal(train_out$data(cols = c("fct"))[[1]], factor(rep(c("A", "B", "F"), times = c(25, 30, 45))))
  expect_equal(train_out$data(cols = c("ord"))[[1]], factor(rep(c("1", "2", "3"), times = c(20, 25, 55)), ordered = TRUE))

  predict_out = op$predict(list(task))[[1]]
  expect_equal(predict_out$data(cols = c("fct"))[[1]], factor(rep(c("A", "B", "F"), times = c(25, 30, 45))))
  expect_equal(predict_out$data(cols = c("ord"))[[1]], factor(rep(c("1", "2", "3"), times = c(20, 25, 55)), ordered = TRUE))
  op$param_set$values$no_collapse_above_absolute = Inf

  # test: prevalence works
  op$param_set$values$no_collapse_above_prevalence = 0.15
  train_out = op$train(list(task))[[1]]
  expect_equal(train_out$data(cols = c("fct"))[[1]], factor(rep(c("A", "B", "F"), times = c(25, 30, 45))))
  expect_equal(train_out$data(cols = c("ord"))[[1]], factor(rep(c("1", "2", "3"), times = c(20, 25, 55)), ordered = TRUE))

  predict_out = op$predict(list(task))[[1]]
  expect_equal(predict_out$data(cols = c("fct"))[[1]], factor(rep(c("A", "B", "F"), times = c(25, 30, 45))))
  expect_equal(predict_out$data(cols = c("ord"))[[1]], factor(rep(c("1", "2", "3"), times = c(20, 25, 55)), ordered = TRUE))

  # test: if given both, does as documented (i.e. lower one is used since we are using union)
  op$param_set$values$no_collapse_above_absolute = 10
  train_out = op$train(list(task))[[1]]
  expect_equal(train_out$data(cols = c("fct"))[[1]], factor(rep(c("A", "B", "D", "F"), times = c(25, 30, 25, 20))))
  expect_equal(train_out$data(cols = c("ord"))[[1]], factor(rep(c("1", "2", "3", "6"), times = c(20, 25, 30, 25)), ordered = TRUE))

  predict_out = op$predict(list(task))[[1]]
  expect_equal(predict_out$data(cols = c("fct"))[[1]], factor(rep(c("A", "B", "D", "F"), times = c(25, 30, 25, 20))))
  expect_equal(predict_out$data(cols = c("ord"))[[1]], factor(rep(c("1", "2", "3", "6"), times = c(20, 25, 30, 25)), ordered = TRUE))

  # test: unseen levels are not touched in predict
  op$param_set$values$no_collapse_above_absolute = Inf
  op$param_set$values$no_collapse_above_prevalence = 1
  df_pred = data.frame(
    target = runif(7),
    fct = factor(LETTERS[1:7]),
    ord = factor(1:7, ordered = TRUE)
  )
  pred_task = TaskRegr$new(df_pred, target = "target", id = "test_pred")
  op$train(list(task))
  predict_out = op$predict(list(pred_task))[[1]]

  expect_equal(predict_out$data(cols = c("fct"))[[1]], factor(c("A", "B", "A", "A", "A", "A", "G")))
  expect_equal(predict_out$data(cols = c("ord"))[[1]], factor(c("2", "2", "3", "3", "3", "3", "7"), ordered = TRUE))
})
