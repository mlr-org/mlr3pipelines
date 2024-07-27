context("PipeOpRowApply")

test_that("PipeOpRowApply - basic properties", {

  op = PipeOpRowApply$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)

  expect_datapreproc_pipeop_class(PipeOpRowApply, task = task)

  expect_equal(op$train(list(task))[[1]]$nrow, task$nrow)
  expect_equal(op$predict(list(task))[[1]]$nrow, task$nrow)

})

test_that("PipeOpRowApply - transform works on task with only numeric features", {

  op = PipeOpRowApply$new()
  task = mlr_tasks$get("iris")

  # applicator generates matrix with names
  applicator = function(x) x^2
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(t(apply(iris[1:4], 1, applicator)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(t(apply(iris[1:4], 1, applicator)))
  )

  # applicator generates matrix without names but same number of columns (should keep names)
  applicator = scale
  op$param_set$values$applicator = applicator
  result = as.data.table(t(apply(iris[1:4], 1, applicator)))
  setNames(result, colnames(iris[1:4]))

  expect_equal(
    op$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    result
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    result
  )

  # applicator generates matrix without names but different number of columns (should generate new names)
  applicator = function(x) rep(sum(x), 2)
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = c("V1", "V2")),
    as.data.table(t(apply(iris[1:4], 1, applicator)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = c("V1", "V2")),
    as.data.table(t(apply(iris[1:4], 1, applicator)))
  )


  # applicator generates vector (should generate new name)
  applicator = sum
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = "V1"),
    as.data.table(t(matrix(apply(iris[1:4], 1, applicator), nrow = 1)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = "V1"),
    as.data.table(t(matrix(apply(iris[1:4], 1, applicator), nrow = 1)))
  )

  # error if apply generates anything but a matrix or vector (e.g. non-simplifiable list)
  applicator = function(x) if(mean(x) < 3) c(x[[1]], x[[2]]) else x[[1]]
  op$param_set$values$applicator = applicator

  expect_error(op$train(list(task)))
  expect_error(op$predict(list(task)))

  # col_prefix


})


test_that("PipeOpRowApply - transform works on task with only integer features", {

})


test_that("PipeOpRowApply - transform works on task with both numeric and integer features", {

})


test_that("PipeOpRowApply - transform works on task with only one row", {

})


test_that("PipeOpRowApply - transform works on empty task", {

  task = tsk("iris")$filter(0L)
  po = PipeOpRowApply$new()
  po$param_set$values$applicator = function(x) as.integer(x)

  train_out = po$train(list(task))[[1L]]
  expect_data_table(train_out$data(), nrows = 0L)
  expect_true(all(train_out$feature_types$type == "integer"))

  predict_out = po$predict(list(task))[[1L]]
  expect_data_table(predict_out$data(), nrows = 0L)
  expect_true(all(predict_out$feature_types$type == "integer"))

})


