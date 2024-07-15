context("PipeOpRowApply")

test_that("apply general tests", {

  op = PipeOpRowApply$new()
  expect_pipeop(op)

  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpRowApply, task = task,
                                  constargs = list(param_vals = list(applicator = as.integer)))

  expect_datapreproc_pipeop_class(PipeOpRolApply, task = mlr_tasks$get("pima"),
                                  constargs = list(param_vals = list(applicator = as.numeric)))

})


test_that("apply results look as they should", {

  po = PipeOpRowApply$new()
  task = mlr_tasks$get("iris")

  po$param_set$values = list(applicator = as.character)

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )

  po$param_set$values = list(applicator = function(x) x^2)

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )

  tomean = function(x) rep(mean(x), length(x))

  po$param_set$values = list(applicator = tomean)

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )

  po$param_set$values = list(applicator = as.character, affect_columns = selector_grep("^Sepal"))

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )

  po$param_set$values = list(applicator = Vectorize(as.character), affect_columns = selector_grep("^Sepal"))

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),

  )
})

test_that("empty task", {

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
