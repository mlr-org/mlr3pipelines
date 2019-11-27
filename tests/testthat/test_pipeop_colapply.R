context("PipeOpColApply")

test_that("apply general tests", {

  op = PipeOpColApply$new()
  expect_pipeop(op)

  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpColApply, task = task,
    constargs = list(param_vals = list(applicator = as.character)))

  expect_datapreproc_pipeop_class(PipeOpColApply, task = mlr_tasks$get("pima"),
    constargs = list(param_vals = list(applicator = as.numeric)))

})

test_that("apply results look as they should", {

  po = PipeOpColApply$new()
  task = mlr_tasks$get("iris")

  po$param_set$values = list(applicator = as.character)

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(do.call(cbind, lapply(iris[1:4], as.character)))
  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(do.call(cbind, lapply(iris[1:4], as.character)))
  )

  po$param_set$values = list(applicator = Vectorize(as.character))

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(do.call(cbind, lapply(iris[1:4], as.character)))
  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(do.call(cbind, lapply(iris[1:4], as.character)))
  )

  po$param_set$values = list(applicator = function(x) x^2)

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(do.call(cbind, lapply(iris[1:4], function(x) x^2)))
  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(do.call(cbind, lapply(iris[1:4], function(x) x^2)))
  )

  po$param_set$values = list(applicator = Vectorize(function(x) x^2))

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(do.call(cbind, lapply(iris[1:4], function(x) x^2)))
  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(do.call(cbind, lapply(iris[1:4], function(x) x^2)))
  )

  tomean = function(x) rep(mean(x), length(x))

  po$param_set$values = list(applicator = tomean)

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(do.call(cbind, lapply(iris[1:4], tomean)))
  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(do.call(cbind, lapply(iris[1:4], tomean)))
  )

  po$param_set$values = list(applicator = Vectorize(tomean))

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(iris[1:4])
  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    as.data.table(iris[1:4])
  )

  po$param_set$values = list(applicator = as.character, affect_columns = selector_grep("^Sepal"))

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    cbind(as.data.table(do.call(cbind, lapply(iris[1:2], as.character))), iris[3:4])
  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    cbind(as.data.table(do.call(cbind, lapply(iris[1:2], as.character))), iris[3:4])
  )

  po$param_set$values = list(applicator = Vectorize(as.character), affect_columns = selector_grep("^Sepal"))

  expect_equal(
    po$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    cbind(as.data.table(do.call(cbind, lapply(iris[1:2], as.character))), iris[3:4])
  )

  expect_equal(
    po$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    cbind(as.data.table(do.call(cbind, lapply(iris[1:2], as.character))), iris[3:4])
  )
})
