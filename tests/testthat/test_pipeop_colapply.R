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

test_that("empty task", {

  task = tsk("iris")$filter(0L)
  po = PipeOpColApply$new()
  po$param_set$values$applicator = function(x) as.integer(x)

  train_out = po$train(list(task))[[1L]]
  expect_data_table(train_out$data(), nrows = 0L)
  expect_true(all(train_out$feature_types$type == "integer"))

  predict_out = po$predict(list(task))[[1L]]
  expect_data_table(predict_out$data(), nrows = 0L)
  expect_true(all(predict_out$feature_types$type == "integer"))
})

test_that("multiple column output", {

  task = tsk("iris")
  po = PipeOpColApply$new()

  # cbind, not named (.V1, .V2 is due to as.data.table coercion within $transform_dt)
  po$param_set$values$applicator = function(x) cbind(floor(x), ceiling(x))
  train_out = po$train(list(task))[[1L]]
  expect_setequal(train_out$feature_names, as.vector(t(outer(task$feature_names, c(".V1", ".V2"), FUN = "paste0"))))
  expect_equal(
    train_out$data(cols = train_out$feature_names),
    as.data.table(map(task$data(cols = task$feature_names), .f = function(x) cbind(floor(x), ceiling(x))))
  )
  expect_equal(train_out, po$predict(list(task))[[1L]])

  # cbind, named
  po$param_set$values$applicator = function(x) cbind(floor = floor(x), ceiling = ceiling(x))
  train_out = po$train(list(task))[[1L]]
  expect_setequal(train_out$feature_names, as.vector(t(outer(task$feature_names, c(".floor", ".ceiling"), FUN = "paste0"))))
  expect_equal(
    train_out$data(cols = train_out$feature_names),
    as.data.table(map(task$data(cols = task$feature_names), .f = function(x) cbind(floor = floor(x), ceiling = ceiling(x))))
  )
  expect_equal(train_out, po$predict(list(task))[[1L]])

  # data.frame, not named (data.frame does its own automatic column name generation)
  po$param_set$values$applicator = function(x) data.frame(floor(x), ceiling(x))
  train_out = po$train(list(task))[[1L]]
  expect_setequal(train_out$feature_names, as.vector(t(outer(task$feature_names, c(".floor.x.", ".ceiling.x."), FUN = "paste0"))))
  expect_equal(
    train_out$data(cols = train_out$feature_names),
    as.data.table(map(task$data(cols = task$feature_names), .f = function(x) data.frame(floor(x), ceiling(x))))
  )
  expect_equal(train_out, po$predict(list(task))[[1L]])

  # data.frame, named
  po$param_set$values$applicator = function(x) data.frame(floor = floor(x), ceiling = ceiling(x))
  train_out = po$train(list(task))[[1L]]
  expect_setequal(train_out$feature_names, as.vector(t(outer(task$feature_names, c(".floor", ".ceiling"), FUN = "paste0"))))
  expect_equal(
    train_out$data(cols = train_out$feature_names),
    as.data.table(map(task$data(cols = task$feature_names), .f = function(x) data.frame(floor = floor(x), ceiling = ceiling(x))))
  )
  expect_equal(train_out, po$predict(list(task))[[1L]])

  # data.table, not named (data.table does its own automatic column name generation)
  po$param_set$values$applicator = function(x) data.table(floor(x), ceiling(x))
  train_out = po$train(list(task))[[1L]]
  expect_setequal(train_out$feature_names, as.vector(t(outer(task$feature_names, c(".V1", ".V2"), FUN = "paste0"))))
  expect_equal(
    train_out$data(cols = train_out$feature_names),
    as.data.table(map(task$data(cols = task$feature_names), .f = function(x) data.table(floor(x), ceiling(x))))
  )
  expect_equal(train_out, po$predict(list(task))[[1L]])

  # data.table, named
  po$param_set$values$applicator = function(x) data.table(floor = floor(x), ceiling = ceiling(x))
  train_out = po$train(list(task))[[1L]]
  expect_setequal(train_out$feature_names, as.vector(t(outer(task$feature_names, c(".floor", ".ceiling"), FUN = "paste0"))))
  expect_equal(
    train_out$data(cols = train_out$feature_names),
    as.data.table(map(task$data(cols = task$feature_names), .f = function(x) data.table(floor = floor(x), ceiling = ceiling(x))))
  )
  expect_equal(train_out, po$predict(list(task))[[1L]])
})
