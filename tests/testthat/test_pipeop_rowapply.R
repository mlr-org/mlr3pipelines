context("PipeOpRowApply")

test_that("PipeOpRowApply - basic properties", {

  op = PipeOpRowApply$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)

  expect_datapreproc_pipeop_class(PipeOpRowApply, task = task)

})

test_that("PipeOpRowApply - transform on task with only numeric features", {

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
  setnames(result, colnames(iris[1:4]))

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

  # applicator is as.integer
  applicator = as.integer
  op$param_set$values$applicator = applicator
  result = as.data.table(t(apply(iris[1:4], 1, applicator)))
  setnames(result, colnames(iris[1:4]))

  expect_equal(
    op$train(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    result
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = colnames(iris[1:4])),
    result
  )

  # predict task has 0 rows
  task_predict = task$filter(0L)

  expect_equal(
    op$predict(list(task_predict))[[1]]$data(cols = cnames),

  )

  # error if apply generates anything but a matrix or vector (e.g. non-simplifiable list)
  applicator = function(x) if(mean(x) < 3) c(x[[1]], x[[2]]) else x[[1]]
  op$param_set$values$applicator = applicator

  expect_error(op$train(list(task)))
  expect_error(op$predict(list(task)))

  # col_prefix
  op$param_set$values$applicator = function(x) x^2
  op$param_set$values$col_prefix = "applied"
  cnames = paste("applied", colnames(iris[1:4]), sep = ".")

  expect_equal(
    op$train(list(task))[[1]]$feature_names,
    cnames
  )
  expect_equal(
    op$predict(list(task))[[1]]$feature_names,
    cnames
  )

})


test_that("PipeOpRowApply - transform works on task with only integer features", {

  op = PipeOpRowApply$new()
  task = mlr_tasks$get("german_credit")
  cnames = c("age", "amount", "duration")
  task$select(cnames)
  german_credit = task$data(cols = cnames)

  # applicator generates matrix with names
  applicator = function(x) x^2
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = cnames),
    as.data.table(t(apply(german_credit, 1, applicator)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = cnames),
    as.data.table(t(apply(german_credit, 1, applicator)))
  )

  # applicator generates matrix without names but same number of columns (should keep names)
  applicator = scale
  op$param_set$values$applicator = applicator
  result = as.data.table(t(apply(german_credit, 1, applicator)))
  setnames(result, cnames)

  expect_equal(
    op$train(list(task))[[1]]$data(cols = cnames),
    result
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = cnames),
    result
  )

  # applicator generates matrix without names but different number of columns (should generate new names)
  applicator = function(x) rep(sum(x), 2)
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = c("V1", "V2")),
    as.data.table(t(apply(german_credit, 1, applicator)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = c("V1", "V2")),
    as.data.table(t(apply(german_credit, 1, applicator)))
  )

  # applicator generates vector (should generate new name)
  applicator = sum
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = "V1"),
    as.data.table(t(matrix(apply(german_credit, 1, applicator), nrow = 1)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = "V1"),
    as.data.table(t(matrix(apply(german_credit, 1, applicator), nrow = 1)))
  )

  # applicator is as.numeric
  applicator = as.integer
  op$param_set$values$applicator = applicator
  result = as.data.table(t(apply(german_credit, 1, applicator)))
  setnames(result, cnames)

  expect_equal(
    op$train(list(task))[[1]]$data(cols = cnames),
    result
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = cnames),
    result
  )

  # predict task has 0 rows
  task_predict = task$filter(0L)

  expect_equal(
    op$predict(list(task_predict))[[1]]$data(cols = cnames),

  )

  # error if apply generates anything but a matrix or vector (e.g. non-simplifiable list)
  applicator = function(x) if(mean(x) < 1000) c(x[[1]], x[[2]]) else x[[1]]
  op$param_set$values$applicator = applicator

  expect_error(op$train(list(task)))
  expect_error(op$predict(list(task)))

  # col_prefix
  op$param_set$values$applicator = function(x) x^2
  op$param_set$values$col_prefix = "applied"
  cnames = paste("applied", cnames, sep = ".")

  expect_equal(
    op$train(list(task))[[1]]$feature_names,
    cnames
  )
  expect_equal(
    op$predict(list(task))[[1]]$feature_names,
    cnames
  )

})


test_that("PipeOpRowApply - transform works on task with both numeric and integer features", {

  op = PipeOpRowApply$new()
  task = mlr_tasks$get("wine")
  cnames = task$feature_names
  wine = task$data(cols = cnames)

  # applicator generates matrix with names
  applicator = function(x) x^2
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = cnames),
    as.data.table(t(apply(wine, 1, applicator)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = cnames),
    as.data.table(t(apply(wine, 1, applicator)))
  )

  # applicator generates matrix without names but same number of columns (should keep names)
  applicator = scale
  op$param_set$values$applicator = applicator
  result = as.data.table(t(apply(wine, 1, applicator)))
  setnames(result, cnames)

  expect_equal(
    op$train(list(task))[[1]]$data(cols = cnames),
    result
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = cnames),
    result
  )

  # applicator generates matrix without names but different number of columns (should generate new names)
  applicator = function(x) rep(sum(x), 2)
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = c("V1", "V2")),
    as.data.table(t(apply(wine, 1, applicator)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = c("V1", "V2")),
    as.data.table(t(apply(wine, 1, applicator)))
  )

  # applicator generates vector (should generate new name)
  applicator = sum
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = "V1"),
    as.data.table(t(matrix(apply(wine, 1, applicator), nrow = 1)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = "V1"),
    as.data.table(t(matrix(apply(wine, 1, applicator), nrow = 1)))
  )

  # applicator is as.integer
  applicator = as.integer
  op$param_set$values$applicator = applicator
  result = as.data.table(t(apply(wine, 1, applicator)))
  setnames(result, cnames)

  expect_equal(
    op$train(list(task))[[1]]$data(cols = cnames),
    result
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = cnames),
    result
  )

  # applicator is as.numeric
  applicator = as.numeric
  op$param_set$values$applicator = applicator
  result = as.data.table(t(apply(wine, 1, applicator)))
  setnames(result, cnames)

  expect_equal(
    op$train(list(task))[[1]]$data(cols = cnames),
    result
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = cnames),
    result
  )

  # predict task has 0 rows
  task_predict = task$filter(0L)

  expect_equal(
    op$predict(list(task_predict))[[1]]$data(cols = cnames),

  )

  # error if apply generates anything but a matrix or vector (e.g. non-simplifiable list)
  applicator = function(x) if(mean(x) < 50) c(x[[1]], x[[2]]) else x[[1]]
  op$param_set$values$applicator = applicator

  expect_error(op$train(list(task)))
  expect_error(op$predict(list(task)))

  # col_prefix
  op$param_set$values$applicator = function(x) x^2
  op$param_set$values$col_prefix = "applied"
  cnames = paste("applied", cnames, sep = ".")

  expect_equal(
    op$train(list(task))[[1]]$feature_names,
    cnames
  )
  expect_equal(
    op$predict(list(task))[[1]]$feature_names,
    cnames
  )

})


test_that("PipeOpRowApply - transform works on task with only one row", {

  op = PipeOpRowApply$new()
  task = mlr_tasks$get("wine")$filter(1)
  cnames = task$feature_names
  wine = task$data(cols = cnames)

  # applicator generates matrix with names
  applicator = function(x) x^2
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = cnames),
    as.data.table(t(apply(wine, 1, applicator)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = cnames),
    as.data.table(t(apply(wine, 1, applicator)))
  )

  # applicator generates matrix without names but same number of columns (should keep names)
  applicator = scale
  op$param_set$values$applicator = applicator
  result = as.data.table(t(apply(wine, 1, applicator)))
  setnames(result, cnames)

  expect_equal(
    op$train(list(task))[[1]]$data(cols = cnames),
    result
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = cnames),
    result
  )

  # applicator generates matrix without names but different number of columns (should generate new names)
  applicator = function(x) rep(sum(x), 2)
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = c("V1", "V2")),
    as.data.table(t(apply(wine, 1, applicator)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = c("V1", "V2")),
    as.data.table(t(apply(wine, 1, applicator)))
  )

  # applicator generates vector (should generate new name)
  applicator = sum
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = "V1"),
    as.data.table(t(matrix(apply(wine, 1, applicator), nrow = 1)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = "V1"),
    as.data.table(t(matrix(apply(wine, 1, applicator), nrow = 1)))
  )

})

test_that("PipeOpRowApply - transform works on empty task (no rows)", {

  op = PipeOpRowApply$new()
  task = mlr_tasks$get("wine")$filter(0)

  # applicator is as.integer
  applicator = as.integer
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(),
    task$data()
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(),
    task$data()
  )

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


