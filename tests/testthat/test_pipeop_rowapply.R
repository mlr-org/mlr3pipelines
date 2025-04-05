context("PipeOpRowApply")

test_that("PipeOpRowApply - basic properties", {
  op = PipeOpRowApply$new()
  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpRowApply, task = task)
})

test_that("PipeOpRowApply - transform on task with only numeric features", {
  op = PipeOpRowApply$new()
  task = mlr_tasks$get("iris")
  cnames = task$feature_names
  iris = task$data(cols = cnames)

  # applicator generates matrix with names
  applicator = function(x) x^2
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = cnames),
    as.data.table(t(apply(iris, 1, applicator)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = cnames),
    as.data.table(t(apply(iris, 1, applicator)))
  )

  # applicator generates matrix without names but same number of columns (should keep names)
  applicator = as.integer
  op$param_set$values$applicator = applicator
  result = as.data.table(t(apply(iris, 1, applicator)))
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
    as.data.table(t(apply(iris, 1, applicator)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = c("V1", "V2")),
    as.data.table(t(apply(iris, 1, applicator)))
  )

  # applicator generates vector (should generate new name)
  applicator = sum
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(cols = "V1"),
    as.data.table(t(matrix(apply(iris, 1, applicator), nrow = 1)))
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(cols = "V1"),
    as.data.table(t(matrix(apply(iris, 1, applicator), nrow = 1)))
  )

  # applicator generates empty output
  applicator = function(x) numeric(0)
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(),
    task$data(cols = task$target_names)
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(),
    task$data(cols = task$target_names)
  )

  # error if apply generates anything but a matrix or vector (e.g. non-simplifiable list)
  applicator = function(x) if(mean(x) < 3) c(x[[1]], x[[2]]) else x[[1]]
  op$param_set$values$applicator = applicator

  expect_error(op$train(list(task)))
  expect_error(op$predict(list(task)))

  # col_prefix
  op$param_set$values$applicator = function(x) x^2
  op$param_set$values$col_prefix = "applied"
  cnames = paste("applied", task$feature_names, sep = ".")

  expect_equal(op$train(list(task))[[1]]$feature_names, cnames)
  expect_equal(op$predict(list(task))[[1]]$feature_names, cnames)

})


test_that("PipeOpRowApply - transform works on task with only integer features", {

  op = PipeOpRowApply$new()
  task = mlr_tasks$get("german_credit")$select(c("age", "amount", "duration"))
  cnames = task$feature_names
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
  applicator = as.numeric
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

  # applicator generates empty output
  applicator = function(x) numeric(0)
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(),
    task$data(cols = task$target_names)
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(),
    task$data(cols = task$target_names)
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

  expect_equal(op$train(list(task))[[1]]$feature_names, cnames)
  expect_equal(op$predict(list(task))[[1]]$feature_names, cnames)

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

  # applicator generates empty output
  applicator = function(x) numeric(0)
  op$param_set$values$applicator = applicator

  expect_equal(
    op$train(list(task))[[1]]$data(),
    task$data(cols = task$target_names)
  )
  expect_equal(
    op$predict(list(task))[[1]]$data(),
    task$data(cols = task$target_names)
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
  cnames = task$feature_names

  # applicator generates matrix with names
  applicator = function(x) x^2
  op$param_set$values$applicator = applicator

  train_out = op$train(list(task))[[1]]
  expect_data_table(train_out$data(), nrows = 0)
  expect_set_equal(train_out$feature_names, cnames)

  predict_out = op$predict(list(task))[[1]]
  expect_data_table(predict_out$data(), nrows = 0)
  expect_set_equal(predict_out$feature_names, cnames)

  # applicator generates matrix without names but same number of columns (should keep names)
  applicator = scale
  op$param_set$values$applicator = applicator

  train_out = op$train(list(task))[[1]]
  expect_data_table(train_out$data(), nrows = 0)
  expect_set_equal(train_out$feature_names, cnames)

  predict_out = op$predict(list(task))[[1]]
  expect_data_table(predict_out$data(), nrows = 0)
  expect_set_equal(predict_out$feature_names, cnames)

  # applicator generates matrix without names but different number of columns (should generate new names)
  applicator = function(x) rep(sum(x), 2)
  op$param_set$values$applicator = applicator

  train_out = op$train(list(task))[[1]]
  expect_data_table(train_out$data(), nrows = 0)
  expect_set_equal(train_out$feature_names, c("V1", "V2"))

  predict_out = op$predict(list(task))[[1]]
  expect_data_table(predict_out$data(), nrows = 0)
  expect_set_equal(predict_out$feature_names, c("V1", c("V2")))

  # applicator generates vector (should generate new name)
  applicator = sum
  op$param_set$values$applicator = applicator

  train_out = op$train(list(task))[[1]]
  expect_data_table(train_out$data(), nrows = 0)
  expect_set_equal(train_out$feature_names, "V1")

  predict_out = op$predict(list(task))[[1]]
  expect_data_table(predict_out$data(), nrows = 0)
  expect_set_equal(predict_out$feature_names, "V1")

  # applicator changes feature type
  applicator = as.integer
  op$param_set$values$applicator = applicator

  train_out = op$train(list(task))[[1]]
  expect_data_table(train_out$data(), nrows = 0)
  expect_set_equal(train_out$feature_names, cnames)
  expect_true(all(train_out$feature_types$type == "integer"))

  predict_out = op$predict(list(task))[[1]]
  expect_data_table(predict_out$data(), nrows = 0)
  expect_set_equal(predict_out$feature_names, cnames)
  expect_true(all(predict_out$feature_types$type == "integer"))

})


test_that("PipeOpRowApply - transform works for empty predict task (no rows)", {

  op = PipeOpRowApply$new()
  task_train = mlr_tasks$get("wine")
  task_predict = task_train$filter(0)
  cnames = task_train$feature_names

  # applicator generates matrix with names
  applicator = function(x) x^2
  op$param_set$values$applicator = applicator

  op$train(list(task_train))
  predict_out = op$predict(list(task_predict))[[1]]
  expect_data_table(predict_out$data(), nrows = 0)
  expect_set_equal(predict_out$feature_names, cnames)

  # applicator generates matrix without names but same number of columns (should keep names)
  applicator = scale
  op$param_set$values$applicator = applicator

  op$train(list(task_train))
  predict_out = op$predict(list(task_predict))[[1]]
  expect_data_table(predict_out$data(), nrows = 0)
  expect_set_equal(predict_out$feature_names, cnames)

  # applicator generates matrix without names but different number of columns (should generate new names)
  applicator = function(x) rep(sum(x), 2)
  op$param_set$values$applicator = applicator

  op$train(list(task_train))
  predict_out = op$predict(list(task_predict))[[1]]
  expect_data_table(predict_out$data(), nrows = 0)
  expect_set_equal(predict_out$feature_names, c("V1", c("V2")))

  # applicator generates vector (should generate new name)
  applicator = sum
  op$param_set$values$applicator = applicator

  op$train(list(task_train))
  predict_out = op$predict(list(task_predict))[[1]]
  expect_data_table(predict_out$data(), nrows = 0)
  expect_set_equal(predict_out$feature_names, "V1")

  # applicator changes feature type
  applicator = as.integer
  op$param_set$values$applicator = applicator

  op$train(list(task_train))
  predict_out = op$predict(list(task_predict))[[1]]
  expect_data_table(predict_out$data(), nrows = 0)
  expect_set_equal(predict_out$feature_names, cnames)
  expect_true(all(predict_out$feature_types$type == "integer"))

})

test_that("PipeOpRowApply - transform works on task with no numeric or integer columns", {

  op = PipeOpRowApply$new()
  task = mlr_tasks$get("penguins")$select(c("island", "sex"))
  cnames = task$feature_names

  op$param_set$values$applicator = as.integer

  expect_equal(
    op$train(list(task))[[1]],
    task
  )
  expect_equal(
    op$predict(list(task))[[1]],
    task
  )

})
