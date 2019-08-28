context("PipeOpEncode")

test_that("PipeOpEncode", {
  task = mlr_tasks$get("boston_housing")

  chaslevels = task$levels()$chas
  townlevels = task$levels()$town

  expect_datapreproc_pipeop_class(PipeOpEncode, task = task)

  expect_datapreproc_pipeop_class(PipeOpEncode, task = mlr_tasks$get("iris"))

  op = PipeOpEncode$new()
  expect_pipeop(op)

  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  # factor cols are removed
  expect_true(all(c("chas", "town") %nin% fn))
  expect_true("factor" %nin% nt$feature_types$type)

  # chas is one-hot encoded
  expect_true(all(sprintf("chas.%s", chaslevels) %in% fn))

  # town is one-hot encoded
  expect_true(all(make.names(sprintf("town.%s", townlevels), unique = TRUE) %in% fn))

  nt = predict_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  # factor cols are removed
  expect_true(all(c("chas", "town") %nin% fn))
  expect_true("factor" %nin% nt$feature_types$type)

  # chas is one-hot encoded
  expect_true(all(sprintf("chas.%s", chaslevels) %in% fn))

  # town is one-hot encoded
  expect_true(all(make.names(sprintf("town.%s", townlevels), unique = TRUE) %in% fn))


  # repeat with method == reference
  op = PipeOpEncode$new()
  op$param_set$values = list(method = "treatment")
  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  nt = predict_pipeop(op, inputs = list(task))[[1L]]
  # factor cols are removed
  expect_true(all(c("chas", "town") %nin% fn))
  expect_true("factor" %nin% nt$feature_types$type)

  # chas is one-hot encoded, without 1st level
  expect_true(all(sprintf("chas.%s", tail(chaslevels, -1L)) %in% fn))
  expect_true(all(sprintf("chas.%s", head(chaslevels, 1L)) %nin% fn))

  # town is one-hot encoded, without 1st level
  expect_true(all(make.names(sprintf("town.%s", tail(townlevels, -1L)), unique = TRUE) %in% fn))
  expect_true(all(make.names(sprintf("town.%s", head(townlevels, 1L)), unique = TRUE) %nin% fn))

  op = PipeOpEncode$new()
  op$param_set$values$method = "helmert"
  nt = train_pipeop(op, inputs = list(task))[[1L]]

  fn = nt$feature_names

  expect_set_equal(fn, c(setdiff(task$feature_names, c("chas", "town")),
    make.names(sprintf("chas.%s", seq_len(length(chaslevels) - 1)), unique = TRUE),
    make.names(sprintf("town.%s", seq_len(length(townlevels) - 1L)), unique = TRUE)))

  op = PipeOpEncode$new()
  op$param_set$values$method = "sum"
  nt = train_pipeop(op, inputs = list(task))[[1L]]

  fn = nt$feature_names

  expect_set_equal(fn, c(setdiff(task$feature_names, c("chas", "town")),
    make.names(sprintf("chas.%s", seq_len(length(chaslevels) - 1)), unique = TRUE),
    make.names(sprintf("town.%s", seq_len(length(townlevels) - 1L)), unique = TRUE)))

  op = PipeOpEncode$new()
  op$param_set$values$method = "poly"
  nt = train_pipeop(op, inputs = list(task))[[1L]]

  fn = nt$feature_names

  expect_set_equal(fn, c(setdiff(task$feature_names, c("chas", "town")),
    make.names(sprintf("chas.%s", seq_len(length(chaslevels) - 1)), unique = TRUE),
    make.names(sprintf("town.%s", seq_len(length(townlevels) - 1L)), unique = TRUE)))
})


test_that("PipeOpEncodeLmer regr", {
  task = mlr_tasks$get("boston_housing")

  chaslevels = task$levels()$chas
  townlevels = task$levels()$town

  expect_datapreproc_pipeop_class(PipeOpEncodeLmer, task = task)

  expect_datapreproc_pipeop_class(PipeOpEncodeLmer, task = mlr_tasks$get("iris"))

  op = PipeOpEncodeLmer$new()
  expect_pipeop(op)

  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  expect_true("factor" %nin% nt$feature_types$type)

  nt = predict_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  # factor cols are removed
  expect_true("factor" %nin% nt$feature_types$type)

})


test_that("PipeOpEncodeLmer", {

  sample_n_letters = function(n, l = 3) {sample(letters[1:l], n, replace = TRUE)}

  task = mlr3::TaskClassif$new("task",
    data.table::data.table(x = sample_n_letters(10), y = sample_n_letters(10), z = 1:10), "x")

  expect_datapreproc_pipeop_class(PipeOpEncodeLmer, task = task)
  op = PipeOpEncodeLmer$new()
  expect_pipeop(op)
  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  # factor cols are removed
  expect_true(all(c("y") %nin% fn))
  expect_true("factor" %nin% nt$feature_types$type)
  # y is encoded
  expect_true(all(sprintf("y.%s", task$levels()$x) %in% fn))
  nt = predict_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names
  # factor cols are removed
  expect_true(all(c("y") %nin% fn))
  expect_true("factor" %nin% nt$feature_types$type)
  # y is encoded
  expect_true(all(sprintf("y.%s", task$levels()$x) %in% fn))


  task = mlr3::TaskClassif$new("task",
    data.table::data.table(x = sample_n_letters(10, 2), y = sample_n_letters(10, 3), z = 1:10), "x")

  expect_datapreproc_pipeop_class(PipeOpEncodeLmer, task = task)
  op = PipeOpEncodeLmer$new()
  expect_pipeop(op)
  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  # factor cols are removed
  expect_true(all(c("y") %nin% fn))
  expect_true("factor" %nin% nt$feature_types$type)
  # y is encoded
  expect_true(all(sprintf("y.%s", task$levels()$x) %in% fn))
  nt = predict_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names
  # factor cols are removed
  expect_true(all(c("y") %nin% fn))
  expect_true("factor" %nin% nt$feature_types$type)
  # y is encoded
  expect_true(all(sprintf("y.%s", task$levels()$x) %in% fn))

})



test_that("PipeOpEncodeLmer Edge Cases", {
  task = mlr3::TaskClassif$new("task",
    data.table::data.table(x = sample_n_letters(10, 2), y = 1:10, z = 1:10), "x")

  expect_datapreproc_pipeop_class(PipeOpEncodeLmer, task = task)
  op = PipeOpEncodeLmer$new()
  expect_pipeop(op)
  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  # factor cols are removed
  expect_true(all(c("y") %nin% fn))
  expect_true("factor" %nin% nt$feature_types$type)
  # y is encoded
  expect_true(all(sprintf("y.%s", task$levels()$x) %in% fn))
  nt = predict_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names
  # factor cols are removed
  expect_true(all(c("y") %nin% fn))
  expect_true("factor" %nin% nt$feature_types$type)
  # y is encoded
  expect_true(all(sprintf("y.%s", task$levels()$x) %in% fn))
})