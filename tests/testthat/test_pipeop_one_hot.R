context("PipeOpOneHot")

test_that("PipeOpOneHot", {

  task = mlr_tasks$get("bh")

  expect_datapreproc_pipeop_class(PipeOpOneHot, task = task)

  expect_datapreproc_pipeop_class(PipeOpOneHot, task = mlr_tasks$get("iris"))

  op = PipeOpOneHot$new()
  expect_pipeop(op)

  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  # factor cols are removed
  expect_true(all(c("chas", "town") %nin% fn))
  expect_true("factor" %nin% nt$feature_types$type)

  # chas is one-hot encoded
  expect_true(all(sprintf("chas.%s", task$levels("chas")) %in% fn))

  # town is one-hot encoded
  expect_true(all(make.names(sprintf("town.%s", task$levels("town")), unique = TRUE) %in% fn))

  nt = predict_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  # factor cols are removed
  expect_true(all(c("chas", "town") %nin% fn))
  expect_true("factor" %nin% nt$feature_types$type)

  # chas is one-hot encoded
  expect_true(all(sprintf("chas.%s", task$levels("chas")) %in% fn))

  # town is one-hot encoded
  expect_true(all(make.names(sprintf("town.%s", task$levels("town")), unique = TRUE) %in% fn))


  # repeat with method == reference
  op = PipeOpOneHot$new()
  op$param_set$values = list(method = "treatment")
  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  nt = predict_pipeop(op, inputs = list(task))[[1L]]
  # factor cols are removed
  expect_true(all(c("chas", "town") %nin% fn))
  expect_true("factor" %nin% nt$feature_types$type)

  # chas is one-hot encoded, without 1st level
  expect_true(all(sprintf("chas.%s", tail(task$levels("chas"), -1L)) %in% fn))
  expect_true(all(sprintf("chas.%s", head(task$levels("chas"), 1L)) %nin% fn))

  # town is one-hot encoded, without 1st level
  expect_true(all(make.names(sprintf("town.%s", tail(task$levels("town"), -1L)), unique = TRUE) %in% fn))
  expect_true(all(make.names(sprintf("town.%s", head(task$levels("town"), 1L)), unique = TRUE) %nin% fn))

  op = PipeOpOneHot$new()
  op$param_set$values$method = "helmert"
  nt = train_pipeop(op, inputs = list(task))[[1L]]

  fn = nt$feature_names

  expect_set_equal(fn, c(setdiff(task$feature_names, c("chas", "town")),
    make.names(sprintf("chas.%s", seq_len(length(task$levels("chas")) -1)), unique = TRUE),
    make.names(sprintf("town.%s", seq_len(length(task$levels("town")) -1L)), unique = TRUE)))

  op = PipeOpOneHot$new()
  op$param_set$values$method = "sum"
  nt = train_pipeop(op, inputs = list(task))[[1L]]

  fn = nt$feature_names

  expect_set_equal(fn, c(setdiff(task$feature_names, c("chas", "town")),
    make.names(sprintf("chas.%s", seq_len(length(task$levels("chas")) -1)), unique = TRUE),
    make.names(sprintf("town.%s", seq_len(length(task$levels("town")) -1L)), unique = TRUE)))

  op = PipeOpOneHot$new()
  op$param_set$values$method = "poly"
  nt = train_pipeop(op, inputs = list(task))[[1L]]

  fn = nt$feature_names

  expect_set_equal(fn, c(setdiff(task$feature_names, c("chas", "town")),
    make.names(sprintf("chas.%s", seq_len(length(task$levels("chas")) -1)), unique = TRUE),
    make.names(sprintf("town.%s", seq_len(length(task$levels("town")) -1L)), unique = TRUE)))


})
