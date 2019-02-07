context("PipeOpOneHot")

test_that("PipeOpOneHot", {
  task = mlr_tasks$get("bh")
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
  op$param_set$param_vals = list(method = "reference")
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
})
