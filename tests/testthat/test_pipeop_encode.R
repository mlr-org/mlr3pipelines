context("PipeOpEncode")

test_that("PipeOpEncode", {
  task = mlr_tasks$get("boston_housing")

  td = task$data()
  td$town[1] = NA
  td$chas[2] = NA
  natask = TaskRegr$new("boston_housing_na", td, task$target_names)

  check_dat = function(dat) {
    expect_true(all(is.na(dat[1, grep("^town\\.", colnames(dat), value = TRUE), with = FALSE])))
    expect_true(!any(is.na(dat[-1, grep("^town\\.", colnames(dat), value = TRUE), with = FALSE])))
    expect_true(all(is.na(dat[2, grep("^chas\\.", colnames(dat), value = TRUE), with = FALSE])))
    expect_true(!any(is.na(dat[-2, grep("^chas\\.", colnames(dat), value = TRUE), with = FALSE])))
  }

  chaslevels = task$levels()$chas
  townlevels = task$levels()$town

  expect_datapreproc_pipeop_class(PipeOpEncode, task = task)

  expect_datapreproc_pipeop_class(PipeOpEncode, task = natask)

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

  # NAs are handled well
  check_dat(op$predict(list(natask))[[1]]$data())
  check_dat(op$train(list(natask))[[1]]$data())
  check_dat(op$predict(list(natask))[[1]]$data())

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
  check_dat(op$predict(list(natask))[[1]]$data())
  check_dat(op$train(list(natask))[[1]]$data())
  check_dat(op$predict(list(natask))[[1]]$data())

  op = PipeOpEncode$new()
  op$param_set$values$method = "sum"
  nt = train_pipeop(op, inputs = list(task))[[1L]]

  fn = nt$feature_names

  expect_set_equal(fn, c(setdiff(task$feature_names, c("chas", "town")),
    make.names(sprintf("chas.%s", seq_len(length(chaslevels) - 1)), unique = TRUE),
    make.names(sprintf("town.%s", seq_len(length(townlevels) - 1L)), unique = TRUE)))
  check_dat(op$predict(list(natask))[[1]]$data())
  check_dat(op$train(list(natask))[[1]]$data())
  check_dat(op$predict(list(natask))[[1]]$data())

  op = PipeOpEncode$new()
  op$param_set$values$method = "poly"
  nt = train_pipeop(op, inputs = list(task))[[1L]]

  fn = nt$feature_names

  expect_set_equal(fn, c(setdiff(task$feature_names, c("chas", "town")),
    make.names(sprintf("chas.%s", seq_len(length(chaslevels) - 1)), unique = TRUE),
    make.names(sprintf("town.%s", seq_len(length(townlevels) - 1L)), unique = TRUE)))
  check_dat(op$predict(list(natask))[[1]]$data())
  check_dat(op$train(list(natask))[[1]]$data())
  check_dat(op$predict(list(natask))[[1]]$data())

})
