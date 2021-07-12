context("PipeOpVtreat")

test_that("PipeOpVtreat - basic properties", {
  skip_if_not_installed("vtreat")

  expect_pipeop(PipeOpVtreat$new())

  task_regr = mlr_tasks$get("boston_housing")
  expect_datapreproc_pipeop_class(PipeOpVtreat, task = task_regr, deterministic_train = FALSE, deterministic_predict = FALSE)

  task_classiftc = mlr_tasks$get("pima")
  expect_datapreproc_pipeop_class(PipeOpVtreat, task = task_classiftc, deterministic_train = FALSE, deterministic_predict = FALSE)

  task_classifmc = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpVtreat, task = task_classifmc, deterministic_train = FALSE, deterministic_predict = FALSE)
})

test_that("PipeOpVtreat - Regression", {
  skip_if_not_installed("vtreat")

  op = PipeOpVtreat$new()

  # clean task simply passes through
  clean_task = TaskRegr$new("vtreat_regr_clean", backend = data.frame(y = c(-1, 0, 1), x = c(1, 1, 1)), target = "y")

  expect_equal(op$train(list(clean_task))[[1L]]$data(), op$predict(list(clean_task))[[1L]]$data())
  expect_null(op$state$treatment_plan)

  # toy task as here: https://github.com/WinVector/vtreat/blob/master/Examples/Regression/Regression_FP.md
  set.seed(2020)

  make_data <- function(nrows) {
      d <- data.frame(x = 5 * rnorm(nrows))
      d["y"] = sin(d[["x"]]) + 0.01 * d[["x"]] + 0.1 * rnorm(nrows)
      d[4:10, "x"] = NA  # introduce NAs
      d["xc"] = paste0("level_", 5 * round(d$y / 5, 1))
      d["x2"] = rnorm(nrows)
      d[d["xc"] == "level_-1", "xc"] = NA  # introduce a NA level
      return(d)
  }

  task = TaskRegr$new("vtreat_regr", backend = make_data(100), target = "y")

  train_out1 = op$train(list(task))[[1L]]
  settings = op$state$treatment_plan$settings
  expect_true(op$state$treatment_plan$treatment_type == "NumericOutcomeTreatment")
  expect_true(all(settings$var_list == task$feature_names))
  expect_true(all(settings$outcome_name == task$target_names))
  expect_true(length(settings$cols_to_copy) == 0L)
  expect_equivalent(settings$params, vtreat::regression_parameters(list(check_for_duplicate_frames = FALSE)))
  predict_out1 = op$predict(list(task))[[1L]]
  expect_false(identical(train_out1$data(), predict_out1$data()))

  op$param_set$values = list(recommended = FALSE,
    cols_to_copy = selector_name("xc"),
    minFraction = 0.1,
    imputation_map = list("x" = 0),
    affect_columns = selector_name(c("x", "xc")))
  train_out2 = op$train(list(task))[[1L]]
  expect_true(op$state$treatment_plan$settings$var_list == "x")
  expect_true(all(c("x2", "xc") %in% train_out2$feature_names))
  expect_true("y" %nin% train_out2$feature_names)
  expect_true(op$state$treatment_plan$settings$params$minFraction == 0.1)
  expect_true(train_out2$missings("x") == 0)
})

test_that("PipeOpVtreat - Binary Classification", {
  skip_if_not_installed("vtreat")

  op = PipeOpVtreat$new()

  # clean task simply passes through
  clean_task = TaskClassif$new("vtreat_binary_clean", backend = data.frame(y = as.factor(c(0, 0, 1, 1, 0)), x = c(1, 1, 1, 1, 1)), target = "y")

  expect_equal(op$train(list(clean_task))[[1L]]$data(), op$predict(list(clean_task))[[1L]]$data())
  expect_null(op$state$treatment_plan)

  # toy task as here: https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification_FP.md
  set.seed(2020)

  make_data <- function(nrows) {
      d <- data.frame(x = 5 * rnorm(nrows))
      d["y"] = sin(d[["x"]]) + 0.01 * d[["x"]] + 0.1 * rnorm(nrows)
      d[4:10, "x"] = NA  # introduce NAs
      d["xc"] = paste0("level_", 5 * round(d$y / 5, 1))
      d["x2"] = rnorm(nrows)
      d[d["xc"] == "level_-1", "xc"] = NA  # introduce a NA level
      d["yc"] = as.factor(d[["y"]] > 0.5)
      return(d)
  }

  task = TaskClassif$new("vtreat_binary", backend = make_data(100), target = "yc")

  train_out1 = op$train(list(task))[[1L]]
  settings = op$state$treatment_plan$settings
  expect_true(op$state$treatment_plan$treatment_type == "BinomialOutcomeTreatment")
  expect_true(all(settings$var_list == task$feature_names))
  expect_true(all(settings$outcome_name == task$target_names))
  expect_true(length(settings$cols_to_copy) == 0L)
  expect_equivalent(settings$params, vtreat::classification_parameters(list(check_for_duplicate_frames = FALSE)))
  predict_out1 = op$predict(list(task))[[1L]]
  expect_false(identical(train_out1$data(), predict_out1$data()))

  op$param_set$values = list(recommended = FALSE,
    cols_to_copy = selector_name(c("xc", "y")),
    ncross = 4,
    imputation_map = list("x" = 0),
    affect_columns = selector_name(c("x", "xc", "y")))
  train_out2 = op$train(list(task))[[1L]]
  expect_true(op$state$treatment_plan$settings$var_list == "x")
  expect_true(all(c("x2", "xc", "y") %in% train_out2$feature_names))
  expect_true("yc" %nin% train_out2$feature_names)
  expect_true(op$state$treatment_plan$settings$params$ncross == 4)
  expect_true(train_out2$missings("x") == 0)
})

test_that("PipeOpVtreat - Multiclass Classification", {
  skip_if_not_installed("vtreat")

  op = PipeOpVtreat$new()

  # clean task simply passes through
  clean_task = TaskClassif$new("vtreat_multiclass_clean", backend = data.frame(y = as.factor(c(-1, 0, 1)), x = c(1, 1, 1)), target = "y")

  expect_equal(op$train(list(clean_task))[[1L]]$data(), op$predict(list(clean_task))[[1L]]$data())
  expect_null(op$state$treatment_plan)

  # toy task as here: https://github.com/WinVector/vtreat/blob/master/Examples/Multinomial/MultinomialExample_FP.md
  set.seed(2020)

  make_data <- function(nrows) {
      d <- data.frame(x = 5 * rnorm(nrows))
      d["y"] = sin(d[["x"]]) + 0.01 * d[["x"]] + 0.1 * rnorm(nrows)
      d[4:10, "x"] = NA  # introduce NAs
      d["xc"] = paste0("level_", 5 * round(d$y / 5, 1))
      d["x2"] = rnorm(nrows)
      d[d["xc"] == "level_-1", "xc"] = NA  # introduce a NA level
      d["yc"] = as.factor(ifelse(d[["y"]] > 0.5, "large", ifelse(d[["y"]] < -0.5, "small", "liminal")))
      return(d)
  }

  task = TaskClassif$new("vtreat_multi", backend = make_data(100), target = "yc")

  train_out1 = op$train(list(task))[[1L]]
  settings = op$state$treatment_plan$settings
  expect_true(op$state$treatment_plan$treatment_type == "MultinomialOutcomeTreatment")
  expect_true(all(settings$var_list == task$feature_names))
  expect_true(all(settings$outcome_name == task$target_names))
  expect_true(length(settings$cols_to_copy) == 0L)
  expect_equivalent(settings$params, vtreat::multinomial_parameters(list(check_for_duplicate_frames = FALSE)))
  predict_out1 = op$predict(list(task))[[1L]]
  expect_false(identical(train_out1$data(), predict_out1$data()))

  op$param_set$values = list(recommended = FALSE,
    cols_to_copy = selector_name("y"),
    scale = TRUE,
    imputation_map = list("x" = 0),
    affect_columns = selector_name(c("x", "xc", "y")))
  train_out2 = op$train(list(task))[[1L]]
  expect_true(all(op$state$treatment_plan$settings$var_list == c("x", "xc")))
  expect_true(all(c("x2", "y") %in% train_out2$feature_names))
  expect_true("yc" %nin% train_out2$feature_names)
  expect_true(op$state$treatment_plan$settings$params$scale == TRUE)
  expect_true(train_out2$missings("x") == 0)
})

test_that("PipeOpVtreat - Edge Cases", {
  skip_if_not_installed("vtreat")

  op = PipeOpVtreat$new()

  set.seed(3)
  dat = data.table(y = rnorm(12), x = as.factor(rep(c("a", "b", "c"), 4L)),
    weights = rep(c(1L, 2L), 6L))

  task = TaskRegr$new("test", backend = dat, target = "y")
  task$col_roles$weight = "weights"
  task$col_roles$feature = "x"

  po = PipeOpVtreat$new()
  train_out1 = po$train(list(task))[[1L]]
  predict_out1 = po$predict(list(task))[[1L]]

  expect_true(colnames(train_out1$data()) == "y")
  expect_equal(train_out1$data(), predict_out1$data())

  task$col_roles$weight = character()
  train_out2 = po$train(list(task))[[1L]]
  predict_out2 = po$predict(list(task))[[1L]]
})

