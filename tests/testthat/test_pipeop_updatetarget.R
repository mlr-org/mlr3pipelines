context("PipeOpUpdateTarget")

test_that("update target multi to binary", {
  trafo_fun = function(x) {factor(ifelse(x == "setosa", "setosa", "other"))}
  pom = PipeOpUpdateTarget$new(param_vals = list(trafo = trafo_fun, new_target_name = "setosa"))
  expect_pipeop(pom)
  newtsk = pom$train(list(tsk("iris")))[[1]]
  expect_task(newtsk)
  expect_true("setosa" %in% newtsk$target_names)
  expect_true(all((newtsk$data()$setosa == "setosa") == (tsk("iris")$data()$Species == "setosa")))
  expect_true(pom$is_trained)

  newtsk2 = pom$predict(list(tsk("iris")))[[1]]
  expect_task(newtsk2)
  expect_true("setosa" %in% newtsk2$target_names)
  expect_true(all(levels(newtsk2$data()$setosa) == c("other", "setosa")))
})

test_that("update target regr to classif", {
  trafo_fun = function(x) {factor(ifelse(x < 25, "<25", ">=25"))}
  pom = PipeOpUpdateTarget$new(param_vals = list(trafo = trafo_fun, new_target_name = "threshold_25", new_task_type = "classif"))
  expect_pipeop(pom)
  newtsk = pom$train(list(tsk("boston_housing")))[[1]]
  expect_task(newtsk)
  expect_true("threshold_25" %in% newtsk$target_names)
  expect_true(all((newtsk$data()$threshold_25 == "<25") == (tsk("boston_housing")$data()$medv < 25)))
  expect_true(pom$is_trained)

  newtsk2 = pom$predict(list(tsk("boston_housing")))[[1]]
  expect_task(newtsk2)
  expect_true("threshold_25" %in% newtsk2$target_names)
  expect_true(all(levels(newtsk2$data()$threshold_25) == c("<25", ">=25")))
})

test_that("update target classif to regr", {
  # this is e.g. used in mlr3ordinal for casting
  # orginal to regr
  trafo_fun = function(x) {map_dtc(x, as.numeric)}
  pom = PipeOpUpdateTarget$new(param_vals = list(trafo = trafo_fun, new_target_name = "quality", new_task_type = "regr"))
  expect_pipeop(pom)
  newtsk = pom$train(list(tsk("wine")))[[1]]
  expect_true(inherits(newtsk, "TaskRegr"))
  expect_task(newtsk)
  expect_true("quality" %in% newtsk$target_names)
  expect_true(all(newtsk$data()$quality == as.numeric(tsk("wine")$data()$type)))
  expect_true(pom$is_trained)

  newtsk2 = pom$predict(list(tsk("wine")))[[1]]
  expect_task(newtsk2)
  expect_true(inherits(newtsk, "TaskRegr"))
  expect_true("quality" %in% newtsk2$target_names)
})

test_that("update target same target", {
  # this is e.g. used in mlr3ordinal for casting
  # orginal to classif
  pom = PipeOpUpdateTarget$new(param_vals = list(new_target_name = "type", new_task_type = "classif"))
  expect_pipeop(pom)
  newtsk = pom$train(list(tsk("wine")))[[1]]
  expect_task(newtsk)
  expect_true("type" %in% newtsk$target_names)
  expect_equal(newtsk$data(), tsk("wine")$data())
  expect_true(pom$is_trained)

  newtsk2 = pom$predict(list(tsk("wine")))[[1]]
  expect_task(newtsk2)
  expect_true("type" %in% newtsk2$target_names)
  expect_equal(newtsk2$data(), tsk("wine")$data())
})

test_that("rename target", {
  pom = PipeOpUpdateTarget$new(param_vals = list(new_target_name = "new_type"))
  expect_pipeop(pom)
  newtsk = pom$train(list(tsk("wine")))[[1]]
  expect_task(newtsk)
  expect_true("new_type" %in% newtsk$target_names)
  expect_true("type" %nin% c(newtsk$target_names, newtsk$feature_names))
  expect_equal(newtsk$data()$new_type, tsk("wine")$data()$type)

  newtsk2 = pom$predict(list(tsk("wine")))[[1]]
  expect_task(newtsk2)
  expect_true("new_type" %in% newtsk2$target_names)
  expect_true("type" %nin% c(newtsk2$target_names, newtsk2$feature_names))
  expect_equal(levels(newtsk2$data()$new_type), levels(tsk("wine")$data()$type))
})

test_that("update resample and predict_newdata", {
  skip_on_cran()
  t = tsk("wine")
  pom = PipeOpUpdateTarget$new(param_vals = list(new_target_name = "type", new_task_type = "classif"))
  g = GraphLearner$new(pom %>>% lrn("classif.rpart"))
  r = resample(t, g, rsmp("holdout"))
  expect_numeric(r$score()$classif.ce)
  g$train(t)
  g$predict_newdata(t$data(cols = t$feature_names))
})

test_that("make an existing feature a target", {
  pom = PipeOpUpdateTarget$new(param_vals = list(new_target_name = "ash", new_task_type = "regr", drop_original_target = FALSE))
  expect_pipeop(pom)
  newtsk = pom$train(list(tsk("wine")))[[1]]
  expect_task(newtsk)
  expect_true("ash" %in% newtsk$target_names)
  expect_true("type" %nin% newtsk$target_names)
  expect_true("type" %in% newtsk$feature_names)
  expect_equal(newtsk$data()$ash, tsk("wine")$data()$ash)

  newtsk2 = pom$predict(list(tsk("wine")))[[1]]
  expect_equivalent(newtsk, newtsk2)
})
