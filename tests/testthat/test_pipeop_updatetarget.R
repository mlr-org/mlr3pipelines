 context("PipeOpUpdateTarget")

test_that("update target multi to binary", {
  trafo_fun = function(x) {factor(ifelse(x == "setosa", "setosa", "other"))}
  pom = po("update_target", param_vals = list(trafo = trafo_fun, new_target_name = "setosa"))
  expect_pipeop(pom)
  newtsk = pom$train(list(tsk("iris")))[[1]]
  expect_task(newtsk)
  expect_true("setosa" %in% newtsk$target_names)
  expect_true(all((newtsk$data()$setosa == "setosa") == (tsk("iris")$data()$Species == "setosa")))
  expect_true(pom$is_trained)

  newtsk2 = pom$predict(list(tsk("iris")))[[1]]
  expect_task(newtsk2)
  expect_true("setosa" %in% newtsk2$target_names)
  expect_true(all(is.na(newtsk2$data()$setosa)))
  expect_true(all(levels(newtsk2$data()$setosa) == c("other", "setosa")))
})

test_that("update target regr to classif", {
  trafo_fun = function(x) {factor(ifelse(x < 25, "<25", ">=25"))}
  pom = po("update_target", param_vals = list(trafo = trafo_fun, new_target_name = "threshold_25",
    new_task_type = "classif"))
  expect_pipeop(pom)
  newtsk = pom$train(list(tsk("boston_housing")))[[1]]
  expect_task(newtsk)
  expect_true("threshold_25" %in% newtsk$target_names)
  expect_true(all((newtsk$data()$threshold_25 == "<25") == (tsk("boston_housing")$data()$medv < 25)))
  expect_true(pom$is_trained)

  newtsk2 = pom$predict(list(tsk("boston_housing")))[[1]]
  expect_task(newtsk2)
  expect_true("threshold_25" %in% newtsk2$target_names)
  expect_true(all(is.na(newtsk2$data()$threshold_25)))
  expect_true(all(levels(newtsk2$data()$threshold_25) == c("<25", ">=25")))
})

test_that("update target classif to regr", {
  trafo_fun = function(x) {map_dtc(x, as.numeric)}
  pom = po("update_target", param_vals = list(trafo = trafo_fun, new_target_name = "quality",
    new_task_type = "regr"))
  expect_pipeop(pom)
  newtsk = pom$train(list(tsk("wine")))[[1]]
  expect_task(newtsk)
  expect_true("quality" %in% newtsk$target_names)
  expect_true(all(newtsk$data()$quality == as.numeric(tsk("wine")$data()$type)))
  expect_true(pom$is_trained)

  newtsk2 = pom$predict(list(tsk("wine")))[[1]]
  expect_task(newtsk2)
  expect_true("quality" %in% newtsk2$target_names)
  expect_true(all(is.na(newtsk2$data()$quality)))
})

test_that("update target same target", {
  pom = po("update_target", new_target_name = "type", new_task_type = "classif")
  expect_pipeop(pom)
  newtsk = pom$train(list(tsk("wine")))[[1]]
  expect_task(newtsk)
  expect_true("type" %in% newtsk$target_names)
  expect_equal(newtsk$data(), tsk("wine")$data())
  expect_true(pom$is_trained)

  newtsk2 = pom$predict(list(tsk("wine")))[[1]]
  expect_task(newtsk2)
  expect_true("quality" %in% newtsk2$target_names)
  expect_true(all(is.na(newtsk2$data()$quality)))
})
