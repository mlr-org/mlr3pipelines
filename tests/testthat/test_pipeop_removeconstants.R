context("PipeOpRemoveConstants")

test_that("PipeOpRemoveConstants - basic properties", {
  task = mlr_tasks$get("boston_housing")
  task$cbind(data.table(xx = rep(1, 506), yy = rep("a", 506)))

  op = PipeOpRemoveConstants$new()
  expect_pipeop(op)

  expect_datapreproc_pipeop_class(PipeOpRemoveConstants, task = task)

})

test_that("PipeOpRemoveConstants removes expected cols", {

  task = mlr_tasks$get("boston_housing")
  task$cbind(data.table(xx = rep(1, 506), yy = rep("a", 506),
    xx1 = c(2, rep(1, 505)), yy1 = c("b", rep("a", 505))))

  po = PipeOpRemoveConstants$new()

  cn = po$train(list(task))[[1]]$feature_names

  expect_set_equal(cn, po$state$features)

  expect_set_equal(cn, setdiff(task$feature_names, c("xx", "yy")))

  po$param_set$values$ratio = 1/505.9999

  cn = po$train(list(task))[[1]]$feature_names
  expect_set_equal(cn, setdiff(task$feature_names, c("xx", "yy", "xx1", "yy1")))

  po$param_set$values$ratio = 1/506.0001

  cn = po$train(list(task))[[1]]$feature_names
  expect_set_equal(cn, setdiff(task$feature_names, c("xx", "yy")))

  po$param_set$values$ratio = 0
  po$param_set$values$abs_tol = 1

  cn = po$train(list(task))[[1]]$feature_names
  expect_set_equal(cn, setdiff(task$feature_names, c("xx", "yy", "xx1", "lat", "lon", "nox")))

  test_dropping = function(data, expected_data, params) {
    intask = TaskClassif$new("iris", cbind(data, target = c("x", rep("y", nrow(data) - 1))), "target")
    resulttask = PipeOpRemoveConstants$new(param_vals = params)$train(list(intask))[[1]]
    expect_equal(resulttask$data(cols = resulttask$feature_names), as.data.table(expected_data),
      ignore.col.order = TRUE)
  }

  test_dropping(iris, iris, list())
  test_dropping(iris, iris[c(1, 3, 5)], list(abs_tol = 3))
  test_dropping(iris, iris[3:5], list(rel_tol = 1))
  test_dropping(iris[1:10, ], iris[1:10, 1:4], list())

  iris.na = iris
  iris.na[[1]] = 1
  iris.na[1, 1] = NA
  iris.na[[2]] = NA_integer_

  test_dropping(iris.na , iris.na[c(1, 3:5)], list(na_ignore = FALSE))
  test_dropping(iris.na , iris.na[3:5], list(na_ignore = FALSE, ratio = 0.01))
  test_dropping(iris.na , iris.na[3:5], list(na_ignore = TRUE))
  test_dropping(iris.na , iris.na[3:5], list(na_ignore = TRUE, abs_tol = 0, rel_tol = 0))

  iris.na[[2]][1:3] = 1:3
  test_dropping(iris.na , iris.na[c(1, 2:5)], list(na_ignore = FALSE))
  test_dropping(iris.na , iris.na[2:5], list(na_ignore = FALSE, ratio = 0.01))
  test_dropping(iris.na , iris.na[3:5], list(na_ignore = FALSE, ratio = 0.03))
  test_dropping(iris.na , iris.na[2:5], list(na_ignore = TRUE))
  test_dropping(iris.na , iris.na[2:5], list(na_ignore = TRUE, ratio = 0.03))

  minus.iris = iris
  for (i in 1:4) minus.iris[i] = minus.iris[i] * -1
  test_dropping(minus.iris , minus.iris, list())

})
