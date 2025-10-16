context("PipeOpSplines")

test_that("PipeOpSplines - basic properties", {
  skip_if_not_installed("splines")
  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpSplines, task = task, predict_like_train = FALSE)
  # when we train we get the Boundary.knots, when we now predict on the same data the results will change
})

test_that("Error when trying to pass degree argument while factor = natural", {
  skip_if_not_installed("splines")
  expect_error(po("basissplines", factor = "natural", degree = 3))
})

test_that("results are identical as when calculating by hand", {
  skip_if_not_installed("splines")
  type = list("natural", "polynomial")
  intercept = list(TRUE, FALSE)
  degree = list(1, 2, 3, 4, 5)
  df = list(1, 2, 3, 4, 5)
  task = tsk("iris")
  for (j in degree) {
    for (k in intercept) {
    po = po("splines", type = "polynomial", df = j + 1, degree = j, intercept = k)
    result = po$train(list(task))[[1]]$data()[, -1]
    result_calc = as.data.table(
      cbind(
      splines::bs(iris$Petal.Length, df = j + 1, degree = j, intercept = k),
      splines::bs(iris$Petal.Width, df = j + 1, degree = j, intercept = k),
      splines::bs(iris$Sepal.Length, df = j + 1, degree = j, intercept = k),
      splines::bs(iris$Sepal.Width, df = j + 1, degree = j, intercept = k)))
    expect_equal(unname(as.matrix(result)), unname(as.matrix(result_calc)))
    }
  }
  for (j in degree) {
    for (k in intercept) {
      po = po("splines", type = "natural", df = j + 1, intercept = k)
      result = po$train(list(task))[[1]]$data()[, -1]
      result_calc = as.data.table(
        cbind(
        splines::ns(iris$Petal.Length, df = j + 1, intercept = k),
        splines::ns(iris$Petal.Width, df = j + 1, intercept = k),
        splines::ns(iris$Sepal.Length, df = j + 1, intercept = k),
        splines::ns(iris$Sepal.Width, df = j + 1, intercept = k)))
      expect_equal(unname(as.matrix(result)), unname(as.matrix(result_calc)))
    }
  }
})

test_that("Selector", {
  skip_if_not_installed("splines")
  type = list("natural", "polynomial")
  intercept = list(TRUE, FALSE)
  selector = list("Petal.Length", "Sepal.Length", "Petal.Width", "Sepal.Width")
  degree = list(1, 2, 3, 4, 5)
  df = list(1, 2, 3, 4, 5)
  task = tsk("iris")
  for (i in selector) {
    for (j in degree) {
      for (k in intercept) {
        po = po("splines", type = "polynomial", df = j + 1, degree = j, intercept = k, affect_columns = selector_grep(i))
        result = po$train(list(task))[[1]]$data()[, -1]
        result_calc = as.data.table(
          cbind(
            splines::bs(iris[[i]], df = j + 1, degree = j, intercept = k)))
        expect_subset(unname(as.matrix(result_calc)), unname(as.matrix(result)))
      }
    }
  }
  for (i in selector) {
    for (j in degree) {
      for (k in intercept) {
        po = po("splines", type = "natural", df = j + 1, intercept = k, affect_columns = selector_grep(i))
        result = po$train(list(task))[[1]]$data()[, -1]
        result_calc = as.data.table(
          cbind(
            splines::ns(iris[[i]], df = j + 1, intercept = k)))
        expect_subset(unname(as.matrix(result_calc)), unname(as.matrix(result)))
      }
    }
  }
})

