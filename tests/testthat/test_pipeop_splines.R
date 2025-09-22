context("PipeOpSplines")

test_that("PipeOpSplines - basic properties", {
  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpSplines, task = task, deterministic_predict = FALSE)
})

test_that("Error when trying to pass degree argument while factor = cubic", {
  expect_error(po("basissplines", factor = "cubic", degree = 3))
})

test_that("results are identical as when calculating by hand", {
  degree = list(1, 2, 3, 4, 5)
  df = list(1, 2, 3, 4, 5)
  task = tsk("iris")
  for (j in degree) {
    po = po("splines", type = "polynomial", df = j, degree = j)
    result = po$train(list(task))[[1]]$data()[, -1]
    result_calc = as.data.table(stats::model.matrix(
      Species ~ splines::bs(Petal.Length, df = j, degree = j) +
        splines::bs(Petal.Width, df = j, degree = j) +
        splines::bs(Sepal.Length, df = j, degree = j) +
        splines::bs(Sepal.Width, df = j, degree = j),
      data = iris
    ))
    data.table::setnames(result, rep("", ncol(result)))
    data.table::setnames(result_calc, rep("", ncol(result_calc)))
    expect_equal(result, result_calc)
  }
  for (j in df) {
    po = po("splines", df = j)
    result = po$train(list(task))[[1]]$data()[, -1]
    result_calc = as.data.table(stats::model.matrix(
      Species ~ splines::ns(Petal.Length, df = j) +
        splines::ns(Petal.Width, df = j) +
        splines::ns(Sepal.Length, df = j) +
        splines::ns(Sepal.Width, df = j),
      data = iris
    ))
    data.table::setnames(result, rep("", ncol(result)))
    data.table::setnames(result_calc, rep("", ncol(result_calc)))
    expect_equal(result, result_calc)
  }
})


test_that("Selector", {
  selector = list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  task = tsk("iris")
  factor = list("polynomial", "natural")
  for (i in seq_along(factor)) {
    for (j in seq_along(selector)) {
      po = po("splines", type = factor[[i]], affect_columns = selector_grep(selector[[j]]))
      result = po$train(list(task))[[1]]$data()[, -1]
      result_calc = as.data.table(stats::model.matrix(
        as.formula(paste0(if (factor[[i]] == "polynomial") {"Species ~ splines::bs("} else {"Species ~ splines::ns("}, selector[[j]], ")")),
        data = iris
      ))
      relevant_result = as.data.table(intersect(result_calc, result))
      data.table::setnames(result_calc, rep("", ncol(result_calc)))
      data.table::setnames(relevant_result, rep("", ncol(relevant_result)))
      expect_equal(result_calc, relevant_result)
    }
  }
})
