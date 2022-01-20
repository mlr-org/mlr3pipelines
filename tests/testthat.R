if (requireNamespace("testthat", quietly = TRUE)) {
  library("checkmate")
  library("testthat")
  library("mlr3")
  library("paradox")
  library("mlr3pipelines")
  test_check("mlr3pipelines")
}
