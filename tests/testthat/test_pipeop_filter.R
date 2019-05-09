context("PipeOpFilter")

test_that("PipeOpFilter", {
  task = mlr_tasks$get("boston_housing")

  expect_datapreproc_pipeop_class(PipeOpFilter,
    list(filter = mlr3featsel::FilterVariance$new(), param_vals = list(filter.frac = 0.5)), task = task)

  expect_datapreproc_pipeop_class(PipeOpFilter,
    list(filter = mlr3featsel::FilterVariance$new(), param_vals = list(filter.frac = 0.5)), task = mlr_tasks$get("iris"))

  po = PipeOpFilter$new(mlr3featsel::FilterVariance$new())

  expect_equal(po$id, mlr3featsel::FilterVariance$new()$id)

  expect_error(po$train(list(task)), "Exactly one of 'nfeat', 'frac', 'cutoff' must be given.*none")

  po$param_set$values = list(filter.nfeat = 1, filter.frac = 1, FilterVariance.na.rm = TRUE)
  expect_error(po$train(list(task)), "Exactly one of 'nfeat', 'frac', 'cutoff' must be given.*nfeat, frac")

  po$param_set$values = list(filter.nfeat = 1, FilterVariance.na.rm = TRUE)

  tt = po$train(list(task))[[1]]

  expect_set_equal(tt$feature_names, c("chas", "town", "tract"))

  tt2 = po$predict(list(task$clone()$filter(1:10)))[[1]]

  expect_set_equal(tt2$feature_names, c("chas", "town", "tract"))

  # the following only operates on the five columns named below, one of which ('chas') is factorial and not affected
  # by the variance filter. Filtering `frac = 0.5` should remove 'indus' and 'lon'.
  po$affect_columns = function(task) c("chas", "b", "age", "indus", "lon")
  po$param_set$values = list(filter.frac = 0.5, FilterVariance.na.rm = TRUE)

  tt = po$train(list(task))[[1]]

  expect_set_equal(tt$feature_names, c(setdiff(task$feature_names, po$affect_columns(task)), "chas", "b", "age"))
})
