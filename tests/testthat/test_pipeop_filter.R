context("PipeOpFilter")

test_that("PipeOpFilter", {
  task = mlr_tasks$get("boston_housing")

  expect_datapreproc_pipeop_class(PipeOpFilter,
    list(filter = mlr3filters::FilterVariance$new(), param_vals = list(filter.frac = 0.5)), task = task,
    check_ps_default_values = FALSE)

  expect_datapreproc_pipeop_class(PipeOpFilter,
    list(filter = mlr3filters::FilterVariance$new(), param_vals = list(filter.frac = 0.5)), task = mlr_tasks$get("iris"),
    check_ps_default_values = FALSE)

  po = PipeOpFilter$new(mlr3filters::FilterVariance$new())

  expect_equal(po$id, mlr3filters::FilterVariance$new()$id)

  expect_error(po$train(list(task)), "Exactly one of 'nfeat', 'frac', 'cutoff', or 'permuted' must be given.*none")

  po$param_set$values = list(filter.nfeat = 1, filter.frac = 1, na.rm = TRUE)
  expect_error(po$train(list(task)), "Exactly one of 'nfeat', 'frac', 'cutoff', or 'permuted' must be given.*nfeat, frac")

  po$param_set$values = list(filter.nfeat = 1, na.rm = TRUE)

  orig_filter = po$filter$clone(deep = TRUE)

  tt = po$train(list(task))[[1]]

  expect_deep_clone(po$filter, orig_filter) # po$filter not changed by train

  expect_set_equal(tt$feature_names, c("chas", "town", "tract"))

  tt2 = po$predict(list(task$clone()$filter(1:10)))[[1]]

  expect_set_equal(tt2$feature_names, c("chas", "town", "tract"))

  # the following only operates on the five columns named below, one of which ('chas') is factorial and not affected
  # by the variance filter. Filtering `frac = 0.5` should remove 'indus' and 'lon'.
  po$param_set$values = list(filter.frac = 0.5, na.rm = TRUE)
  po$param_set$values$affect_columns = function(task) c("chas", "b", "age", "indus", "lon")

  tt = po$train(list(task))[[1]]

  expect_set_equal(tt$feature_names, c(setdiff(task$feature_names, po$param_set$values$affect_columns(task)), "chas", "b", "age"))
})


test_that("PipeOpFilter parameters", {

  po = PipeOpFilter$new(mlr3filters::FilterVariance$new())

  expect_set_equal(c("filter.nfeat", "filter.frac", "filter.cutoff", "filter.permuted"),
    grep("^filter\\.", names(po$param_set$params), value = TRUE))

  po = po$clone(deep = TRUE) # cloning often breaks param connection

  po$param_set$values$na.rm = FALSE
  expect_equal(po$filter$param_set$values$na.rm, FALSE)

  po$param_set$values$na.rm = TRUE
  expect_equal(po$filter$param_set$values$na.rm, TRUE)

})


test_that("PipeFilter permuted", {
  set.seed(1)
  N = 50
  task = tgen("2dnormals")$generate(N)

  po = po("filter", filter = mlr3filters::FilterAUC$new(), filter.permuted = 1)
  out = po$train(list(task))[[1]]
  expect_equal(out$feature_names, c("x1", "x2"))

  task$cbind(data.frame(foo = runif(N), bar = runif(N)))
  out = po$train(list(task))[[1]]
  expect_equal(out$feature_names, c("x1", "x2"))

  po = po("filter", filter = mlr3filters::FilterAUC$new(), filter.permuted = 2)
  out = po$train(list(task))[[1]]
  expect_equal(out$feature_names, c("x1", "x2"))

  po = po("filter", filter = mlr3filters::FilterAUC$new(), filter.permuted = 100)
  out = po$train(list(task))[[1]]
  expect_equal(out$feature_names, task$feature_names)

  task$select(setdiff(task$feature_names, c("x1", "x2")))
  po = po("filter", filter = mlr3filters::FilterAUC$new(), filter.permuted = 1)
  out = po$train(list(task))[[1]]
  expect_equal(out$feature_names, character())
})
