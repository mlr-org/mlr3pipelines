context("FilterEnsemble")

test_that("FilterEnsemble combines wrapped filter scores", {
  skip_if_not_installed("mlr3filters")

  task = mlr_tasks$get("sonar")
  filters = list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  )
  flt_ensemble = FilterEnsemble$new(lapply(filters, function(flt) flt$clone(deep = TRUE)))

  weights = c(0.3, 0.7)
  names(weights) = vapply(filters, function(flt) flt$id, character(1))
  flt_ensemble$param_set$values$weights = weights

  flt_ensemble$calculate(task)
  combined_scores = flt_ensemble$scores

  individual_scores = lapply(filters, function(flt) {
    flt$calculate(task)
    flt$scores[task$feature_names]
  })

  expected = Reduce(`+`, Map(function(scores, w) scores * w, individual_scores, weights))
  expect_equal(combined_scores[task$feature_names], expected)

  flt_ensemble$param_set$values$rank_transform = TRUE
  flt_ensemble$calculate(task)
  combined_rank_scores = flt_ensemble$scores

  expected_rank = Reduce(`+`, Map(function(scores, w) rank(scores) * w, individual_scores, weights))
  expect_equal(combined_rank_scores[task$feature_names], expected_rank)
})

test_that("FilterEnsemble works inside PipeOpFilter", {
  skip_if_not_installed("mlr3filters")

  task = mlr_tasks$get("sonar")
  filters = list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  )
  flt_ensemble = FilterEnsemble$new(lapply(filters, function(flt) flt$clone(deep = TRUE)))

  weights = c(variance = 0.5, auc = 0.5)
  flt_ensemble$param_set$values$weights = weights
  flt_ensemble$calculate(task)

  ordered_features = names(sort(flt_ensemble$scores, decreasing = TRUE))

  po = PipeOpFilter$new(flt_ensemble$clone(deep = TRUE),
    param_vals = list(filter.nfeat = 5, weights = weights)
  )

  filtered_task = po$train(list(task))[[1]]
  expect_setequal(filtered_task$feature_names, ordered_features[seq_len(5)])
})

test_that("FilterEnsemble cloning keeps param sets independent", {
  skip_if_not_installed("mlr3filters")

  filters = list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  )

  original = FilterEnsemble$new(filters)
  original$param_set$values$weights = stats::setNames(c(0.4, 0.6), vapply(filters, function(flt) flt$id, character(1)))

  clone = original$clone(deep = TRUE)

  clone$param_set$values[[paste0(filters[[1]]$id, ".na.rm")]] = FALSE

  expect_false(clone$wrapped[[filters[[1]]$id]]$param_set$get_values()[["na.rm"]])
  expect_true(original$wrapped[[filters[[1]]$id]]$param_set$get_values()[["na.rm"]])
})

test_that("FilterEnsemble ignores NA scores from wrapped filters", {
  skip_if_not_installed("mlr3filters")

  task = mlr_tasks$get("iris")
  variance_filter = mlr3filters::FilterVariance$new()
  permutation_filter = mlr3filters::FilterPermutation$new()

  permutation_filter$resampling$instantiate(task)
  permutation_filter$param_set$values$standardize = TRUE
  permutation_filter$param_set$values$nmc = 1L

  variance_filter$calculate(task)
  permutation_filter$calculate(task)

  variance_scores = variance_filter$scores[task$feature_names]
  expect_true(all(is.nan(permutation_filter$scores[task$feature_names])))

  filters = list(
    variance_filter$clone(deep = TRUE),
    permutation_filter$clone(deep = TRUE)
  )
  weights = c(variance = 0.5, permutation = 0.5)

  flt_ensemble = FilterEnsemble$new(filters)
  flt_ensemble$param_set$values$weights = weights
  flt_ensemble$calculate(task)

  combined_scores = flt_ensemble$scores[task$feature_names]
  expect_equal(combined_scores, variance_scores * weights[["variance"]])
})
