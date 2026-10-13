test_that("GraphLearner deep clones R6 values in saved training parameters", {
  learner = as_learner(
    po("proxy", content = po("pca", center = TRUE)) %>>% lrn("classif.featureless")
  )
  learner$train(tsk("iris"))

  cloned = learner$clone(deep = TRUE)
  original_content = learner$state$param_vals$proxy.content
  cloned_content = cloned$state$param_vals$proxy.content

  expect_false(identical(original_content, cloned_content))
  expect_equal(cloned_content$param_set$values, original_content$param_set$values)

  # Saved training parameters must be isolated as well as the graph's ParamSet.
  cloned_content$param_set$values$center = FALSE
  expect_true(original_content$param_set$values$center)
  expect_false(cloned_content$param_set$values$center)
})
