
test_that("PipeOpClassWeightsEx - basic properties", {
  op = PipeOpClassWeightsEx$new()
  task = mlr_tasks$get("german_credit")
  expect_pipeop(op)
  train_pipeop(op, inputs = list(task))
  predict_pipeop(op, inputs = list(task))

  expect_datapreproc_pipeop_class(PipeOpClassWeights, task = task,
                                  predict_like_train = FALSE)
})

test_that("PipeOpClassWeightsEx - error for Tasks without weights property, #937", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("MASS")

  set.seed(1234)
  task = as_task_classif(data.table(
    y = factor(rep(c("A", "B", "A", "C"), 4)),
    x = runif(16)
  ), target = "y")

  # no error: Learner has weights property
  gr = po("classweightsex", param_vals = list(mapping = c("A" = 0.6, "B" = 0.3, "C" = 0.1))) %>>% lrn("classif.featureless")
  expect_no_error(gr$train(task))

  # error: Learner does not have weights property
  gr = po("classweightsex",  param_vals = list(mapping = c("A" = 0.6, "B" = 0.3, "C" = 0.1))) %>>% lrn("classif.lda")
  expect_error(gr$train(task), ".*Learner does not support weights.*")

  # no error: use_weights is set to "ignore"
  gr = po("classweightsex", param_vals = list(mapping = c("A" = 0.6, "B" = 0.4, "C" = 0.1))) %>>% lrn("classif.lda", use_weights = "ignore")
  expect_no_error(gr$train(task))

})

test_that("PipeOpClassWeightsEx", {

  task = mlr_tasks$get("iris")

  # Method inverse_class_frequency
  poicf = po("classweightsex", param_vals = list(weight_method = "inverse_class_frequency"))
  nt = poicf$train(list(task))[[1L]]
  expect_equal(nt$data(), task$data())


  # manual_weights = as.data.table(1 / table(task$data()$Species))
  # weights = if ("weights_learner" %in% names(nt)) "weights_learner" else "weights"
  # expect_equal(nt[[weights]]$weight, ifelse(nt$truth(nt[[weights]]$row_ids) == "neg", 1, 3))
})
