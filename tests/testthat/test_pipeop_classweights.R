context("PipeOpClassWeights")

test_that("PipeOpClassWeights - basic properties", {
  op = PipeOpClassWeights$new()
  task = mlr_tasks$get("german_credit")
  expect_pipeop(op)
  train_pipeop(op, inputs = list(task))
  predict_pipeop(op, inputs = list(task))

  expect_datapreproc_pipeop_class(PipeOpClassWeights, task = task,
    predict_like_train = FALSE)
})

test_that("PipeOpClassWeights", {
  op = PipeOpClassWeights$new()
  task = mlr_tasks$get("pima")

  op$param_set$values = list(minor_weight =  3)
  nt = op$train(list(task))[[1L]]
  expect_equal(nt$data(), task$data())

  weights = if ("weights_learner" %in% names(nt)) "weights_learner" else "weights"
  expect_equal(nt[[weights]]$weight, ifelse(nt$truth(nt[[weights]]$row_ids) == "neg", 1, 3))

})

test_that("PipeOpClassWeights - error for Tasks without weights property, #937", {
  skip_if_not_installed("mlr3learners")

  set.seed(1234)
  task = as_task_classif(data.table(
    y = factor(rep(c("A", "B", "A"), 4)),
    x = runif(12)
  ), target = "y")

  # no error: Learner has weights property
  gr = po("classweights") %>>% lrn("classif.featureless")
  expect_no_error(gr$train(task))

  # error: Learner does not have weights property
  gr = po("classweights") %>>% lrn("classif.lda")
  expect_error(gr$train(task), ".*Learner does not support weights.*")

  # no error: use_weights is set to "ignore"
  gr = po("classweights") %>>% lrn("classif.lda", use_weights = "ignore")
  expect_no_error(gr$train(task))

})
