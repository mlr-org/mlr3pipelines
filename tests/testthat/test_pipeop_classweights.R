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
