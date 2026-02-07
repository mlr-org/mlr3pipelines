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

  op = po("classweights")
  task = mlr_tasks$get("pima")

  op$param_set$values$minor_weight = 3
  nt = op$train(list(task))[[1L]]
  expect_equal(nt$data(), task$data())
  weights = if ("weights_learner" %in% names(nt)) "weights_learner" else "weights"
  expect_equal(nt[[weights]]$weight, ifelse(nt$truth(nt[[weights]]$row_ids) == "neg", 1, 3))
})

test_that("PipeOpClassWeights - weight roles assigned", {
  classif_roles = mlr_reflections$task_col_roles$classif
  configs = list(
    list(weights_learner = FALSE, weights_measure = FALSE),
    list(weights_learner = TRUE, weights_measure = FALSE),
    list(weights_learner = FALSE, weights_measure = TRUE),
    list(weights_learner = TRUE, weights_measure = TRUE)
  )

  for (i in seq_along(configs)) {
    task = tsk("pima")
    po_roles = po("classweights", param_vals = list(minor_weight = 2))
    po_roles$param_set$set_values(.values = configs[[i]])
    nt = po_roles$train(list(task))[[1L]]

    weightcolname = ".WEIGHTS"
    if (!length(configs[[i]])) {
      expect_false(weightcolname %in% unlist(nt$col_roles))
      next
    }
    expect_false(weightcolname %in% nt$col_roles$feature)

    types = names(configs[[i]])[unlist(configs[[i]])]
    for (type in types) {
      final_role = if (type %in% classif_roles) type else "weight"
      expect_true(weightcolname %in% nt$col_roles[[final_role]])
    }
  }
})

test_that("PipeOpClassWeights - error for Tasks without weights property, #937", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("MASS")

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
