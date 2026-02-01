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
  task = mlr_tasks$get("pima")
  classif_roles = mlr_reflections$task_col_roles$classif
  configs = list(
    weight_type = "learner",
    weight_type = "measure",
    weight_type = c("learner", "measure"),
    weight_type = character()
  )

  for (cfg in configs) {
    task_clone = task$clone(deep = TRUE)
    po_roles = po("classweights", param_vals = list(
      minor_weight = 2,
      weight_type = cfg))
    nt = po_roles$train(list(task_clone))[[1L]]
    weightcolname = ".WEIGHTS"

    if (length(cfg) == 0) {
      expect_false(weightcolname %in% unlist(nt$col_roles))
      next
    }

    expect_false(weightcolname %in% nt$col_roles$feature)
    for (type in cfg) {
      preferred_role = paste0("weights_", type)
      final_role = if (preferred_role %in% classif_roles) preferred_role else "weight"
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
