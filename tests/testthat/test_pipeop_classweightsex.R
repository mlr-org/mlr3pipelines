
test_that("PipeOpClassWeightsEx - basic properties", {
  op = PipeOpClassWeightsEx$new()
  task = mlr_tasks$get("german_credit")
  expect_pipeop(op)
  train_pipeop(op, inputs = list(task))
  predict_pipeop(op, inputs = list(task))

  expect_datapreproc_pipeop_class(PipeOpClassWeightsEx, task = task, predict_like_train = FALSE)
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

test_that("PipeOpClassWeightsEx - methods", {
  task = mlr_tasks$get("penguins")

  # Method inverse_class_frequency
  poicf = po("classweightsex", param_vals = list(weight_method = "inverse_class_frequency"))
  nt = poicf$train(list(task))[[1L]]
  expect_equal(nt$data(), task$data())

  freq = prop.table(table(task$truth()))
  manual_weights = 1 / freq[task$truth()]

  if ("weights_learner" %in% names(nt$col_roles)) {
    computed_weights = nt$weights_learner
  } else {
    computed_weights = nt$weights
  }

  expect_equal(computed_weights[["weight"]], as.numeric(unclass(manual_weights)))

  # Method inverse square root of class frequency
  poisf = po("classweightsex", param_vals = list(weight_method = "inverse_square_root_of_frequency"))
  nt = poisf$train(list(task))[[1L]]
  expect_equal(nt$data(), task$data())

  freq = prop.table(table(task$truth()))
  manual_weights = 1 / sqrt(freq[task$truth()])

  if ("weights_learner" %in% names(nt$col_roles)) {
    computed_weights = nt$weights_learner
  } else {
    computed_weights = nt$weights
  }

  expect_equal(computed_weights[["weight"]], as.numeric(unclass(manual_weights)))

  # Method median frequency balancing
  pomfb = po("classweightsex", param_vals = list(weight_method = "median_frequency_balancing"))
  nt = pomfb$train(list(task))[[1L]]
  expect_equal(nt$data(), task$data())

  freq = prop.table(table(task$truth()))
  manual_weights = median(freq[task$truth()]) / freq[task$truth()]

  if ("weights_learner" %in% names(nt$col_roles)) {
    computed_weights = nt$weights_learner
  } else {
    computed_weights = nt$weights
  }

  expect_equal(computed_weights[["weight"]], as.numeric(unclass(manual_weights)))

  # Method: explicit weighting
  mapping = c(Adelie = 0.4, Gentoo = 0.3, Chinstrap = 0.3)
  pomfb = po("classweightsex", weight_method = "explicit", mapping = mapping)
  nt = pomfb$train(list(task))[[1L]]
  expect_equal(nt$data(), task$data())

  manual_weights = mapping[task$truth()]

  if ("weights_learner" %in% names(nt$col_roles)) {
    computed_weights = nt$weights_learner
  } else {
    computed_weights = nt$weights
  }

  expect_equal(computed_weights[["weight"]], unname(manual_weights))
})

test_that("PipeOpClassWeightsEx - explicit mapping must cover all classes", {
  task = mlr_tasks$get("penguins")
  po_explicit = po("classweightsex", param_vals = list(
    weight_method = "explicit",
    mapping = c("Adelie" = 0.5, "Chinstrap" = 0.3)
  ))
  expect_error(
    po_explicit$train(list(task)),
    "missing.*class",
    fixed = FALSE
  )
})

test_that("PipeOpClassWeightsEx - weight roles assigned", {
  classif_roles = mlr_reflections$task_col_roles$classif
  configs = list(
    list(weights_learner = FALSE, weights_measure = FALSE),
    list(weights_learner = TRUE, weights_measure = FALSE),
    list(weights_learner = FALSE, weights_measure = TRUE),
    list(weights_learner = TRUE, weights_measure = TRUE)
  )

  for (i in seq_along(configs)) {
    task = tsk("penguins")
    po_roles = po("classweightsex", param_vals = list(weight_method = "inverse_class_frequency"))
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
