context("PipeOpLearnerCV")

test_that("PipeOpLearnerCV - basic properties", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearnerCV$new(lrn)
  expect_pipeop(po$clone(), check_ps_default_values = FALSE)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  task = mlr_tasks$get("iris")
  tsk = train_pipeop(po, list(task = task))[[1]]
  expect_class(tsk, "Task")
  expect_true(tsk$nrow == 150L)
  expect_true(tsk$ncol == 2L)
  expect_equal(task$target_names, tsk$target_names)
  expect_equal(task$class_names, tsk$class_names)
  vals = factor(unique(tsk$data(cols = tsk$feature_names)$response))
  expect_character(setdiff(vals, task$class_names), len = 0)

  tsk = predict_pipeop(po, list(task = task))[[1]]
  expect_class(tsk, "Task")
  expect_true(tsk$nrow == 150L)
  expect_true(tsk$ncol == 2L)
  expect_equal(task$target_names, tsk$target_names)
  expect_equal(task$class_names, tsk$class_names)
  vals = factor(unique(tsk$data(cols = tsk$feature_names)$response))
  expect_character(setdiff(vals, task$class_names), len = 0)

  lrn = mlr_learners$get("classif.featureless")
  iris_with_unambiguous_mode = mlr_tasks$get("iris")$filter(c(1:49, 52:150))  # want featureless learner without randomness
  expect_datapreproc_pipeop_class(PipeOpLearnerCV,
    list(lrn), iris_with_unambiguous_mode, predict_like_train = FALSE, deterministic_train = FALSE, check_ps_default_values = FALSE)
  # 'insample' PipeOpLearnerCV with deterministic Learner is deterministic in every regard!
  expect_datapreproc_pipeop_class(PipeOpLearnerCV,
    list(lrn, param_vals = list(resampling.method = "insample")), iris_with_unambiguous_mode, check_ps_default_values = FALSE)

  expect_error(PipeOpLearnerCV$new())

})

test_that("PipeOpLearnerCV - param values", {
  lrn = mlr_learners$get("classif.rpart")
  polrn = PipeOpLearnerCV$new(lrn)
  expect_subset(c("minsplit", "resampling.method", "resampling.folds"), polrn$param_set$ids())
  expect_equal(polrn$param_set$values, list(resampling.method = "cv", resampling.folds = 3, resampling.keep_response = FALSE, xval = 0))
  polrn$param_set$values$minsplit = 2
  expect_equal(polrn$param_set$values, list(resampling.method = "cv", resampling.folds = 3, resampling.keep_response = FALSE, minsplit = 2, xval = 0))
  polrn$param_set$values$resampling.folds = 4
  expect_equal(polrn$param_set$values, list(resampling.method = "cv", resampling.folds = 4, resampling.keep_response = FALSE, minsplit = 2, xval = 0))
})

test_that("PipeOpLearnerCV - within resampling", {
  lrn = mlr_learners$get("classif.rpart")
  gr = GraphLearner$new(PipeOpLearnerCV$new(lrn) %>>% po(id = "l2", lrn))
  rr = resample(tsk("iris"), gr, rsmp("holdout"))
  expect_class(rr, "ResampleResult")
})

test_that("PipeOpLearnerCV - insample resampling", {
  lrn = mlr_learners$get("classif.featureless")
  iris_with_unambiguous_mode = mlr_tasks$get("iris")$filter(c(1:49, 52:150))  # want featureless learner without randomness

  polrn = PipeOpLearnerCV$new(lrn, param_vals = list(resampling.method = "insample"))
  expect_equal(polrn$train(list(iris_with_unambiguous_mode))[[1]]$data(),
    cbind(iris_with_unambiguous_mode$data(cols = "Species"),
      classif.featureless.response = factor("virginica", levels = levels(iris[[5]]))))

  lrn = mlr_learners$get("classif.rpart")
  polrn = PipeOpLearnerCV$new(lrn, param_vals = list(resampling.method = "insample"))
  expect_equal(polrn$train(list(iris_with_unambiguous_mode))[[1]],
    polrn$predict(list(iris_with_unambiguous_mode))[[1]])
})

test_that("PipeOpLearnerCV - graph but no id", {
  g = PipeOpNOP$new() %>>% PipeOpLearner$new(LearnerClassifRpart$new())
  po = PipeOpLearnerCV$new(g)
  expect_string(po$id)
})

test_that("PipeOpLearnerCV - model active binding to state", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearnerCV$new(lrn)
  task = mlr_tasks$get("iris")

  # before training states are NULL
  expect_null(po$state)
  expect_equal(po$learner$state, po$state)
  expect_equal(po$learner_model$state, po$state)

  # after training learner_model's state and state are equal
  train_out = po$train(list(task))
  train_state = po$state
  expect_null(po$learner$state)
  expect_equal(po$learner_model$state, train_state)

  # after predicting states are unchanged
  predict_out = po$predict(list(task))
  expect_equal(po$state, train_state)
  expect_null(po$learner$state)
  expect_equal(po$learner_model$state, po$state)
})

test_that("predict_type", {
  expect_equal(po("learner_cv", lrn("classif.rpart", predict_type = "response"))$predict_type, "response")
  expect_equal(po("learner_cv", lrn("classif.rpart", predict_type = "prob"))$predict_type, "prob")

  lcv <- po("learner_cv", lrn("classif.rpart", predict_type = "prob"))

  lcv$predict_type = "response"
  expect_equal(lcv$predict_type, "response")
  expect_equal(lcv$learner$predict_type, "response")

  expect_equal(lcv$train(list(tsk("iris")))[[1]]$feature_names, "classif.rpart.response")

  lcv$predict_type = "prob"

  expect_equal(lcv$predict_type, "prob")
  expect_equal(lcv$learner$predict_type, "prob")

  expect_subset(c("classif.rpart.prob.virginica", "classif.rpart.prob.setosa", "classif.rpart.prob.versicolor"),
    lcv$train(list(tsk("iris")))[[1]]$feature_names)

})

test_that("marshal", {
  task = tsk("iris")
  po_lrn = as_pipeop(po("learner_cv", learner = lrn("classif.debug")))
  po_lrn$train(list(task))
  po_state = po_lrn$state
  expect_class(po_state, "pipeop_learner_cv_state")
  po_state_marshaled = marshal_model(po_state, inplace = FALSE)
  expect_class(po_state_marshaled, "pipeop_learner_cv_state_marshaled")
  expect_true(is_marshaled_model(po_state_marshaled))
  expect_equal(po_state, unmarshal_model(po_state_marshaled))
})

test_that("marshal multiplicity", {
  po = po("learner_cv", learner = lrn("classif.debug"))
  po$train(list(Multiplicity(tsk("iris"), tsk("sonar"))))
  s = po$state
  sm = marshal_model(po$state)
  expect_class(po$state, "Multiplicity")
  expect_true(is_marshaled_model(sm$marshaled[[1L]]))
  expect_true(is_marshaled_model(sm$marshaled[[2L]]))

  su = unmarshal_model(sm)
  expect_equal(su, s)

  # recursive
  po = po("learner_cv", learner = lrn("classif.debug"))
  po$train(list(Multiplicity(Multiplicity(tsk("iris")))))
  p1 = po$predict(list(Multiplicity(Multiplicity(tsk("iris")))))

  s = po$state
  sm = marshal_model(po$state)
  expect_class(po$state, "Multiplicity")
  expect_true(is_marshaled_model(sm$marshaled[[1L]][[1L]]))

  su = unmarshal_model(sm)
  expect_equal(su, s)

  po$state = su
  p2 = po$predict(list(Multiplicity(Multiplicity(tsk("iris")))))
  expect_equal(p1, p2)


  task = tsk("iris")
  learner = lrn("classif.debug")

  lrncv_po = po("learner_cv", learner)
  lrncv_po$learner$predict_type = "response"

  nop = mlr_pipeops$get("nop")

  graph = gunion(list(
    lrncv_po,
    nop
  )) %>>% po("featureunion") %>>% lrn("classif.rpart")

  glrn = as_learner(graph)
  expect_learner(glrn, task)

  p1 = glrn$train(task)$predict(task)
  p2 = glrn$marshal()$unmarshal()$predict(task)
  expect_equal(p1, p2)

})

test_that("state class and multiplicity", {
  po = po("learner_cv", learner = lrn("classif.debug"))
  po$train(list(Multiplicity(tsk("iris"))))
  expect_class(po$state, "Multiplicity")
  expect_class(po$state[[1L]], "pipeop_learner_cv_state")

  # recursive
  po1 = po("learner_cv", learner = lrn("classif.debug"))
  po1$train(list(Multiplicity(Multiplicity(tsk("iris")))))
  expect_class(po1$state, "Multiplicity")
  expect_class(po1$state[[1L]], "Multiplicity")
  expect_class(po1$state[[1L]][[1L]], "pipeop_learner_cv_state")
})
