context("tunethreshold")

test_that("threshold works for multiclass", {
  t = tsk("iris")
  po_cv =  po("learner_cv", learner = lrn("classif.rpart", predict_type = "prob"))
  res = po_cv$train(list(t))
  po_thr = po("tunethreshold")
  expect_pipeop(po_thr)
  po_thr$train(res)
  thr = po_thr$state$threshold
  expect_numeric(thr, len = 3L, lower = 0, upper = 1)
  expect_set_equal(names(thr), t$class_names)
  res2 = po_cv$predict(list(t))
  out = po_thr$predict(res2)[[1]]
  expect_prediction(out)
  expect_true(out$score() < 0.33)
})

test_that("threshold works for binary", {
  t = tsk("pima")
  po_cv =  po("learner_cv", learner = lrn("classif.rpart", predict_type = "prob"))
  res = po_cv$train(list(t))
  po_thr = po("tunethreshold")
  expect_pipeop(po_thr)
  po_thr$train(res)
  thr = po_thr$state$threshold
  expect_numeric(thr, len = 2, lower = 0, upper = 1)
  expect_set_equal(names(thr), t$class_names)
  res2 = po_cv$predict(list(t))
  out = po_thr$predict(res2)[[1]]
  expect_prediction(out)
  expect_true(out$score() < 0.33)
  po_cv =  po("learner_cv", learner = lrn("classif.rpart", predict_type = "response")) %>>%
    po("tunethreshold")
  expect_error(po_cv$train(t), "prob")
})

test_that("tunethreshold graph works", {

  graph = po("learner_cv", lrn("classif.rpart", predict_type = "prob")) %>>% po("tunethreshold")

  out = graph$train(tsk("pima"))

  expect_null(out$tunethreshold.output)

  out = graph$predict(tsk("pima"))

  expect_prediction(out$tunethreshold.output)

  glrn = as_learner(graph)

  glrn$train(tsk("pima"))

  expect_prediction(glrn$predict(tsk("pima")))


})
