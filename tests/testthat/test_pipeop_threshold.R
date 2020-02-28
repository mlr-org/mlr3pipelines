context("PipeOpThreshold")

test_that("threshold", {
  po_thr = po("threshold")
  expect_pipeop(po_thr)
  expect_true(po_thr$id == "threshold")
  expect_true(gr$param_set$values$threshold.thresholds == numeric(0))

  po_thr = po("threshold", param_vals = list(thresholds = c(0.3, 0.5)))
  expect_pipeop(po_thr)
  expect_true(po_thr$id == "threshold")
  expect_true(all(po_thr$param_set$values$thresholds == c(0.3, 0.5)))
})


test_that("threshold", {
  po_lrn = po(lrn("classif.rpart", predict_type = "prob"))

  # binary
  t = tsk("german_credit")

  # works with no args
  po_thr = PipeOpThreshold$new()
  expect_pipeop(po_thr)
  gr = po_lrn %>>% po_thr
  gr$train(t)
  prd = gr$predict(t)
  expect_prediction(prd[[1]])
  expect_true(gr$param_set$values$threshold.thresholds == numeric(0))

  # Same as setting threshold at 0.5
  gr = po_lrn %>>% po_thr
  gr$param_set$values$threshold.thresholds = 0.5
  gr$train(t)
  prd2 = gr$predict(t)
  expect_equal(prd, prd2)
  expect_true(gr$param_set$values$threshold.thresholds == 0.5)

  # multiclass
  t = tsk("iris")

  # works with no args
  po_thr = PipeOpThreshold$new()
  expect_pipeop(po_thr)
  gr = po_lrn %>>% po_thr
  gr$train(t)
  prd = gr$predict(t)
  expect_prediction(prd[[1]])

  # works with args
  po_thr = PipeOpThreshold$new(param_vals = list(thresholds = c(0.3, 0.4, 0.3)))
  expect_pipeop(po_thr)
  gr = po_lrn %>>% po_thr
  gr$train(t)
  prd = gr$predict(t)
  expect_prediction(prd[[1]])
  expect_true(all(gr$param_set$values$threshold.thresholds == c(0.3, 0.4, 0.4))
})
