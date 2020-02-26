context("PipeOpThreshold")

# we check that all pipeops that are exported are also in the dictionary, and can be constructed from there.
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

  # Same as setting threshold at 0.5
  gr$param_set$values$threshold.thresholds = 0.5
  gr = po_lrn %>>% po_thr
  gr$train(t)
  prd2 = gr$predict(t)
  expect_equal(prd, prd2)

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
})
