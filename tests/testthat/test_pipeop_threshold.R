context("PipeOpThreshold")

test_that("threshold general", {
  po_thr = po("threshold")
  expect_pipeop(po_thr)
  expect_true(po_thr$id == "threshold")
  expect_equal(po_thr$param_set$values$thresholds, 0.5)

  po_thr = po("threshold", param_vals = list(thresholds = c(0.3, 0.5)))
  expect_pipeop(po_thr)
  expect_true(po_thr$id == "threshold")
  expect_true(all(po_thr$param_set$values$thresholds == c(0.3, 0.5)))
})


test_that("thresholding works for binary", {
  po_lrn = po(lrn("classif.rpart", predict_type = "prob"))

  # binary
  t = tsk("german_credit")$filter(rows = 1:100)

  # works with no args
  po_thr = PipeOpThreshold$new()
  expect_pipeop(po_thr)
  gr = po_lrn %>>% po_thr
  gr$train(t)
  prd = gr$predict(t)
  expect_prediction(prd[[1]])
  expect_equal(gr$param_set$values$threshold.thresholds, 0.5)

  # Same as setting threshold at 0.5
  gr = po_lrn %>>% po_thr
  gr$param_set$values$threshold.thresholds = 0.5
  gr$train(t)
  prd2 = gr$predict(t)
  expect_equal(prd, prd2)
  expect_true(gr$param_set$values$threshold.thresholds == 0.5)

  # converges to prop.table for 0,1
  pt = prop.table(table(t$truth()))

  gr = po_lrn %>>% po_thr
  gr$param_set$values$threshold.thresholds = 1
  gr$train(t)
  prd2 = gr$predict(t)
  expect_true(gr$param_set$values$threshold.thresholds == 1)
  expect_true(prd2$threshold.output$score() == pt[1])

  gr = po_lrn %>>% po_thr
  gr$param_set$values$threshold.thresholds = 0
  gr$train(t)
  prd2 = gr$predict(t)
  expect_true(gr$param_set$values$threshold.thresholds == 0)
  expect_true(prd2$threshold.output$score() == pt[2])

  gr = po_lrn %>>% po_thr
  gr$param_set$values$threshold.thresholds = c(1, 0)
  gr$train(t)
  prd2 = gr$predict(t)
  expect_true(all(gr$param_set$values$threshold.thresholds == c(1, 0)))
  expect_true(prd2$threshold.output$score() == pt[1])
})


test_that("thresholding works for multiclass", {
  po_lrn = po(lrn("classif.rpart", predict_type = "prob"))

  # multiclass
  t = tsk("iris")

  # works with no args
  po_thr = PipeOpThreshold$new()
  expect_pipeop(po_thr)
  gr = po_lrn %>>% po_thr
  gr$train(t)
  expect_error(gr$predict(t), "only supported for binary classification")
  gr$param_set$values$threshold.thresholds = c(a = 1, b = 0.4, c = 0.1)
  gr$train(t)
  expect_error(gr$predict(t), "permutation of")
  gr$param_set$values$threshold.thresholds = c(.1, .1, .1)
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
  expect_true(all(gr$param_set$values$threshold.thresholds == c(0.3, 0.4, 0.3)))

  # works with named args
  po_thr = PipeOpThreshold$new(param_vals =
    list(thresholds = c("virginica" = 0.3, "versicolor" = 0.4, "setosa" = 0.3)))
  expect_pipeop(po_thr)
  gr = po_lrn %>>% po_thr
  gr$train(t)
  prd = gr$predict(t)
  expect_prediction(prd[[1]])
  expect_true(all(gr$param_set$values$threshold.thresholds == c(0.3, 0.4, 0.3)))

  # errors with wrong args
  po_thr = PipeOpThreshold$new(param_vals = list(thresholds = c(0.3, 0.4)))
  gr = po_lrn %>>% po_thr
  gr$train(t)
  expect_error(gr$predict(t), "must have length one or length equal to number of outcome levels")

  po_thr = PipeOpThreshold$new(param_vals =
    list(thresholds = c("foo" = 0.3, "versicolor" = 0.4, "setosa" = 0.3)))
  gr = po_lrn %>>% po_thr
  gr$train(t)
  expect_error(gr$predict(t))
})
