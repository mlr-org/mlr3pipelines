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
  skip_if_not_installed("rpart")
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
  skip_if_not_installed("rpart")
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

test_that("threshold predict type", {
  skip_if_not_installed("rpart")

  t = tsk("iris")
  g =  po("learner", learner = lrn("classif.rpart", predict_type = "prob")) %>>% po("threshold", thresholds = c(.2, .3, .5))
  g$train(t)
  res = g$predict(t)[[1]]

  expect_set_equal(res$predict_types, c("response", "prob"))
  expect_data_table(as.data.table(res))
  expect_names(colnames(as.data.table(res)),
    permutation.of = c("truth", "response", "prob.setosa", "prob.versicolor", "prob.virginica", "row_ids"))
  g$pipeops$threshold$predict_type = "response"
  res = g$predict(t)[[1]]
  expect_prediction(res)
  expect_set_equal(res$predict_types, "response")
  expect_data_table(as.data.table(res))
  expect_names(colnames(as.data.table(res)),
    permutation.of = c("truth", "response", "row_ids"))

  glrn = as_learner(g)
  expect_equal(glrn$predict_type, "response")
  glrn$train(t)

  res = glrn$predict(t)
  expect_prediction(res)
  expect_set_equal(res$predict_types, "response")
  expect_data_table(as.data.table(res))
  expect_names(colnames(as.data.table(res)),
    permutation.of = c("truth", "response", "row_ids"))

  glrn$predict_type = "prob"
  res = glrn$predict(t)
  expect_set_equal(res$predict_types, c("response", "prob"))
  expect_data_table(as.data.table(res))
  expect_names(colnames(as.data.table(res)),
    permutation.of = c("truth", "response", "prob.setosa", "prob.versicolor", "prob.virginica", "row_ids"))

  # the following checks that setting 'response' does not change the predict-type of the classif.rpart
  glrn$predict_type = "response"
  expect_equal(glrn$graph$pipeops$classif.rpart$predict_type, "prob")
  res = glrn$predict(t)
  expect_prediction(res)
  expect_set_equal(res$predict_types, "response")
  expect_data_table(as.data.table(res))
  expect_names(colnames(as.data.table(res)),
    permutation.of = c("truth", "response", "row_ids"))

})
