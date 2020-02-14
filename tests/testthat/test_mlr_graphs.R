context("mlr_graphs")

test_that("Robustify Pipeline", {
  lrn = lrn("classif.rpart")

  # complete data, numeric
  tsk = tsk("iris")
  p = pipe("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true("scale" %in% names(p$pipeops))
  expect_true(length(p$pipeops) == 2)

  # complete data no scaling
  p = pipe("robustify", task = tsk, learner = lrn, scaling = FALSE) %>>% po(lrn)
  expect_graph(p)
  expect_true("nop" %in% names(p$pipeops))
  expect_true(length(p$pipeops) == 2)

  tsk = tsk("pima")
  # missings with scaling (rpart can do scaling)
  p = pipe("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true("scale" %in% names(p$pipeops))

  # with fct, assuming rpart can not do fct
  dt = data.table("fct" = factor(rep_len(letters[1:3], tsk$nrow)))
  tsk$cbind(dt)
  lrn$feature_types = c("integer", "numeric")
  p = pipe("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("scale", "encode") %in% names(p$pipeops)))

  # missing fcts, assuming rpart can not do missings
  lrn$properties = c("multiclass", "twoclass")
  dt =  data.table("fct2" = factor(rep_len(c(letters[1:3], NA), tsk$nrow)))
  tsk$cbind(dt)
  p = pipe("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("imputehist", "missind", "scale", "encode", "imputenewlvl") %in% names(p$pipeops)))

  # no scaling
  p = pipe("robustify", task = tsk, learner = lrn, scaling = FALSE) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("imputehist", "missind") %in% names(p$pipeops)))
  expect_true(!("scale" %in% names(p$pipeops)))

  # test on mixed, no missings
  tsk = tsk("boston_housing")
  lrn = lrn("regr.rpart")
  p = pipe("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("encode", "scale") %in% names(p$pipeops)))
  expect_true(!("missind" %in% names(p$pipeops)))
  expect_true(!("imputenewlvl" %in% names(p$pipeops)))

  # logical impute_missings
  p = pipe("robustify", task = tsk, learner = lrn, impute_missings = TRUE) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("imputehist", "missind", "imputenewlvl") %in% names(p$pipeops)))

  # no task
  p = pipe("robustify") %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("fixfactors", "imputehist", "missind", "imputenewlvl",
    "collapsefactors", "scale", "encode") %in% names(p$pipeops)))

  p = pipe("robustify", impute_missings = FALSE) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("fixfactors", "collapsefactors", "scale", "encode") %in% names(p$pipeops)))
  expect_true(!all(c("imputehist", "missind", "imputenewlvl") %in% names(p$pipeops)))

  # missings during predict
  dt = tsk$data()
  dt[2, 3] = NA
  tsk2 = TaskRegr$new(id = "bh", dt, target = "medv")
  lrn$properties = c("multiclass", "twoclass")
  p = pipe("robustify", impute_missings = TRUE) %>>% po(lrn)
  g = GraphLearner$new(p)
  g$train(tsk)
  prd = g$predict(tsk2)
  expect_prediction(prd)

})


test_that("Bagging Pipeline", {
  # classif
  tsk = tsk("iris")
  lrn = lrn("classif.rpart")
  p = pipe("bagging", graph = po(lrn), averager = po("classifavg"))
  expect_graph(p)
  expect_true(length(p$pipeops) == 10 + 10 + 1)

  # regr
  tsk = tsk("boston_housing")
  lrn = lrn("regr.rpart")
  p = pipe("bagging", graph = po(lrn), iterations = 5L, averager = po("regravg"))
  expect_graph(p)
  expect_true(length(p$pipeops) == 5 + 5 + 1)

  # graph instead of po(lrn)
  gr = po("pca") %>>% po(lrn)
  p = pipe("bagging", graph = gr, iterations = 5L, averager = po("regravg"))
  expect_graph(p)
  expect_true(length(p$pipeops) == 5 + (2*5) + 1)
  resample(tsk, GraphLearner$new(p), rsmp("holdout"))
})
