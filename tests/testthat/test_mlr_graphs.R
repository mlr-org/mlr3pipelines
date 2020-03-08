context("mlr_graphs")

test_that("Robustify Pipeline", {
  lrn = lrn("classif.rpart")

  # complete data, numeric
  tsk = tsk("iris")
  p = robustify_pipeline(task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true("removeconstants" %in% names(p$pipeops))
  expect_true("fixfactors" %nin% names(p$pipeops))
  expect_true(length(p$pipeops) == 2)

  tsk = tsk("pima")
  # missings with scaling (rpart can do missings)
  p = ppl("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("removeconstants") %in% names(p$pipeops)))

  # with fct, assuming rpart can not do fct
  dt = data.table("fct" = factor(rep_len(letters[1:3], tsk$nrow)))
  tsk$cbind(dt)
  lrn$feature_types = c("integer", "numeric")
  p = ppl("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true("encode" %in% names(p$pipeops))

  # missing fcts, assuming rpart can not do missings
  lrn$properties = c("multiclass", "twoclass")
  dt =  data.table("fct2" = factor(rep_len(c(letters[1:3], NA), tsk$nrow)))
  tsk$cbind(dt)
  p = ppl("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("imputehist", "missind", "encode", "imputenewlvl") %in% names(p$pipeops)))

  # no scaling
  p = ppl("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("imputehist", "missind") %in% names(p$pipeops)))

  # test on mixed, no missings
  tsk = tsk("boston_housing")
  lrn = lrn("regr.rpart")
  p = ppl("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true("encode" %in% names(p$pipeops))
  expect_true(!("missind" %in% names(p$pipeops)))
  expect_true(!("imputenewlvl" %in% names(p$pipeops)))

  # logical impute_missings
  p = ppl("robustify", task = tsk, learner = lrn, impute_missings = TRUE) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("imputehist", "missind", "imputenewlvl") %in% names(p$pipeops)))

  # no task
  p = robustify_pipeline() %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("char_to_fct", "imputehist", "missind", "imputenewlvl",
    "collapsefactors", "encode") %in% names(p$pipeops)))

  p = ppl("robustify", impute_missings = FALSE) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("char_to_fct", "fixfactors", "collapsefactors", "encode") %in% names(p$pipeops)))
  expect_true(!all(c("imputehist", "missind", "imputenewlvl") %in% names(p$pipeops)))

  # missings during predict
  dt = tsk$data()
  dt[2, 3] = NA
  tsk2 = TaskRegr$new(id = "bh", dt, target = "medv")
  lrn$properties = c("multiclass", "twoclass")
  p = ppl("robustify", impute_missings = TRUE) %>>% po(lrn)
  g = GraphLearner$new(p)
  g$train(tsk)
  prd = g$predict(tsk2)
  expect_prediction(prd)
})

test_that("Bagging Pipeline", {
  # classif
  tsk = tsk("iris")
  lrn = lrn("classif.rpart")
  p = ppl("bagging", graph = po(lrn), averager = po("classifavg"))
  expect_graph(p)
  expect_true(length(p$pipeops) == 10 + 10 + 1)

  # regr
  tsk = tsk("boston_housing")
  lrn = lrn("regr.rpart")
  p = ppl("bagging", graph = po(lrn), iterations = 5L, averager = po("regravg"))
  expect_graph(p)
  expect_true(length(p$pipeops) == 5 + 5 + 1)

  # graph instead of po(lrn)
  gr = po("pca") %>>% po(lrn)
  p = bagging_pipeline(graph = gr, iterations = 2L, averager = po("regravg"))
  expect_graph(p)
  expect_true(length(p$pipeops) == 2 + (2*2) + 1)
  res = resample(tsk$filter(1:50), GraphLearner$new(p), rsmp("holdout"))
  expect_resample_result(res)

  # no averager
  tsk = tsk("iris")
  lrn = lrn("classif.rpart")
  p = bagging_pipeline(graph = po(lrn))
  expect_graph(p)
  expect_true(length(p$pipeops) == 10 + 10)
  expect_data_table(p$output, nrows = 10)
})

test_that("Branching Pipeline", {
  lrns = map(list(lrn("classif.rpart"), lrn("classif.featureless")), po)
  task = mlr_tasks$get("boston_housing")
  gr = branch_pipeline(lrns)

  expect_graph(gr)
  expect_data_table(gr$param_set$deps)
  par_ids = unlist(map(lrns, function(x) as_graph(x)$param_set$ids()))
  expect_subset(par_ids, gr$param_set$ids())

  # FIXME: Check that this allows to change parameters
  # change parameters
  # gr$param_set$values$branch.selection = "classif.featureless"

  gr$train(tsk("iris"))
  out = gr$predict(tsk("iris"))$unbranch.output
  assert_prediction(out)
})
