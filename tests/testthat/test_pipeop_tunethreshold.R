context("tunethreshold")

test_that("threshold works for multiclass", {
  skip_if_not_installed("rpart")
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
  expect_set_equal(out$predict_types, c("response", "prob"))
  expect_data_table(as.data.table(out))
  expect_names(colnames(as.data.table(out)),
    permutation.of = c("truth", "response", "prob.setosa", "prob.versicolor", "prob.virginica", "row_ids"))
  po_thr$predict_type = "response"
  out = po_thr$predict(res2)[[1]]
  expect_prediction(out)
  expect_true(out$score() < 0.33)
  expect_set_equal(out$predict_types, "response")
  expect_data_table(as.data.table(out))
  expect_names(colnames(as.data.table(out)),
    permutation.of = c("truth", "response", "row_ids"))


})

test_that("threshold works for binary", {
  skip_if_not_installed("rpart")
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

  expect_set_equal(out$predict_types, c("response", "prob"))
  expect_data_table(as.data.table(out))
  expect_names(colnames(as.data.table(out)),
    permutation.of = c("truth", "response", "prob.pos", "prob.neg", "row_ids"))
  po_thr$predict_type = "response"
  out = po_thr$predict(res2)[[1]]
  expect_prediction(out)
  expect_true(out$score() < 0.33)
  expect_set_equal(out$predict_types, "response")
  expect_data_table(as.data.table(out))
  expect_names(colnames(as.data.table(out)),
    permutation.of = c("truth", "response", "row_ids"))

})

test_that("tunethreshold graph works", {
  skip_if_not_installed("rpart")

  graph = po("learner_cv", lrn("classif.rpart", predict_type = "prob")) %>>% po("tunethreshold")

  out = graph$train(tsk("pima"))

  expect_null(out$tunethreshold.output)

  out = graph$predict(tsk("pima"))

  expect_prediction(out$tunethreshold.output)

  glrn = as_learner(graph)

  glrn$train(tsk("pima"))

  expect_prediction(glrn$predict(tsk("pima")))


})

test_that("threshold works for classes that are not valid R names", {
  skip_if_not_installed("rpart")
  ppl = po("learner_cv", lrn("classif.rpart", predict_type = "prob")) %>>% po("tunethreshold")

  cols = c("0", "1", "-", "_")
  testtask = as_task_classif(
    data.frame(x = rep(1:3, each = 24), y = factor(rep(letters[1:3], each = 24)),
      target = factor(rep(c(cols, make.names(cols)), each = 9))),
      target = "target", id = "testtask"
  )

  ppl$train(testtask)

  expect_prediction(ppl$predict(testtask)[[1]])

  expect_numeric(ppl$state$tunethreshold$threshold, len = length(cols) * 2)
  expect_names(names(ppl$state$tunethreshold$threshold), permutation.of = c(cols, make.names(cols)))
})


test_that("threshold respects minimization / maximization", {
  skip_on_cran()
  set.seed(1)

  demotask = as_task_classif(id = "demotask", data.frame(
    propensity = rep(seq(0, 1, length.out = 11), each = 10),
    target = as.factor(upper.tri(matrix(1, nrow = 10, ncol = 11)))
  ), target = "target", positive = "TRUE")

  # task where 'propensity' gives the probability of 'target == TRUE'

  demotask_imbalanced = as_task_classif(id = "demotask2",
    demotask$data(rows = rep(c(rep(1:10, 30), 1:110), 10)),
    target = "target",
    positive = "TRUE"
  )

  task = demotask_imbalanced

  # classif.bacc: balanced accuracy is maximized.
  # for optimal accuracy, the decision should be split at propensity ~ 0.5
  # for balanced accuracy, the decision should be split at a lower propensity: since
  # there are more 'false' samples, getting more 'true positives' by lowering propensity
  # decisoin cutoff increases bacc more than getting fewer false negatives decreases it.
  graph = po("learner_cv", resampling.folds = 5, learner = lrn("classif.rpart", predict_type = "prob")) %>>%
    po("tunethreshold", measure = msr("classif.bacc"), optimizer = "random_search")

  learner1 = as_learner(graph)
  learner2 = lrn("classif.rpart")

  design = benchmark_grid(
    task,
    list(learner1, learner2),
    rsmp("repeated_cv", folds = 3, repeats = 3)
  )

  bmr = benchmark(design, store_models = FALSE, store_backends = FALSE)

  # balanced accuracy after threshold tuning should be greater than balanced accuracy for the untuned learner
  expect_true(diff(bmr$aggregate(msr("classif.bacc"))$classif.bacc) < 0)

  # more info:
  # bmr$aggregate(msrs(c("classif.bacc", "classif.acc")))

  # maximize TPR: should get close to 1
  graph = po("learner_cv", resampling.folds = 5, learner = lrn("classif.rpart", predict_type = "prob")) %>>%
    po("tunethreshold", measure = msr("classif.tpr"), optimizer = "random_search")

  learner1 = as_learner(graph)

  design = benchmark_grid(
    task,
    list(learner1, learner2),
    rsmp("cv", folds = 3)
  )

  bmr = benchmark(design, store_models = FALSE, store_backends = FALSE)

  # fpr for our constructed learner is < than for the untuned
  expect_true(diff(bmr$aggregate(msr("classif.tpr"))$classif.tpr) < 0)

  graph = po("learner_cv", resampling.folds = 5, learner = lrn("classif.rpart", predict_type = "prob")) %>>%
    po("tunethreshold", measure = msr("classif.fpr"), optimizer = "random_search")

  learner1 = as_learner(graph)

  design = benchmark_grid(
    task,
    list(learner1, learner2),
    rsmp("cv", folds = 3)
  )

  bmr = benchmark(design, store_models = FALSE, store_backends = FALSE)

  # fpr for our constructed learner is < than for the untuned
  expect_true(diff(bmr$aggregate(msr("classif.fpr"))$classif.fpr) > 0)

})

test_that("threshold works with cost measure", {

  skip_on_cran()
  set.seed(1)

  demotask = as_task_classif(id = "demotask", data.frame(
    propensity = rep(seq(0, 1, length.out = 11), each = 10),
    target = as.factor(upper.tri(matrix(1, nrow = 10, ncol = 11)))
  ), target = "target", positive = "TRUE")

  # task where 'propensity' gives the probability of 'target == TRUE'

  demotask_large = as_task_classif(id = "demotask2",
    demotask$data(rows = rep(1:110, 30)),
    target = "target",
    positive = "TRUE"
  )

  task = demotask_large
  costs1 = matrix(c(0, 10, 1, 0), nrow = 2)
  costs2 = matrix(c(0, 1, 10, 0), nrow = 2)
  dimnames(costs1) = list(response = task$class_names, truth = task$class_names)
  dimnames(costs2) = list(response = task$class_names, truth = task$class_names)
  mcosts1 = msr("classif.costs", costs = costs1, id = "cost1")
  mcosts2 = msr("classif.costs", costs = costs2, id = "cost2")

  graph1 = po("learner_cv", learner = lrn("classif.rpart", predict_type = "prob")) %>>%
    po("tunethreshold", measure = mcosts1, optimizer = "random_search")
  graph2 = po("learner_cv", learner = lrn("classif.rpart", predict_type = "prob")) %>>%
    po("tunethreshold", measure = mcosts2, optimizer = "random_search")

  learner1 = GraphLearner$new(graph1, id = "fnr.minimizer")
  learner2 = GraphLearner$new(graph2, id = "fpr.minimizer")
  design = benchmark_grid(
    task,
    list(learner1, learner2, lrn("classif.rpart")),
    rsmp("cv", folds = 10)
  )

  bmr = benchmark(design, store_models = FALSE, store_backends = FALSE)

  aggr = bmr$aggregate(c(msrs(c("classif.fnr", "classif.fpr")), list(mcosts1, mcosts2)))[
    c("fnr.minimizer", "fpr.minimizer", "classif.rpart"), on = "learner_id"]


  expect_equal(order(aggr$classif.fnr), c(1, 3, 2)) # fnr-minimizer first, then base model, then fpr.minimizer
  expect_equal(order(aggr$classif.fpr), c(2, 3, 1)) # fpr-minimizer first, then base model, then fnr.minimizer
  expect_equal(order(aggr$cost1), c(1, 3, 2)) # cost1 most satisfied by cost1-minimizing model
  expect_equal(order(aggr$cost2), c(2, 3, 1)) # cost2 most satisfied by cost2-minimizing model

})


test_that("threshold graph transparency", {

  lrn_prob = as_learner(
    po("learner_cv", lrn("classif.rpart", predict_type = "prob")) %>>%
      po("tunethreshold")
  )

  lrn_response = as_learner(
    po("learner_cv", lrn("classif.rpart", predict_type = "prob")) %>>%
      po("tunethreshold", predict_type = "response")
  )

  expect_equal(lrn_prob$predict_type, "prob")
  expect_equal(lrn_response$predict_type, "response")

  t = tsk("iris")

  pprob = lrn_prob$train(t)$predict(t)
  expect_set_equal(pprob$predict_types, c("response", "prob"))
  expect_names(colnames(as.data.table(pprob)),
    permutation.of = c("truth", "response", "prob.setosa", "prob.versicolor", "prob.virginica", "row_ids"))

  presp = lrn_response$train(t)$predict(t)
  expect_set_equal(presp$predict_types, "response")
  expect_names(colnames(as.data.table(presp)),
    permutation.of = c("truth", "response", "row_ids"))

})
