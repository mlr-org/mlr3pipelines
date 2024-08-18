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
  expect_true(diff(bmr$aggregate(msr("classif.fpr"))$classif.tpr) < 0)

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
