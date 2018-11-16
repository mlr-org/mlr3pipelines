context("PipeOp")


test_that("PipeOp - simple pipe", {

  task = mlr_tasks$get("iris")

  op1 = PipeOpScaler$new()
  expect_class(op1, "PipeOpScaler")
  expect_false(op1$is_learnt)

  n1 <- GraphNode$new(op1)
  expect_false(n1$can_fire)

  trainGraph(n1, task)
  expect_true(n1$can_fire)

  expect_class(op1, "PipeOpScaler")
  expect_true(op1$is_learnt)
  expect_class(op1$result, "Task")

})

test_that("PipeOp - learner pipe", {

  task = mlr_tasks$get("iris")

  lrn = mlr_learners$get("classif.rpart")
  lrn$predict_type <- "prob"

  op1 = PipeOpScaler$new()
  op2 = PipeOpPCA$new()
  op3 = PipeOpLearner$new(learner = lrn)

  root <- GraphNode$new(op1)
  root$set_next(GraphNode$new(op2))$set_next(GraphNode$new(op3))

  trainGraph(root, task)
  expect_class(op1$result, "Task")
  expect_class(op2$result, "Task")
  expect_class(op3$result, "Task")

  op3$params$prediction
})


test_that("Gather parameters", {

  op1 = PipeOpScaler$new("myscaler")
  op2 = PipeOpPCA$new()

  root = GraphNode$new(op1)
  root$set_next(GraphNode$new(op2))

  lrn = mlr_learners$get("classif.rpart")
  op3 = PipeOpLearner$new(learner = lrn)
  root$
    set_next(GraphNode$new(op2))$
    set_next(GraphNode$new(op3))

  ps = graph_gather_params(root)

  expect_class(ps, "ParamSet")
  expect_subset(sprintf("%s:%s", op1$id, op1$param_set$ids), ps$ids)
  expect_subset(sprintf("%s:%s", op2$id, op2$param_set$ids), ps$ids)
  expect_subset(sprintf("%s:%s", lrn$id, lrn$param_set$ids), ps$ids)
})

test_that("PipeOp", {
  
  task = mlr_tasks$get("iris")

  op1 = PipeOpPCA$new()
  n1 = GraphNode$new(op1)

  lrn1 = mlr3:::LearnerClassifRpart$new(id = "l1")
  lrn2 = mlr3:::LearnerClassifRpart$new(id = "l2")

  lrn2$param_vals = list("maxdepth" = 1)

  op2a = PipeOpLearner$new(lrn1)
  op2b = PipeOpLearner$new(lrn2)

  n1$set_next(list(GraphNode$new(op2a), GraphNode$new(op2b)))

  trainGraph(n1, task)

  expect_true(!is.null(op2a$params$model))
  expect_true(!is.null(op2b$params$model))

})

# test_that("PipeOp", {
#   task = mlr_tasks$get("iris")
#   dd = iris[, -5]
#   nd = iris[, -5]
#
#   op1 = PipeOpScaler$new()
#   op2 = PipeOpPCA$new()
#   lrn = mlr_learners$get("classif.rpart")
#   op3 = PipeOpLearner$new(learner = lrn)
#
#   pp = Pipeline$new(list(op1, op2, op3))
#
#   mod = pp$train(dd)
#   pred = pp$predict(nd)
# })

# op1 = PipeOpScaler$new()
# op2 = PipeOpPCA$new()
# lrn = mlr.learners$get("classif.rpart")
# op3 = PipeOpLearner$new(learner = lrn)

# pp = Pipeline$new(list(op1, op2, op3))

# mod = pp$train(dd)
# pred = pp$predict(nd)

# opmm = PipeOpMultiplexer$new(list(op1, op2))
# task = opmm$train(dd)
# opmm$reset()
# opmm$set_param_vals(list(selected = "pca"))
# task = opmm$train(dd)

# ps = ParamSetFlat$new(params = list(
#   ParamReal$new(id = "minsplit", lower = 0, upper = 1)
# ))
# op = PipeOpTune$new(NULL, NULL, NULL, NULL, param_set = ps)
# dd2 = op$train(dd)
# nd2 = op$predict(nd)
