context("PipeOp")

test_that("Gather parameters", {
  
  op1 = PipeOpScaler$new("myscaler")
  op2 = PipeOpPCA$new()
  op1$set_next(list(op2))
  
  lrn = mlr_learners$get("classif.rpart")
  op3 = PipeOpLearner$new(learner = lrn)
  op2$set_next(list(op3))
  
  pipeline_gather_params(op1)
  
})

test_that("PipeOp - simple pipe", {
  
  task = mlr_tasks$get("iris")
  
  op1 = PipeOpScaler$new()
  op2 = PipeOpPCA$new()
  lrn = mlr_learners$get("classif.rpart")
  lrn$predict_type <- "prob"
  
  op3 = PipeOpLearner$new(learner = lrn)
  
  op1$set_next(list(op2))
  op2$set_next(list(op3))  

  trainGraph(op1, task)

  op3$params$prediction
  
})

test_that("PipeOp", {
  
  set.seed(123)
  
  task = mlr_tasks$get("iris")
  
  op1 = PipeOpPCA$new()
  lrn1 <- mlr3:::LearnerClassifRpart$new(id = "l1")
  lrn2 <- mlr3:::LearnerClassifRpart$new(id = "l2")
  
  lrn2$par_vals <- list("maxdepth" = 1)

  op2a <- PipeOpLearner$new(lrn1)
  op2b <- PipeOpLearner$new(lrn2)
  
  op1$set_next(list(op2a, op2b))
  
  trainGraph(op1, task)
  
  expect_true(!is.null(op2a$params$model))
  expect_true(op2b$params$model)
    
})

test_that("PipeOp", {
  
  op = PipeOpScaler$new()
  op$set_prev(list(task))
  dd2 = op$train()
  print(op)
  nd2 = op$predict(nd)
  print(op)
})


test_that("PipeOp", {
  task = mlr_tasks$get("iris")
  dd = iris[, -5]
  nd = iris[, -5]

  op1 = PipeOpScaler$new()
  op2 = PipeOpPCA$new()
  lrn = mlr_learners$get("classif.rpart")
  op3 = PipeOpLearner$new(learner = lrn)

  pp = Pipeline$new(list(op1, op2, op3))

  mod = pp$train(dd)
  pred = pp$predict(nd)
})

# lrn = mlr.learners$get("classif.rpart")
# op = PipeOpLearner$new(learner = lrn)
# dd2 = op$train(dd)
# print(op)
# nd2 = op$predict(nd)
# print(op)



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
# opmm$set_par_vals(list(selected = "pca"))
# task = opmm$train(dd)

# ps = ParamSetFlat$new(params = list(
#   ParamReal$new(id = "minsplit", lower = 0, upper = 1)
# ))
# op = PipeOpTune$new(NULL, NULL, NULL, NULL, par_set = ps)
# dd2 = op$train(dd)
# nd2 = op$predict(nd)
