context("PipeOp")

library(BBmisc)
ad_all("../paradox")
load_all("../mlr3")
load_all()

test_that("PipeOp", {
  task = mlr_tasks$get("iris")
  dd = iris[, -5]
  nd = iris[, -5]

  op = PipeOpScaler$new()
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
