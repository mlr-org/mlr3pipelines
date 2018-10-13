load_all("~/cos/paradox")
load_all("~/cos/mlr3")
load_all()

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


