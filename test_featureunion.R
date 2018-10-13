# load_all("~/cos/paradox")
# load_all("~/cos/mlr3")
load_all()

task = mlr_tasks$get("iris")
dd = iris[, -5]
nd = iris[, -5]

op1 = PipeOpScaler$new()

op2 = PipeOpFanOut$new(2)

op3a = PipeOpPCA$new()

op3b = PipeOpNULL$new()

op4 = PipeOpFeatureUnion$new()

lrn = mlr_learners$get("classif.rpart")
op5 = PipeOpLearner$new(learner = lrn)

op1$set_next(list(op2))

op2$set_next(list(op3a, op3b))

op4$set_prev(list(op3a, op3b))

op4$set_next(list(op5))

trainGraph(op1, task)



