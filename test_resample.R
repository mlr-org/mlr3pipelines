load_all("~/cos/paradox")
load_all("~/cos/mlr3")
load_all()

task = mlr_tasks$get("iris")


op1 = PipeOpScaler$new()
lrn = mlr_learners$get("classif.rpart")
op2 = PipeOpLearner$new(learner = lrn)
pp = Pipeline$new(list(op2))
resa = mlr_resamplings$get("cv")
resa$folds = 2L
mm = mlr_measures$get("mmce")

rr = mlr3::resample(task, pp, resa, mm)
