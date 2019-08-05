if (FALSE) {
op1 = PipeOpDownsample$new()
op2 = PipeOpLearner$new(mlr_learners$get("classif.rpart"))

g1 = ensure_graph(op1)
g2 = ensure_graph(op2)

g = op1 %>>% op2
g$plot()
g$channels
g$pipeops

x = greplicate(graph, 2)
x$channels
x$plot()

}


g = gunion(list(PipeOpScale$new(), PipeOpPCA$new())) %>>%
  PipeOpFeatureUnion$new(2)

g$plot()

g = g %>>% PipeOpLearner$new(mlr_learners$get("classif.rpart"))
task = mlr_tasks$get("iris")
g$fire(list(task = task), "train")
g$pipeops$classif.rpart$state$model
g$fire(list(task = task), "predict")


g = PipeOpChunk$new(3) %>>% PipeOpLearner$new(mlr_learners$get("classif.rpart"))

g$plot()

g = g %>>% PipeOpLearner$new(mlr_learners$get("classif.rpart"))
task = mlr_tasks$get("iris")
g$fire(list(task = task), "train")
g$pipeops$classif.rpart$state$model
g$fire(list(task = task), "predict")


g = PipeOpChunk$new(3) %>>% PipeOpLearner$new(mlr_learners$get("classif.rpart"))
g$plot()
g$channels
}

      if (pv[["resampling"]] == "nocv") {
        rdesc = mlr_resamplings$get("custom")$instantiate(task, train_set = list(seq_len(task$nrow)), test_set = list(seq_len(task$nrow)))
      } else {
        rdesc = mlr_resamplings$get(pv[["resampling"]])
        rdesc$param_set$values = list(folds = pv[["folds"]])
      }
