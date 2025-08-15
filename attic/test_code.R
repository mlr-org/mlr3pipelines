library("dimRed")
dat = loadDataSet("3D S Curve", n = 500)
Scurve_data = cbind(dat@data,dat@meta[,1])
colnames(Scurve_data) <- c("x1", "y1", "z1", "x2")
task_Scurve = TaskRegr$new(
  id = "SCurve_task",
  backend = Scurve_data,
  target = "x2"
)

po$train(list(task_Scurve))
po$state$e_vectors
plot(po$state$e_vectors)



# Training Method

# Isomap Algorithm version
irisS4 = loadDataSet("Iris")
emb <- embed(irisS4, "Isomap", knn = 50)
plot(emb, type = "2vars")

# PipeOpIsomap version
po = PipeOpIsomap$new("isomap")
po$train(list(tsk("iris")))
plot(po$state$e_vectors, col = po$state$target)


# Prediction Method

set.seed(567)
# Isomap Algorithm version
samp <- sample(nrow(irisS4), size = 65)
emb2 <- embed(irisS4, "Isomap", knn = 40)
emb3 <- predict(emb2, irisS4[samp])
plot(emb2, type = "2vars")
plot(emb3, type = "2vars")

# PipeOpIsomap version
po = PipeOpIsomap$new("isomap", k = 40)
po$train(list(tsk("iris")))
plot(po$state$e_vectors, col = po$state$target)
samp <- sample(nrow(iris), size = 65)
iris_new = iris[samp,]
po$predict(list(as_task_classif(iris_new, target = "Species")))
