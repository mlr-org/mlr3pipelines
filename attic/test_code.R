library("dimRed")
dat = loadDataSet("3D S Curve", n = 500)

emb = embed(dat, "Isomap", knn = 10)
seq_len(min(3, ncol(emb@data@meta)))
plot(emb)
emb@data@meta$x = 1
seq_len(min(3, ncol(emb@data@meta)))
plot(emb)
emb = embed(dat, "Isomap", knn = 10)
emb@data@meta$y = 1
plot(emb)


Scurve_data = cbind(dat@data,dat@meta[,1])




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
emb2 <- embed(irisS4, "Isomap", knn = 38)
emb3 <- predict(emb2, irisS4[samp])
plot(emb2, type = "2vars")
plot(emb3, type = "2vars")

# PipeOpIsomap version
po = PipeOpIsomap$new("isomap")
po$param_set$values$k = 38
po$train(list(tsk("iris")))
plot(po$state$e_vectors, col = po$state$target)
iris_filtered = tsk("iris")$filter(samp)
po$predict(list(iris_filtered))
plot(predict)


po = PipeOpIsomap$new("isomap")
po$param_set$values$k = 30
po$train(list(tsk("mtcars")))
plot(po$state$e_vectors, col = po$state$target)
