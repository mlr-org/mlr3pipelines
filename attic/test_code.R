library("dimRed")
dat = loadDataSet("3D S Curve", n = 500)
emb = embed(dat, "Isomap")
plot(emb)

emb = embed(dat, "Isomap", knn = 10, ndim = 3, get_geod = TRUE)
plot(emb)

emb = embed(dat, "Isomap", knn = 10, ndim = 3, get_geod = TRUE, keep.org.data = FALSE)
plot(emb)

abc = Isomap()
abc@fun(data = dat, keep.org.data = FALSE, pars = list(knn = 50,
                                                ndim = 2,
                                                get_geod = FALSE))

emb = embed(dat, "abc", knn = 10, ndim = 3, get_geod = TRUE)



seq_len(min(3, ncol(emb@data@meta)))
plot(emb)
emb@data@meta$x = 1
seq_len(min(3, ncol(emb@data@meta)))

emb = embed(dat, "Isomap", knn = 10)
emb@data@meta$y = 1
plot(emb)


Scurve_data = cbind(dat@data,dat@meta[,1])




# Training Method

# Isomap Algorithm version
irisS4 = loadDataSet("Iris")
emb50 <- embed(irisS4, "Isomap", knn = 50)
plot(emb50, type = "2vars")

# PipeOpIsomap version
po50 = PipeOpIsomap$new("isomap")
po50$train(list(tsk("iris")))
plot(po50$state$e_vectors, col = po50$state$target)

# lowering neighbors
irisS4 = loadDataSet("Iris")
emb40 <- embed(irisS4, "Isomap", knn = 40)
plot(emb40, type = "2vars")

po40 = PipeOpIsomap$new("isomap", param_vals = list(k = 40))
po40$train(list(tsk("iris")))
plot(po40$state$e_vectors, col = po40$state$target)

# increasing dimensions ndim = 3
irisS4 = loadDataSet("Iris")
emb40 <- embed(irisS4, "Isomap", knn = 40, ndim = 3)
scatterplot3d(emb40@data@data)

po40 = PipeOpIsomap$new("isomap", param_vals = list(k = 40, ndim = 3))
po40$train(list(tsk("iris")))
scatterplot3d(po40$state$e_vectors)

# lowering dimensions ndim = 1
irisS4 = loadDataSet("Iris")
emb40 <- embed(irisS4, "Isomap", knn = 40, ndim = 1)
plot(emb40@data@data)

po40 = PipeOpIsomap$new("isomap", param_vals = list(k = 40, ndim = 1))
po40$train(list(tsk("iris")))
plot(po40$state$e_vectors)




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
