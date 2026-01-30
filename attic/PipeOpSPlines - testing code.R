
# po = po("splines", df = 1)
# po_result = po$train(list(tsk("iris")))[[1]]$data()

# po = po("splines", type = "polynomial", df = 2, degree = 4)
# po_result = po$train(list(tsk("iris")))[[1]]$data()
# po_result_calc = stats::model.matrix(Species ~ splines::bs(Sepal.Length) + splines::bs(Sepal.Width) + splines::bs(Petal.Length) + splines::bs(Petal.Width), data = iris)

# selecting columns
# sel_cyl = selector_grep("Sepal.Length")
# pos = po("basissplines", affect_columns = sel_cyl)
# pos_result = pos$train(list(tsk("iris")))[[1]]$data()
# pos_result_calc = stats::model.matrix(Species ~ splines::bs(Sepal.Length), data = iris)


# podf3 = po("basissplines", df = 3)
# podf3_result = podf3$train(list(tsk("iris")))[[1]]$data()
# podf3_result_calc = stats::model.matrix(Species ~ splines::bs(Sepal.Length, df = 3) + splines::bs(Sepal.Width, df = 3) + splines::bs(Petal.Length, df = 3) + splines::bs(Petal.Width, df = 3), data = iris)

# podf4 = po("basissplines", df = 4)
# podf4_result = podf4$train(list(tsk("iris")))[[1]]$data()
# podf4_result_calc = stats::model.matrix(Species ~ splines::bs(Sepal.Length, df = 4) + splines::bs(Sepal.Width, df = 4) + splines::bs(Petal.Length, df = 4) + splines::bs(Petal.Width, df = 4), data = iris)

# podf7 = po("basissplines", df = 7)
# podf7_result = podf7$train(list(tsk("iris")))[[1]]$data()
# podf7_result_calc = stats::model.matrix(Species ~ splines::bs(Sepal.Length, df = 7) + splines::bs(Sepal.Width, df = 7) + splines::bs(Petal.Length, df = 7) + splines::bs(Petal.Width, df = 7), data = iris)

# podeg3df2 = po("basissplines", degree = 3, df = 2)
# podeg3df2_result = podeg3df2$train(list(tsk("iris")))[[1]]$data()
# podeg3df2_result_calc = stats::model.matrix(Species ~ splines::bs(Sepal.Length, degree = 3, df = 2) + splines::bs(Sepal.Width, degree = 3, df = 2) + splines::bs(Petal.Length, degree = 3, df = 2) + splines::bs(Petal.Width, degree = 3, df = 2), data = iris)


# podeg5df8 = po("basissplines", degree = 5, df = 8)
# podeg5df8_result = podeg5df8$train(list(tsk("iris")))[[1]]$data()
# podeg5df8_result_calc = stats::model.matrix(Species ~ splines::bs(Sepal.Length, degree = 5, df = 8) + splines::bs(Sepal.Width, degree = 5, df = 8) + splines::bs(Petal.Length, degree = 5, df = 8) + splines::bs(Petal.Width, degree = 5, df = 8), data = iris)

# pons = po("basissplines", factor = "natural")
# pons_result = pons$train(list(tsk("iris")))[[1]]$data()
# pons_result_calc = stats::model.matrix(Species ~ splines::ns(Sepal.Length) + splines::ns(Sepal.Width) + splines::ns(Petal.Length) + splines::ns(Petal.Width), data = iris)

# pons_error = po("basissplines", factor = "natural", df = 3, degree = 4)

# ponsdf5 = po("basissplines", factor = "natural", df = 5)
# ponsdf5_result = ponsdf5$train(list(tsk("iris")))[[1]]$data()
# ponsdf5_result_calc = stats::model.matrix(Species ~ splines::ns(Sepal.Length, df = 5) + splines::ns(Sepal.Width, df = 5) + splines::ns(Petal.Length, df = 5) + splines::ns(Petal.Width, df = 5), data = iris)




# pop = po("basissplines", df = 5)
# pop$train(list(tsk("mtcars")))[[1]]$data()

# poc = po("basissplines", df = 4, factor = "natural")
# poc$train(list(tsk("mtcars")))[[1]]$data()

# df als hyperparameterf
# das ziel ist es dass wir diese model.matrix für alle features kriegen
# original features behalten dann feature_union ==> egaluser verantwortung


#splines.cyl.1

# task = tsk("mtcars")

# list(task)[[1]]$data()
# pop = po("modelmatrix", formula = ~ splines::ns(task$data()$cyl, 2) + splines::ns(task$data()$hp, 2) +
#           splines::ns(task$data()$disp, 2) + splines::ns(task$data()$drat, 2) + splines::ns(task$data()$wt, 2) +
#           splines::ns(task$data()$qsec, 2) + splines::ns(task$data()$vs, 2) + splines::ns(task$data()$am, 2) +
#           splines::ns(task$data()$gear, 2) + splines::ns(task$data()$carb, 2))
# pop$train(list(task))[[1]]$data()

# pob = po("modelmatrix", formula = ~ splines::bs(task$data()$cyl, 2) + splines::bs(task$data()$hp, 2) +
#           splines::bs(task$data()$disp, 2) + splines::bs(task$data()$drat, 2) + splines::bs(task$data()$wt, 2) +
#           splines::bs(task$data()$qsec, 2) + splines::bs(task$data()$vs, 2) + splines::bs(task$data()$am, 2) +
#           splines::bs(task$data()$gear, 2) + splines::bs(task$data()$carb, 2))

# pob$train(list(task))[[1]]$data()


# fit <- lm(mpg ~ splines::ns(cyl, df = 2) + splines::ns(hp, df = 2), data = mtcars)
# model.matrix(fit) # this is what we want to get as a result from PipeOpSplineBasis

# as.data.frame(stats::model.matrix(mpg ~ splines::ns(cyl, 2) + splines::ns(task$data()$hp, 2), data = mtcars))
