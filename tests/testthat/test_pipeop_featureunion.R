context("PipeOpFeatureUnion")


test_that("featureunion - basic properties", {
  po = PipeOpFeatureUnion$new(3)
  expect_pipeop(po)
  expect_data_table(po$input, nrow = 3)
  expect_data_table(po$output, nrow = 1)
})


test_that("PipeOpFeatureunion - train and predict", {
  # Define PipeOp's
  tsk = mlr_tasks$get("iris")
  t1 = tsk$clone()$set_col_role(c("Sepal.Length", "Sepal.Width"), character())
  t2 = tsk$clone()$set_col_role(c("Petal.Length", "Petal.Width"), character())

  po = PipeOpFeatureUnion$new(2)
  tout = train_pipeop(po, list(t1, t2))
  expect_equivalent(tout[[1]]$feature_names, tsk$feature_names)
  expect_equivalent(tout[[1]]$target_names, tsk$target_names)

  pout = predict_pipeop(po, list(t1, t2))
  expect_equivalent(pout[[1]]$feature_names, tsk$feature_names)
  expect_equivalent(pout[[1]]$target_names, tsk$target_names)

})

# test_that("PipeOpBranch - train and predict", {

#   # Define PipeOp's
#   scatter = PipeOpCopy$new(2)
#   op2a = PipeOpPCA$new()
#   op2b = PipeOpNULL$new()
#   op3 = PipeOpFeatureUnion$new(2)

#   task = mlr_tasks$get("iris")
#   lrn = mlr_learners$get("classif.rpart")
#   op4 = PipeOpLearner$new(learner = lrn)

#   #  FIXME: Check param_set
#   param_names_union = c(
#     "OpNULL.Petal.Width", "OpNULL.Petal.Length",
#     "pca.PC1", "OpNULL.Sepal.Length", "pca.PC2",
#     "OpNULL.Sepal.Width", "pca.PC3")

#   graph = scatter %>>% gunion(op2a, op2b)
#   graph = graph %>>% op3
#   expect_false(graph$is_trained)
#   # test_basic_graph_props(graph)
#   expect_true(length(graph) == 4L)

#   # trained = graph$train(task)
#   # expect_equal(trained$feature_names, c("PC1", "PC2", "PC3", "PC4", "Petal.Length",
#   #   "Petal.Width", "Sepal.Length", "Sepal.Width"))
#   # expect_true(graph$is_trained)
# })

