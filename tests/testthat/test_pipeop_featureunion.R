context("PipeOpFeatureUnion")


test_that("featureunion - basic properties", {
  po = PipeOpFeatureUnion$new(3)
  expect_pipeop(po)
  expect_data_table(po$input, nrow = 3)
  expect_data_table(po$output, nrow = 1)

  expect_pipeop_class(PipeOpFeatureUnion, list(1))
  expect_pipeop_class(PipeOpFeatureUnion, list(3))
  expect_error(PipeOpFeatureUnion$new(0))
})


test_that("PipeOpFeatureUnion - train and predict", {
  # Define PipeOp's
  tsk = mlr_tasks$get("iris")
  t1 = tsk$clone()$set_col_role(c("Sepal.Length", "Sepal.Width"), character())
  t2 = tsk$clone()$set_col_role(c("Petal.Length", "Petal.Width"), character())

  expect_task(cbind_tasks(inputs = list(t1, t2), TRUE))

  po = PipeOpFeatureUnion$new(2)

  tout = train_pipeop(po, list(t1, t2))
  expect_equivalent(tout[[1]]$feature_names, tsk$feature_names)
  expect_equivalent(tout[[1]]$target_names, tsk$target_names)

  pout = predict_pipeop(po, list(t1, t2))
  expect_equivalent(pout[[1]]$feature_names, tsk$feature_names)
  expect_equivalent(pout[[1]]$target_names, tsk$target_names)
})

test_that("PipeOpFeatureUnion - train and predict II", {

  # Define PipeOp's
  scatter = PipeOpCopy$new(2)
  op2a = PipeOpPCA$new()
  op2b = PipeOpNULL$new()
  op3 = PipeOpFeatureUnion$new(2)

  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.rpart")
  op4 = PipeOpLearner$new(learner = lrn)

  #  FIXME: Check param_set
  param_names_union = c(
    "OpNULL.Petal.Width", "OpNULL.Petal.Length",
    "pca.PC1", "OpNULL.Sepal.Length", "pca.PC2",
    "OpNULL.Sepal.Width", "pca.PC3")

  graph = scatter %>>% gunion(list(op2a, op2b))
  graph = graph %>>% op3
  expect_false(graph$is_trained)
  expect_graph(graph)
  expect_true(length(graph$pipeops) == 4L)

  result = graph$train(task)
  expect_equal(names(result), "featureunion.output")
  trained = result[[1]]
  expect_set_equal(trained$feature_names, c("PC1", "PC2", "PC3", "PC4", "Petal.Length",
    "Petal.Width", "Sepal.Length", "Sepal.Width"))
  expect_true(graph$is_trained)
})

test_that("Test wrong inputs", {
  # Differing rows
  pos = PipeOpSubsample$new()
  pos$param_set$values$frac = 0.5
  g = greplicate(
    pos %>>% PipeOpPCA$new(),
    2
  ) %>>% PipeOpFeatureUnion$new(2)
  task = mlr_tasks$get("iris")
  expect_error(g$train(task), "Assertion on 'rows'")
})

# FIXME: Somewhat depends on https://github.com/mlr-org/mlr3/issues/268
# test_that("Duplicate Features", {
#   # Define PipeOp's
#   tsk = mlr_tasks$get("iris")
#   t1 = tsk$clone()$set_col_role("Sepal.Length", character())
#   t2 = tsk$clone()$set_col_role(c("Petal.Length", "Petal.Width"), character())

#   po = PipeOpFeatureUnion$new(2)

#   tout = train_pipeop(po, list(t1, t2))
#   expect_equivalent(tout[[1]]$feature_names, c())
#   expect_equivalent(tout[[1]]$target_names, tsk$target_names)
# })

# FIXME: depends on mlr-org/mlr3#179
## test_that("PipeOpFeatureUnion - levels are preserved", {
##
##   tbl1 = data.table(x1 = factor(letters[10:14], levels = letters), y1 = letters[1:5], target = 1:5)
##   tbl2 = data.table(x2 = factor(letters[10:14], levels = letters), y2 = letters[1:5], target = 1:5)
##
##   tsk1 = TaskRegr$new("tsk1", as_data_backend(tbl1), "target")
##   tsk2 = TaskRegr$new("tsk2", as_data_backend(tbl2), "target")
##
##   tsk1$col_info
##   tsk2$col_info
##
##   pofu = PipeOpFeatureUnion$new(2)
##
##   pofu$train(list(tsk1, tsk2))[[1]]$col_info
##
##
##   pofu$train(list(tsk1$filter(3:5), tsk2$filter(3:5)))[[1]]$col_info
##
##
## })
