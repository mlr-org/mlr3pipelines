context("PipeOpFeatureUnion")


test_that("featureunion - basic properties", {
  po = PipeOpFeatureUnion$new(3)
  expect_pipeop(po)
  expect_data_table(po$input, nrow = 3)
  expect_data_table(po$output, nrow = 1)

  expect_pipeop_class(PipeOpFeatureUnion, list(1))
  expect_pipeop_class(PipeOpFeatureUnion, list(3))

  po = PipeOpFeatureUnion$new()
  expect_pipeop(po)
  expect_data_table(po$input, nrow = 1)
  expect_data_table(po$output, nrow = 1)

})


test_that("PipeOpFeatureUnion - train and predict", {
  # Define PipeOp's
  tsk = mlr_tasks$get("iris")
  t1 = tsk$clone()$set_col_role(c("Sepal.Length", "Sepal.Width"), character())
  t2 = tsk$clone()$set_col_role(c("Petal.Length", "Petal.Width"), character())

  expect_task(cbind_tasks(inputs = list(t1, t2), TRUE, c("", "")))

  po = PipeOpFeatureUnion$new(2)

  tout = train_pipeop(po, list(t1, t2))
  expect_equivalent(tout[[1]]$feature_names, tsk$feature_names)
  expect_equivalent(tout[[1]]$target_names, tsk$target_names)

  pout = predict_pipeop(po, list(t1, t2))
  expect_equivalent(pout[[1]]$feature_names, tsk$feature_names)
  expect_equivalent(pout[[1]]$target_names, tsk$target_names)

  po = PipeOpFeatureUnion$new()

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
  op2b = PipeOpNOP$new()
  op3 = PipeOpFeatureUnion$new(2)
  opdot = PipeOpFeatureUnion$new()


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

  graph = scatter %>>% gunion(list(op2a, op2b))
  graph = graph %>>% opdot
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

test_that("PipeOpFeatureUnion - levels are preserved", {

  tbl1 = data.table(x1 = factor(letters[10:14], levels = letters), y1 = letters[1:5], target = 1:5)
  tbl2 = data.table(x2 = factor(letters[10:14], levels = letters), y2 = letters[1:5], target = 1:5)

  tsk1 = TaskRegr$new("tsk1", as_data_backend(tbl1), "target")
  tsk2 = TaskRegr$new("tsk2", as_data_backend(tbl2), "target")

  tsk1$col_info
  tsk2$col_info

  pofu = PipeOpFeatureUnion$new(2)

  pofu$train(list(tsk1, tsk2))[[1]]$col_info


  pofu$train(list(tsk1$filter(3:5), tsk2$filter(3:5)))[[1]]$col_info


})

test_that("feature renaming", {

  expect_pipeop_class(PipeOpFeatureUnion, list(letters[1:3]))

  expect_equal(nrow(PipeOpFeatureUnion$new(c("a", "b", "c"))$input), 3)
  expect_equal(nrow(PipeOpFeatureUnion$new("a")$input), 1)

  po = PipeOpFeatureUnion$new(c("", "a", "b"))

  task = mlr_tasks$get("iris")

  expect_equal(po$train(list(task, task, task))[[1]]$feature_names,
    c(task$feature_names, paste0("a.", task$feature_names), paste0("b.", task$feature_names)))

  po = PipeOpFeatureUnion$new(c("", "a", "a"))

  expect_equal(po$train(list(task, task, PipeOpPCA$new()$train(list(task))[[1]]))[[1]]$feature_names,
    c(task$feature_names, paste0("a.", task$feature_names), paste0("a.PC", 1:4)))

  # Define PipeOp's
  scatter = PipeOpCopy$new(2)
  op2a = PipeOpPCA$new()
  op2b = PipeOpNOP$new()
  op3 = PipeOpFeatureUnion$new(c("", "XX"))

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
  expect_set_equal(trained$feature_names, c("PC1", "PC2", "PC3", "PC4", "XX.Petal.Length",
    "XX.Petal.Width", "XX.Sepal.Length", "XX.Sepal.Width"))
  expect_true(graph$is_trained)

  po = PipeOpFeatureUnion$new(c("z", "a", "a"))

# FIXME: this needs https://github.com/mlr-org/mlr3/issues/268
#  expect_equal(po$train(list(task, task, PipeOpPCA$new()$train(list(task))[[1]]))[[1]]$feature_names,
#    c(task$feature_names, paste0("a.", task$feature_names), paste0("a.PC", 1:4)))

})

test_that("union with missing rows", {

  tsk = mlr_tasks$get("iris")

  posu = PipeOpSubsample$new()

  t1 = posu$train(list(tsk))[[1]]
  t2 = posu$train(list(tsk))[[1]]

  t1$row_ids
  t2$row_ids

  pofu = PipeOpFeatureUnion$new()

  pofu$train(list(t1, t2))
})
