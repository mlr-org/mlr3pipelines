context("PipeOpFeatureUnion")

test_that("featureunion - basic properties", {
  po = PipeOpFeatureUnion$new(3)
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 3)
  expect_data_table(po$output, nrows = 1)

  expect_pipeop_class(PipeOpFeatureUnion, list(1))
  expect_pipeop_class(PipeOpFeatureUnion, list(3))

  po = PipeOpFeatureUnion$new()
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  expect_error(PipeOpFeatureUnion$new(1, collect_multiplicity = TRUE), regexp = "collect_multiplicity only works with innum == 0")
})

test_that("PipeOpFeatureUnion - train and predict", {
  # Define PipeOp's
  tsk = mlr_tasks$get("iris")
  keep1 = setdiff(tsk$feature_names, c("Sepal.Length", "Sepal.Width"))
  keep2 = setdiff(tsk$feature_names, c("Petal.Length", "Petal.Width"))
  t1 = tsk$clone()$select(keep1)
  t2 = tsk$clone()$select(keep2)

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
  skip_if_not_installed("rpart")
  # Define PipeOp's
  scatter = PipeOpCopy$new(2)
  op2a = PipeOpPCA$new()
  op2b = PipeOpNOP$new()
  op3 = PipeOpFeatureUnion$new(2)
  opdot = PipeOpFeatureUnion$new()

  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.rpart")
  op4 = PipeOpLearner$new(learner = lrn)

  # FIXME: Check param_set
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
  g = pipeline_greplicate(
    pos %>>% PipeOpPCA$new(),
    2
  ) %>>% PipeOpFeatureUnion$new(c("a", "b"))
  task = mlr_tasks$get("iris")
  # TODO: the following was broken by mlr3 recently
  # expect_error(g$train(task), "Assertion on 'rows'")

  # Differing target columns
  po = PipeOpFeatureUnion$new()
  expect_error(po$train(list(tsk("iris"), tsk("mtcars")), regexp = "All tasks must have the same target columns"))
})

test_that("PipeOpFeatureUnion - levels are preserved", {
  tbl1 = data.table(x1 = factor(letters[10:14], levels = letters), y1 = letters[1:5], target = 1:5)
  tbl2 = data.table(x2 = factor(letters[10:14], levels = letters), y2 = letters[1:5], target = 1:5)

  tsk1 = TaskRegr$new("tsk1", as_data_backend(tbl1), "target")
  tsk2 = TaskRegr$new("tsk2", as_data_backend(tbl2), "target")

  tsk1$col_info
  tsk2$col_info

  pofu = PipeOpFeatureUnion$new(2)
  expect_true(!pofu$is_trained)
  pofu$train(list(tsk1, tsk2))[[1]]$col_info
  expect_true(pofu$is_trained)
  pofu$train(list(tsk1$filter(3:5), tsk2$filter(3:5)))[[1]]$col_info
  expect_true(pofu$is_trained)
})

test_that("feature renaming", {
  skip_if_not_installed("rpart")
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

  # FIXME: Check param_set
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

  expect_equal(po$train(list(task, task, PipeOpPCA$new()$train(list(task))[[1]]))[[1]]$feature_names,
    c(task$feature_names, paste0("a.", task$feature_names), paste0("a.PC", 1:4)))
})

# https://github.com/mlr-org/mlr3pipelines/issues/216
## test_that("union with missing rows", {
##
##   tsk = mlr_tasks$get("iris")
##
##   posu = PipeOpSubsample$new()
##
##   t1 = posu$train(list(tsk))[[1]]
##   t2 = posu$train(list(tsk))[[1]]
##
##   t1$row_ids
##   t2$row_ids
##
##   pofu = PipeOpFeatureUnion$new()
##
##   pofu$train(list(t1, t2))
## })

test_that("featureunion - duplicates in feature names", {
  tsk = mlr_tasks$get("iris")

  g = pipeline_greplicate(PipeOpPCA$new(), 2) %>>% PipeOpFeatureUnion$new(2)

  # this should work (just keeps each PC a single time)
  train_out_g = g$train(tsk)

  popca = PipeOpPCA$new()
  train_out_pca = popca$train(list(tsk))
  expect_equal(train_out_g[[1]]$data(), train_out_pca[[1]]$data())

  # the following should not (duplicated feature names but actually different values)
  dat = tsk$data()
  dat$Petal.Length = rnorm(150)
  dat$Sepal.Width[1] = 999
  tsk2 = TaskClassif$new("tsk2", backend = dat, target = "Species")

  po = PipeOpFeatureUnion$new()
  expect_error(po$train(list(tsk, tsk2)), regexp = "different features sharing the same feature name")
})

test_that("featureunion - collect_multiplicity", {
  po = PipeOpFeatureUnion$new(0, collect_multiplicity = TRUE)
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  tsk = mlr_tasks$get("iris")
  keep1 = setdiff(tsk$feature_names, c("Sepal.Length", "Sepal.Width"))
  keep2 = setdiff(tsk$feature_names, c("Petal.Length", "Petal.Width"))
  t1 = tsk$clone()$select(keep1)
  t2 = tsk$clone()$select(keep2)

  train_out = po$train(list(as.Multiplicity(list(t1, t2))))
  predict_out = po$predict(list(as.Multiplicity(list(t1, t2))))
  expect_equal(train_out[[1]]$data, tsk$data)
  expect_equal(predict_out[[1]]$data, tsk$data)
})

test_that("featureunion - cbind_tasks - duplicates", {
  # use iris three times, but the third time with a new inprefix;
  # first task has the same target but a single mew non-overlapping feature
  task1 = tsk("iris")
  task1$filter(1:10)
  task2 = task1$clone(deep = TRUE)
  task3 = task1$clone(deep = TRUE)
  inputs = list(TaskClassif$new("test", backend = cbind(task1$data(cols = "Species"), x = 1:10), target = "Species"), task1, task2, task3)

  output = cbind_tasks(inputs, assert_targets_equal = TRUE, inprefix = c("", "", "", "new_iris"))
  new_iris_names = paste0("new_iris.", task1$feature_names)

  expect_set_equal(output$feature_names, c("x", task1$feature_names, new_iris_names))
  expect_equal(output$data(cols = c("Species", task1$feature_names)), task1$data())
  expect_equal(output$data(cols = "x"), inputs[[1L]]$data(cols = "x"))
  expect_equal(output$data(cols = "x"), inputs[[1L]]$data(cols = "x"))
  expect_equivalent(output$data(cols = c("Species", new_iris_names)), task1$data())
})

test_that("featureunion - does not drop 'x' column", {
  task1 = as_task_regr(data.table(
    z = 1:10,
    y = 1:10
  ), target = "y")

  task2 = as_task_regr(data.table(
    x = 1:10,
    y = 1:10
  ), target = "y")

  taskout = po("featureunion")$train(list(task1, task2))[[1L]]
  expect_permutation(taskout$feature_names, c("x", "z"))
})
