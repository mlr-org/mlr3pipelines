context("PipeOpTargetTrafoSimple")

test_that("PipeOpTargetTrafoSimple - basic properties", {
  expect_pipeop_class(PipeOpTargetTrafoSimple, list(id = "po"))

  po = PipeOpTargetTrafoSimple$new("po")

  expect_pipeop(po)

  g = Graph$new()
  g$add_pipeop(PipeOpTargetTrafoSimple$new())
  g$add_pipeop(LearnerRegrRpart$new())
  g$add_pipeop(PipeOpTargetInverter$new())
  g$add_edge(src_id = "targettrafosimple", dst_id = "targetinverter", src_channel = 1, dst_channel = 1)
  g$add_edge(src_id = "targettrafosimple", dst_id = "regr.rpart", src_channel = 2, dst_channel = 1)
  g$add_edge(src_id = "regr.rpart", dst_id = "targetinverter", src_channel = 1, dst_channel = 2)

  expect_graph(g)

  task = mlr_tasks$get("boston_housing")
  task_copy = task$clone(deep = TRUE)
  address_in = address(task)
  train_out = g$train(task)
  expect_null(train_out[[1L]])
  expect_length(g$state[[1L]], 0L)
  expect_length(g$state[[3L]], 0L)

  predict_out = g$predict(task)

  expect_equal(task, task_copy)
  expect_equal(address_in, address(task))

  learner = LearnerRegrRpart$new()
  learner$train(task)

  expect_equal(learner$predict(task), predict_out[[1L]])

  # set a new target name
  g$pipeops$targettrafosimple$param_set$values$new_target_name = "test"
  train_out = g$train(task)
  expect_equal("test", g$state[[2L]]$train_task$target_names)
  expect_true("medv" %nin% g$state[[2L]]$train_task$feature_names)
  predict_out = g$predict(task)
})

test_that("PipeOpTargetTrafoSimple - log base 2 trafo", {
 g = Graph$new()
 g$add_pipeop(PipeOpTargetTrafoSimple$new("logtrafo",
   param_vals = list(
     trafo = function(x) log(x, base = 2),
     inverter = function(x) 2 ^ x)
   )
 )
 g$add_pipeop(LearnerRegrRpart$new())
 g$add_pipeop(PipeOpTargetInverter$new())
 g$add_edge(src_id = "logtrafo", dst_id = "targetinverter", src_channel = 1, dst_channel = 1)
 g$add_edge(src_id = "logtrafo", dst_id = "regr.rpart", src_channel = 2, dst_channel = 1)
 g$add_edge(src_id = "regr.rpart", dst_id = "targetinverter", src_channel = 1, dst_channel = 2)

 task = mlr_tasks$get("boston_housing")
 train_out = g$train(task)
 predict_out = g$predict(task)

 dat = task$data()
 dat$medv = log(dat$medv, base = 2)
 task_log = TaskRegr$new("boston_housing_log", backend = dat, target = "medv")

 learner = LearnerRegrRpart$new()
 learner$train(task_log)

 learner_predict_out = learner$predict(task_log)
 expect_equal(2 ^ learner_predict_out$truth, predict_out[[1L]]$truth)
 expect_equal(2 ^ learner_predict_out$response, predict_out[[1L]]$response)
})
