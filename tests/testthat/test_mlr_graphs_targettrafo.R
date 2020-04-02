context("ppl - pipeline_targettrafo")

test_that("Target Trafo Pipeline", {
  task = tsk("boston_housing")

  targettrafo = ppl("targettrafo", graph = PipeOpLearner$new(LearnerRegrRpart$new()))
  targettrafo$param_set$values$targettrafosimple.trafo = function(x) log(x, base = 2)
  targettrafo$param_set$values$targettrafosimple.inverter = function(x) 2 ^ x
  expect_graph(targettrafo)
  expect_true(length(targettrafo$pipeops) == 1 + 1 + 1)

  g = Graph$new()
  g$add_pipeop(PipeOpTargetTrafoSimple$new(param_vals = list(
    trafo = function(x) log(x, base = 2),
    inverter = function(x) 2 ^ x)
    )
  )
  g$add_pipeop(LearnerRegrRpart$new())
  g$add_pipeop(PipeOpTargetInverter$new())
  g$add_edge(src_id = "targettrafosimple", dst_id = "targetinverter", src_channel = 1, dst_channel = 1)
  g$add_edge(src_id = "targettrafosimple", dst_id = "regr.rpart", src_channel = 2, dst_channel = 1)
  g$add_edge(src_id = "regr.rpart", dst_id = "targetinverter", src_channel = 1, dst_channel = 2)

  train_tt = targettrafo$train(task)
  predict_tt = targettrafo$predict(task)
  train_g = g$train(task)
  predict_g = g$predict(task)

  expect_equal(train_tt, train_g)
  expect_equal(predict_tt, predict_g)

  targettrafo_g = ppl("targettrafo", graph = PipeOpPCA$new() %>>% PipeOpLearner$new(LearnerRegrRpart$new()))
  targettrafo_g$param_set$values$targettrafosimple.trafo = function(x) log(x, base = 2)
  targettrafo_g$param_set$values$targettrafosimple.inverter = function(x) 2 ^ x

  expect_graph(targettrafo_g)
  expect_true(length(targettrafo_g$pipeops) == 1 + 1 + 1 + 1)

  train_ttg = targettrafo_g$train(task)
  predict_ttg = targettrafo_g$predict(task)

  # assertions on graph
  expect_error(ppl("targettrafo", graph = PipeOpNOP$new()), regexp = "PipeOpLearner")
  expect_error(ppl("targettrafo", graph = PipeOpLearner$new(LearnerRegrRpart$new()),
    trafo_pipeop = PipeOpNOP$new()), regexp = "PipeOpTargetTrafo")

  # IDs
  tt_id = ppl("targettrafo", graph = PipeOpLearner$new(LearnerRegrRpart$new()), id_prefix = "test")
  expect_equal(tt_id$ids(), c("regr.rpart", "targettrafosimple", "testtargetinverter"))
})
