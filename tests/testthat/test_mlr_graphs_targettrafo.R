context("ppl - pipeline_targettrafo")

test_that("Target Trafo Pipeline", {
  task = tsk("boston_housing")

  tt = ppl("targettrafo", graph = PipeOpLearner$new(LearnerRegrRpart$new()))
  tt$param_set$values$targetmutate.trafo = function(x) log(x, base = 2)
  tt$param_set$values$targetmutate.inverter = function(x) list(response = 2 ^ x$response)
  expect_graph(tt)
  expect_true(length(tt$pipeops) == 1 + 1 + 1)

  g = Graph$new()
  g$add_pipeop(PipeOpTargetMutate$new(param_vals = list(
    trafo = function(x) log(x, base = 2),
    inverter = function(x) list(response = 2 ^ x$response))
    )
  )
  g$add_pipeop(LearnerRegrRpart$new())
  g$add_pipeop(PipeOpTargetInvert$new())
  g$add_edge(src_id = "targetmutate", dst_id = "targetinvert", src_channel = 1L, dst_channel = 1L)
  g$add_edge(src_id = "targetmutate", dst_id = "regr.rpart", src_channel = 2L, dst_channel = 1L)
  g$add_edge(src_id = "regr.rpart", dst_id = "targetinvert", src_channel = 1L, dst_channel = 2L)

  train_tt = tt$train(task)[[1L]]
  predict_tt = tt$predict(task)[[1L]]
  train_g = g$train(task)[[1L]]
  predict_g = g$predict(task)[[1L]]

  expect_equal(train_tt, train_g)
  expect_equal(predict_tt, predict_g)

  tt_g = ppl("targettrafo", graph = PipeOpPCA$new() %>>% PipeOpLearner$new(LearnerRegrRpart$new()))
  tt_g$param_set$values$targetmutate.trafo = function(x) log(x, base = 2)
  tt_g$param_set$values$targetmutate.inverter = function(x) list(response = 2 ^ x$response)

  expect_graph(tt_g)
  expect_true(length(tt_g$pipeops) == 1 + 1 + 1 + 1)

  train_ttg = tt_g$train(task)
  predict_ttg = tt_g$predict(task)

  # assertions on graph
  expect_error(ppl("targettrafo", graph = PipeOpNOP$new()), regexp = "PipeOpLearner")
  expect_error(ppl("targettrafo", graph = PipeOpLearner$new(LearnerRegrRpart$new()),
    trafo_pipeop = PipeOpNOP$new()), regexp = "PipeOpTargetTrafo")

  # IDs
  tt_id = ppl("targettrafo", graph = PipeOpLearner$new(LearnerRegrRpart$new()), id_prefix = "test")
  expect_equal(tt_id$ids(), c("regr.rpart", "targetmutate", "testtargetinvert"))
})
