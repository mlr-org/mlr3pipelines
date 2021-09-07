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

  # IDs
  tt_id = ppl("targettrafo", graph = PipeOpLearner$new(LearnerRegrRpart$new()), id_prefix = "test")
  expect_equal(tt_id$ids(), c("regr.rpart", "targetmutate", "testtargetinvert"))
})

test_that("More Complex Target Trafo Pipelines", {
 task = tsk("mtcars")
 tt = pipeline_targettrafo((po("select") %>>% ppl("branch", list(lrn("regr.featureless"), lrn("regr.rpart")))))
 expect_equal(tt$input$op.id, "targetmutate")
 expect_equal(tt$output$op.id, "targetinvert")

 tt$param_set$values$targetmutate.trafo = function(x) exp(x)
 tt$param_set$values$targetmutate.inverter = function(x) log(x)

 train_tt1 = tt$train(task)
 expect_null(train_tt1[[1L]])
 predict_tt1 = tt$predict(task)
 expect_equal(unique(predict_tt1[[1L]]$response)[1L], log(mean(exp(task$data(cols = "mpg")[[1L]]))))

 tt$param_set$values$branch.selection = 2L
 train_tt2 = tt$train(task)
 expect_null(train_tt2[[1L]])
 predict_tt2 = tt$predict(task)
})
