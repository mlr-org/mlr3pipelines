context("preproc")

test_that("preproc - basic sanity checks", {
  op = po("scale")
  task = tsk("iris")

  # Test indata
  expect_no_error(preproc(task, op))
  op$state = NULL
  expect_no_error(preproc(task$data(), op))
  op$state = NULL
  expect_error(preproc(NULL, op))

  # Test processor
  expect_no_error(preproc(task, as_graph(op)))
  expect_error(preproc(task, lrn("regr.rpart")), "no applicable method")

  # Test errors for processors incapable of handling targetless tasks


  # Test state
  # state: list (but checks are mostly done in graph)

  # wrong states are caught
  # states are constructed correctly for PipeOps
  op = po("nop")
  expect_no_error(preproc(task, op, state = list()))
  op$state = NULL  # reset state
  expect_no_error(preproc(task, as_graph(op), state = named_list(op$id, list())))

  # Test max one output channel
  expect_error(preproc(task, gunion(list(NULL, NULL))), "must have exactly one output channel")

})

test_that("preproc - PipeOp processor", {
  task = tsk("iris")
  df = task$data(cols = task$feature_names)
  processor = PipeOpScale$new()
  # Extract state to pass to preproc in some of the tests
  processor$train(list(task))
  state = processor$state
  processor$state = NULL  # Reset PipeOp's state

  # Untrained PipeOp, state NULL, predict FALSE -> train
  train_out = expect_no_error(preproc(task, processor, predict = FALSE))
  expect_equal(train_out, processor$train(list(task))[[1L]])
  expect_true(processor$is_trained)  # sufficient to test modification-in-place?
  processor$state = NULL  # Reset PipeOp's state
  # Untrained PipeOp, state NULL, predict TRUE -> error: Can't predict untrained PipeOp
  expect_error(preproc(task, processor, predict = TRUE), "Cannot predict.*not been trained yet")
  # Untrained PipeOp, state given, predict FALSE -> error: contradictory input
  expect_error(preproc(task, processor, state = state, predict = FALSE), "Inconsistent function arguments")
  # Untrained PipeOp, state given, predict TRUE -> predict
  predict_out = expect_no_error(preproc(task, processor, state = state, predict = TRUE))
  expect_equal(predict_out, processor$predict(list(task))[[1L]])

  # Trained PipeOp, state NULL, predict FALSE -> re-train + warning
  expect_warning({train_out = preproc(task, processor, predict = FALSE)}, "preproc re-trains")
  expect_equal(train_out, processor$train(list(task))[[1L]])
  expect_true(processor$is_trained)
  # Trained PipeOp, state NULL, predict TRUE -> predict
  predict_out = expect_no_error(preproc(task, processor, predict = TRUE))
  expect_equal(predict_out, processor$predict(list(task))[[1L]])
  # Trained PipeOp, state given, predict FALSE -> error: contradictory input
  expect_error(preproc(task, processor, state = state, predict = FALSE), "Inconsistent function arguments")
  # Trained PipeOp, state given, predict TRUE -> predict + warning: using passed state
  expect_warning({predict_out = preproc(task, processor, state = state, predict = TRUE)}, "")
  expect_equal(predict_out, processor$predict(list(task))[[1L]])

  # df
  # same tests as above, but add $data(), if they should return data.frames as well

})

test_that("preproc - Graph processor", {
  task = tsk("iris")
  df = task$data(cols = task$feature_names)
  op = po("scale")
  processor = as_graph(op)
  # Extract state to pass to preproc in some of the tests
  processor$train(task)
  state = named_list(processor$ids(), processor$state)
  processor$state = NULL  # Reset Graph's state

  # Untrained PipeOp, state NULL, predict FALSE -> train
  train_out = expect_no_error(preproc(task, processor, predict = FALSE))
  expect_equal(train_out, processor$train(task)[[1L]])
  expect_true(processor$is_trained)  # sufficient to test modification-in-place?
  processor$state = NULL  # Reset PipeOp
  # Untrained PipeOp, state NULL, predict TRUE -> error: Can't predict untrained PipeOp
  expect_error(preproc(task, processor, predict = TRUE), "Cannot predict.*not been trained yet")
  # Untrained PipeOp, state given, predict FALSE -> error: contradictory input
  expect_error(preproc(task, processor, state = state, predict = FALSE), "Inconsistent function arguments")
  # Untrained PipeOp, state given, predict TRUE -> predict
  predict_out = expect_no_error(preproc(task, processor, state = state, predict = TRUE))
  expect_equal(predict_out, processor$predict(task)[[1L]])

  # Trained PipeOp, state NULL, predict FALSE -> re-train + warning
  train_out = expect_warning(preproc(task, processor, predict = FALSE), "preproc re-trains")
  expect_equal(train_out, processor$train(task)[[1L]])
  expect_true(processor$is_trained)
  # Trained PipeOp, state NULL, predict TRUE -> predict
  predict_out = expect_no_error(preproc(task, processor, predict = TRUE))
  expect_equal(predict_out, processor$predict(list(task))[[1L]])
  # Trained PipeOp, state given, predict FALSE -> re-train + warning
  train_out = expect_warning(preproc(task, processor, state = state, predict = FALSE), "")
  expect_equal(train_out, processor$train(task)[[1L]])
  expect_true(processor$is_trained)
  # Trained PipeOp, state given, predict TRUE -> predict + warning: using passed state
  predict_out = expect_warning(preproc(task, processor, state = state, predict = TRUE), "")
  expect_equal(predict_out, processor$predict(task)[[1L]])


  df = task$data(cols = task$feature_names)
  # same tests as above, but add $data()
})
