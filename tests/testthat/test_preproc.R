context("preproc")

test_that("preproc - basic functionality", {
  task = tsk("iris")
  op = PipeOpDebugBasic$new()
  state = list(input = task)

  # Untrained PipeOp, state NULL, predict FALSE -> train
  train_out = expect_no_error(preproc(task, op, predict = FALSE))
  expect_equal(train_out, task)
  op$state = NULL  # Reset PipeOp
  # Untrained PipeOp, state NULL, predict TRUE -> error: Can't predict untrained PipeOp
  expect_error(preproc(task, op, predict = TRUE), "")
  # Untrained PipeOp, state given, predict FALSE -> error: contradictory input
  preproc(task, op, state = state, predict = FALSE)
  # Untrained PipeOp, state given, predict TRUE
  preproc(task, op, state = state, predict = TRUE) # -> predict

  # Trained PipeOp
  preproc(task, op, predict = FALSE) # -> retrain
  preproc(task, op, predict = TRUE) # -> predict
  preproc(task, op, state = state, predict = FALSE) # -> retrain, igrnoring state or error
  preproc(task, op, state = state, predict = TRUE) # -> predict using passed state or error

  # test that:
  # General:
  # - graph / POs get modified in-place
  # With Task:
  # - works with POs / Graphs
  # - train and preproc with train return same thing
  # - predict and preproc with predict return same thing
  # - returns task
  # With DF:
  # - works with POs / Graphs
  # - train and preproc with train return same thing
  # - predict and preproc with predict return same thing
  # - returns data.frame

  # what po to use?
  # what task to use?
})

test_that("preproc - error messages", {
  # indata: Task or data.table-like object
  # graph: only PipeOp or Graph (Filter or Learner not supported)
  # predict: only logical
  # state: list (but checks are mostly done in graph)
  # graph has more than one output
  # some error if graph cannot handle targetless task
  # predict=FALSE while state is given
  # state is given but graph is already trained (overwriting by assignment)
  # state is not given but graph is trained (overwirting by re-training.)

})
