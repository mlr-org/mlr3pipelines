context("preproc")

test_that("preproc - basic functionality", {
  task = tsk("iris")

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
