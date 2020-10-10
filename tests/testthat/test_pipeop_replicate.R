context("PipeOpReplicate")

test_that("PipeOpReplicate - basic properties", {
  po = PipeOpReplicate$new()
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  expect_pipeop_class(PipeOpReplicate)
})

test_that("PipeOpReplicate - train and predict", {
  tsk = mlr_tasks$get("iris")
  nreps = 3
  po = PipeOpReplicate$new(param_vals = list(reps = nreps))

  tout = train_pipeop(po, list(tsk))
  expect_list(po$state, len = 0)
  expect_multiplicity(tout[[1]])
  expect_list(tout[[1]], len = nreps)
  expect_true(all(map_lgl(tout[[1]], .f = function(x) all.equal(x, tsk))))
  pout = predict_pipeop(po, list(tsk))
  expect_multiplicity(pout[[1]])
  expect_list(pout[[1]], len = 3)
  expect_true(all(map_lgl(pout[[1]], .f = function(x) all.equal(x, tsk))))
})
