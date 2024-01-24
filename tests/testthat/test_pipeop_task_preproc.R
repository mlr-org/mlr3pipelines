context("PipeOpTaskPreproc")

test_that("PipeOpTaskPreproc - basic properties", {
  expect_pipeop_class(PipeOpTaskPreproc, list(id = "potask"))

  po = PipeOpTaskPreproc$new("potask")

  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)
})

test_that("PipeOpTaskPreprocSimple - basic properties", {
  expect_pipeop_class(PipeOpTaskPreprocSimple, list(id = "posimple"))

  po = PipeOpTaskPreprocSimple$new(id = "posimple")
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)
})

test_that("Wrong affect_columns errors", {
  POPP = R6Class("POPP",
    inherit = PipeOpTaskPreproc,
    public = list(
      train_dt = function(dt, levels, target) dt,
      predict_dt = function(dt, levels) dt
    )
  )
  tsk = tsk("boston_housing_classic")
  po = POPP$new("foo", param_vals = list(affect_columns = is.factor))
  expect_pipeop(po)
  expect_error(po$train(list(tsk)), "affected_cols")

  po = POPP$new("foo", param_vals = list(affect_columns = function(x) x$target_names))
  expect_error(po$train(list(tsk)), "affected_cols")
})

test_that("test roles are preprocessed", {
  LearnerClassifTestRoles = R6Class("LearnerClassifTestRoles",
    inherit = mlr3::LearnerClassifRpart,
    public = list(
      task = NULL
    ),
    private = list(
      .train = function(task) {
        self$task = task
        super$.train(task)
      }
    )
  )

  graph = po("pca") %>>% LearnerClassifTestRoles$new()

  task = tsk("iris")
  split = partition(task)
  split$test = c(split$test, split$train[1L])
  task$row_roles$use = split$train
  task$row_roles$test = split$test

  graph$train(task)
  outtask = graph$pipeops[[2L]]$learner$task

  expect_equal(length(split$test), length(outtask$row_roles$test))
  expect_equal(length(split$train), length(outtask$row_roles$use))
  expect_equal(sum(lengths(outtask$row_roles)), 151)
})
