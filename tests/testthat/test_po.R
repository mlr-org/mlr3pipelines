context("mlr_pipeops")

test_that("mlr_pipeops access works", {

  expect_equal(
    po("scale"),
    mlr_pipeops$get("scale")
  )

  expect_equal(
    po("scale", center = FALSE),
    mlr_pipeops$get("scale", param_vals = list(center = FALSE))
  )

  expect_equal(
    po("scale", id = "sx", center = FALSE),
    PipeOpScale$new(id = "sx", param_vals = list(center = FALSE))
  )

  expect_equal(
    po("copy", 2),
    mlr_pipeops$get("copy", 2)
  )

  expect_equal(
    po("copy", outnum = 2),
    mlr_pipeops$get("copy", outnum = 2)
  )

  expect_equal(
    po("branch", options = 2),
    mlr_pipeops$get("branch", options = 2)
  )

  expect_equal(
    po("learner", mlr_learners$get("classif.rpart"), xval = 1),
    mlr_pipeops$get("learner", mlr_learners$get("classif.rpart"), param_vals = list(xval = 1))
  )

  expect_equal(
    po("learner", mlr_learners$get("classif.rpart"), xval = 1, param_vals = list(cp = 0.5)),
    mlr_pipeops$get("learner", mlr_learners$get("classif.rpart"), param_vals = list(xval = 1, cp = 0.5))
  )

  expect_equal(
    po("learner", mlr_learners$get("classif.rpart"), xval = 1, param_vals = list(cp = 0.5), id = "blabla"),
    mlr_pipeops$get("learner", mlr_learners$get("classif.rpart"), param_vals = list(xval = 1, cp = 0.5), id = "blabla")
  )

  expect_error(po("learnerx"), "'learner'")

  # check that we can set the 'key' value
  dblrn = R6Class("debuglearn", inherit = LearnerClassif,
    public = list(
      initialize = function() {
        super$initialize(id = "debuglearn", param_set = paradox::ParamSet$new()$add(paradox::ParamDbl$new("key")))
      }
    )
  )

  expect_equal(
    po("learner", dblrn$new(), key = 99),
    mlr_pipeops$get("learner", dblrn$new(), param_vals = list(key = 99))
  )

})
