context("Typecheck")


test_that("utility function works", {

  expect_equal(get_r6_inheritance("data.table"), NULL)

  expect_equal(get_r6_inheritance("PipeOp"), "PipeOp")

  expect_equal(get_r6_inheritance("PipeOpEncode"), c("PipeOpEncode", "PipeOpTaskPreprocSimple", "PipeOpTaskPreproc", "PipeOp"))

  expect_equal(get_r6_inheritance("LearnerClassifDebug"), c("LearnerClassifDebug", "LearnerClassif", "Learner"))

})
