context("Typecheck")


test_that("utility function works", {

  expect_equal(get_r6_inheritance("data.table"), NULL)

  expect_equal(get_r6_inheritance("PipeOp"), "PipeOp")

  expect_equal(get_r6_inheritance("PipeOpEncode"), c("PipeOpEncode", "PipeOpTaskPreprocSimple", "PipeOpTaskPreproc", "PipeOp"))

  expect_equal(get_r6_inheritance("LearnerClassifDebug"), c("LearnerClassifDebug", "LearnerClassif", "Learner"))

  reset_class_hierarchy_cache()

  oldcache = as.environment(as.list(class_hierarchy_cache))

  expect_equal(oldcache, class_hierarchy_cache)

  expect_equal(get_class_hierarchy("LearnerClassifDebug"), c("LearnerClassifDebug", "LearnerClassif", "Learner"))

  expect_equal(class_hierarchy_cache[["LearnerClassifDebug"]], c("LearnerClassifDebug", "LearnerClassif", "Learner"))
  expect_equal(class_hierarchy_cache[["LearnerClassif"]], c("LearnerClassif", "Learner"))
  expect_equal(class_hierarchy_cache[["Learner"]], "Learner")

  expect_false(isTRUE(all.equal(oldcache, class_hierarchy_cache)))

  reset_class_hierarchy_cache()

  expect_equal(oldcache, class_hierarchy_cache)
})
