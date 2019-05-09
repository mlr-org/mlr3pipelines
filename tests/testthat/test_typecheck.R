context("Typecheck")


test_that("utility function works", {
  expect_equal(get_r6_inheritance("data.table"), NULL)

  expect_equal(get_r6_inheritance("PipeOp"), "PipeOp")

  expect_equal(get_r6_inheritance("PipeOpEncode"), c("PipeOpEncode", "PipeOpTaskPreprocSimple", "PipeOpTaskPreproc", "PipeOp"))

  expect_equal(get_r6_inheritance("LearnerClassifDebug"), c("LearnerClassifDebug", "LearnerClassif", "Learner"))

  expect_equal(get_class_hierarchy("data.table"), c("data.table", "data.frame"))

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

  expect_true(are_types_compatible("PipeOp", "PipeOp"))
  expect_true(are_types_compatible("LearnerClassifDebug", "LearnerClassifRpart"))
  expect_false(are_types_compatible("PipeOpEncode", "LearnerClassifDebug"))

  expect_true(are_types_compatible("PipeOp", "*"))
  expect_true(are_types_compatible("*", "PipeOp"))

  expect_true(are_types_compatible("data.table", "*"))
  expect_true(are_types_compatible("*", "data.table"))


  expect_true(are_types_compatible("ewjoifj", "*"))
  expect_true(are_types_compatible("*", "jewpoijfj"))


  expect_true(are_types_compatible("data.table", "data.frame"))

  expect_true(are_types_compatible("ajfpoiewj", "ajfpoiewj"))

  expect_false(are_types_compatible("ajfpoiewj", "sjpoawj"))
})

test_that("Graph is type-checking", {
  expect_error(PipeOpScale$new() %>>% PipeOpModelAvg$new(1),
    "Output type of PipeOp scale during training \\(Task\\) incompatible with input type of PipeOp modelavg \\(NULL\\)")

  mavtest = PipeOpModelAvg$new(1)
  mavtest$input$train = "Task"

  expect_error(PipeOpScale$new() %>>% mavtest,
    "Output type of PipeOp scale during prediction \\(Task\\) incompatible with input type of PipeOp modelavg \\(Prediction\\)")


  gr = Graph$new()$
    add_pipeop(PipeOpScale$new())$
    add_pipeop(PipeOpModelAvg$new(1))

  expect_error(gr$add_edge("scale", "modelavg"),
    "Output type of PipeOp scale during training \\(Task\\) incompatible with input type of PipeOp modelavg \\(NULL\\)")

  gr = Graph$new()$
    add_pipeop(PipeOpScale$new())$
    add_pipeop(mavtest)

  expect_error(gr$add_edge("scale", "modelavg"),
    "Output type of PipeOp scale during prediction \\(Task\\) incompatible with input type of PipeOp modelavg \\(Prediction\\)")
})
