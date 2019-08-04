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
  expect_true(are_types_compatible("LearnerClassif", "LearnerClassifRpart"))
  expect_false(are_types_compatible("LearnerClassifDebug", "LearnerClassifRpart"))
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
    "Output type of PipeOp scale during prediction \\(Task\\) incompatible with input type of PipeOp modelavg \\(PredictionRegr\\)")


  gr = Graph$new()$
    add_pipeop(PipeOpScale$new())$
    add_pipeop(PipeOpModelAvg$new(1))

  expect_error(gr$add_edge("scale", "modelavg"),
    "Output type of PipeOp scale during training \\(Task\\) incompatible with input type of PipeOp modelavg \\(NULL\\)")

  gr = Graph$new()$
    add_pipeop(PipeOpScale$new())$
    add_pipeop(mavtest)

  expect_error(gr$add_edge("scale", "modelavg"),
    "Output type of PipeOp scale during prediction \\(Task\\) incompatible with input type of PipeOp modelavg \\(PredictionRegr\\)")
})

test_that("Autoconversion utility functions work", {

  reset_class_hierarchy_cache()
  reset_autoconvert_register()

  expect_set_equal(map_chr(default_acr, 1), names(autoconvert_register))

  afun = function(x) "abc123"
  bfun = function(x) "xyz987"

  register_autoconvert_function("test", afun)

  expect_identical(get_autoconverter("test")$fun, afun)

  expect_null(get_autoconverter("test_subclass"))
  expect_null(get_autoconverter("test_superclass"))

  add_class_hierarchy_cache(c("test_megaclass", "test_hyperclass", "test_subclass", "test", "test_superclass"))

  expect_identical(get_autoconverter("test_subclass")$fun, afun)
  expect_identical(get_autoconverter("test_superclass")$fun, afun)
  expect_identical(get_autoconverter("test_hyperclass")$fun, afun)
  expect_identical(get_autoconverter("test_megaclass")$fun, afun)

  register_autoconvert_function("test_hyperclass", bfun)

  expect_identical(get_autoconverter("test_subclass")$fun, bfun)  # does the test_hyperclass conversion, because subclass before superclass
  expect_identical(get_autoconverter("test_superclass")$fun, afun)  # converts to "test", because distance to "test_hyperclass" is larger
  expect_identical(get_autoconverter("test_hyperclass")$fun, bfun) # actually registered to bfun
  expect_identical(get_autoconverter("test_megaclass")$fun, bfun)  # converts to "test_hyperclass" because distance to "test" is larger

  reset_autoconvert_register()  # check that reset actually empties the register to default

  expect_set_equal(map_chr(default_acr, 1), names(autoconvert_register))


})

test_that("Autoconversion for pipeops works", {

  po = PipeOpCopy$new(1)

  po$input$train = "Task"
  po$output$predict = "MeasureClassif"

  expect_equal(po$train(list("iris"))[[1]], mlr_tasks$get("iris"))

  expect_equal(po$predict(list("classif.fn"))[[1]], mlr_measures$get("classif.fn"))

  expect_error(po$predict(list("regr.mse")), "inherit from.*MeasureClassif.*but has.*MeasureRegr")

  expect_error(po$predict(list("regrmse")), "PipeOp copy.*Conversion from given data to MeasureClassif.*regrmse.*not found.*regr\\.mse")

})
