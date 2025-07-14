pipeop = PipeOpInfo$new(id = "info", printer = list(Task = function(x) {x$nrow}))
pipeop$train(list(tsk("iris")))


context("PipeOpInfo")

test_that("basic properties", {
  po = PipeOpInfo$new("info")
  expect_pipeop(po)
  expect_pipeop_class(PipeOpInfo, list(id = "info"))
})

test_that("check whether input and output are equal", {
  # Creation of Prediction Object
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  mtcars_new = subset(mtcars[sample(nrow(mtcars), size = 10), ], select = -mpg)
  prediction = lrn_rpart$predict_newdata(mtcars)
  prediction_new = lrn_rpart$predict_newdata(mtcars_new)
  # Actual Test
  inputs = c(tsk("iris"), prediction, prediction_new, NULL, "default_string")
  output = c("lgr::mlr3/mlr3pipelines::info", "cat", "warning", "message", "none")
  for (j in inputs) {
    for (i in seq_along(output)) {
      poinfo = PipeOpInfo$new(id = "info", log_target = output[i])
      suppressMessages(suppressWarnings(invisible(capture.output(expect_identical(poinfo$train(list(j))[[1]], j)))))
      suppressMessages(suppressWarnings(invisible(capture.output(expect_identical(poinfo$predict(list(j))[[1]], j)))))
    }
  }
})

test_that("output behavior is appropriate", {
  # Creation of Prediction Object
  lg = lgr::get_logger("mlr3")
  old_threshold = lg$threshold
  lg$set_threshold("info")
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  mtcars_new = subset(mtcars[sample(nrow(mtcars), size = 10), ], select = -mpg)
  prediction = lrn_rpart$predict_newdata(mtcars)
  prediction_new = lrn_rpart$predict_newdata(mtcars_new)
  # Actual Test
  inputs = list(tsk("iris"), prediction, prediction_new, NULL, "default_string")
  output = c("lgr::mlr3/mlr3pipelines::info", "cat", "warning", "message", "none")
  expect_func = list(expect_output, expect_output, expect_warning, expect_message, expect_silent)
  for (j in inputs) {
    for (i in seq_along(output)) {
      poinfo = PipeOpInfo$new(id = "info", log_target = output[i])
      expect_func[[i]](poinfo$train(list(j)))
      expect_func[[i]](poinfo$predict(list(j)))
    }
  }
  lg$set_threshold(old_threshold)
})

# Code Example
#tsk("iris")
#expect_output(list(tsk("iris")))


test_that("logger is addressed", {
  logger = lgr::get_logger("debug_logger")
  logger$set_propagate(FALSE)
  appender_buffer = lgr::AppenderBuffer$new()
  logger$add_appender(appender_buffer, name = "appender")
  # Creation of Prediction Object
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  mtcars_new = subset(mtcars[sample(nrow(mtcars), size = 10), ], select = -mpg)
  prediction = lrn_rpart$predict_newdata(mtcars)
  prediction_new = lrn_rpart$predict_newdata(mtcars_new)
  # Actual Test
  poinfo = PipeOpInfo$new(id = "info", log_target = "lgr::debug_logger::info")
  inputs = list(tsk("iris"), prediction, prediction_new, NULL, "default_string")
  for (j in inputs) {
    logfile_prior = appender_buffer$data
    poinfo$train(list(j))
    logfile_posttrain = appender_buffer$data
    expect_false(identical(logfile_prior, logfile_posttrain)) #checke ob Obh passing thr PipeOp info - Training vorkommt
    poinfo$predict(list(j))
    logfile_postprediction = appender_buffer$data
    expect_false(identical(logfile_posttrain, logfile_postprediction))
    appender_buffer$flush()
  }
  logger$remove_appender(1)
})
# propGte = FALSE in Line 6


# Code Examples
logger = lgr::get_logger("mlr3/mlr3pipelines")
appender_buffer = lgr::AppenderBuffer$new()
logger$add_appender(appender_buffer, name = "appender")
appender_buffer$data

logger$log(level = 400, msg = "HELLO")
appender_buffer$data

appender_buffer$flush()
appender_buffer$data

logger$remove_appender(1)
logger


test_that("pattern - check", {
  # Creation of Prediction Object
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  mtcars_new = subset(mtcars[sample(nrow(mtcars), size = 10), ], select = -mpg)
  prediction = lrn_rpart$predict_newdata(mtcars)
  prediction_new = lrn_rpart$predict_newdata(mtcars_new)
  # Actual Test
  inputs = list(tsk("iris"), prediction, prediction_new)
  output = c("cat", "warning", "message")
  capture_func = list(capture_output, capture_warning, capture_messages)
  regex_list = list("\\$data", "\\$score", "truth.*response")
  for (j in seq_along(inputs)) {
    for (i in seq_along(output)) {
      poinfo = PipeOpInfo$new(id = "info", log_target = output[i])
      console_output = capture_func[[i]](poinfo$train(list(inputs[[j]])))
      expect_match(console_output, regex_list[[j]], all = FALSE)
    }
  }
})

test_that("malformed log_target handled accordingly", {
  malformed_log_target = c("malformed", "::", "::::::", "log::", "log::log_level", "log::log_level::", "log::log_level::message::", "::log")
  for (i in seq_along(malformed_log_target)) {
    expect_error(PipeOpInfo$new("info", log_target = malformed_log_target[i]))
  }
})


# Test der testet dass printer überschrieben werden kann
#   pipeop = PipeOpInfo$new(id = "info", printer = list(Task = function(x) {x$nrow})) --- gucken das zB sowas durchgeht oder sogar simpler print("ddsjsjssikadn")
# collect multiplicity - verhalten checkt cm = TRUE/FALSE ==> default/Task printer wird verwendet; checken ob der dann auch wirklich verwendet wird
