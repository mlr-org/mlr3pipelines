context("PipeOpInfo")

test_that("basic properties", {
  po = PipeOpInfo$new("info")
  expect_pipeop(po) # redundant?
  expect_pipeop_class(PipeOpInfo, list(id = "info"))
})


test_that("output behavior is appropriate", {
  # Creation of Prediction Object
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  mtcars_new = subset(mtcars[sample(nrow(mtcars), size = 10), ], select = -mpg)
  prediction = lrn_rpart$predict_newdata(mtcars)
  prediction_new = lrn_rpart$predict_newdata(mtcars_new)
  # Actual Test
  inputs = c(tsk("iris"), prediction, prediction_new, NULL, "default_string")
  output = c("cat", "warning", "message", "none")
  expect_func = list(expect_output, expect_warning, expect_message, expect_silent)
  for (j in inputs) {
    for (i in seq_len(length(output))) {
      poinfo = PipeOpInfo$new(id = "info", log_target = output[i])
      expect_func[[i]](poinfo$train(list(j)))
      expect_func[[i]](poinfo$predict(list(j)))
    }
  }
})


#test_that("logger is addressed", {
#  browser()
#  logger = lgr::get_logger("mlr3/mlr3pipelines")
#  logger_file = (tempfile(fileext = ".info", tmpdir = fs::path_temp()))
#  logger$add_appender(lgr::AppenderFile$new(logger_file), name = "test_logger")

  # Creation of Prediction Object
#  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
#  mtcars_new = subset(mtcars[sample(nrow(mtcars), size = 10), ], select = -mpg)
#  prediction = lrn_rpart$predict_newdata(mtcars)
#  prediction_new = lrn_rpart$predict_newdata(mtcars_new)
  # Actual Test
#  poinfo = PipeOpInfo$new(id = "info")
#  inputs = c(tsk("iris"), prediction, prediction_new, NULL, "default_string")
#  for (j in inputs) {
#      poinfo = PipeOpInfo$new(id = "info")
#      poinfo$train(list(j))
#      poinfo$predict(list(j))
#    }
#  logger$remove_appender("logfile")
#})


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
    for (i in seq_len(length(output))) {
      poinfo = PipeOpInfo$new(id = "info", log_target = output[i])
      suppressMessages(suppressWarnings(invisible(capture.output(expect_identical(poinfo$train(list(j))[[1]], j)))))
      suppressMessages(suppressWarnings(invisible(capture.output(expect_identical(poinfo$predict(list(j))[[1]], j)))))
    }
  }
})

test_that("malformed log_target handled accordingly", {
  malformed_log_target = c("malformed", "::", "::::::", "log::", "log::log_level", "log::log_level::", "log::log_level::message::", "::log")
  lapply(malformed_log_target, function(x) expect_error(PipeOpInfo$new("info", log_target = x)))
})
