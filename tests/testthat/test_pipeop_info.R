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
  inputs = list(tsk("iris"), prediction, prediction_new, NULL, "default_string")
  output = list("lgr::mlr3/mlr3pipelines::info", "cat", "warning", "message", "none")
  for (j in inputs) {
    for (i in seq_along(output)) {
      poinfo = PipeOpInfo$new(id = "info", log_target = output[[i]])
      suppressMessages(suppressWarnings(invisible(capture.output(expect_identical(poinfo$train(list(j))[[1]], j)))))
      suppressMessages(suppressWarnings(invisible(capture.output(expect_identical(poinfo$predict(list(j))[[1]], j)))))
    }
  }
})

test_that("output type depending on log_target is appropriate", {
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
  output = list("lgr::mlr3/mlr3pipelines::info", "cat", "warning", "message", "none")
  expect_func = list(expect_output, expect_output, expect_warning, expect_message, expect_silent)
  for (j in inputs) {
    for (i in seq_along(output)) {
      poinfo = PipeOpInfo$new(id = "info", log_target = output[[i]])
      expect_func[[i]](poinfo$train(list(j)))
      expect_func[[i]](poinfo$predict(list(j)))
    }
  }
  lg$set_threshold(old_threshold)
})

test_that("logger is addressed when log_target is an output logger", {
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
    poinfo$train(list(j))
    logfile_posttrain = appender_buffer$data$msg
    expect_match(logfile_posttrain, "Object passing through PipeOp info - Training", all = FALSE)
    appender_buffer$flush()
    poinfo$predict(list(j))
    logfile_postprediction = appender_buffer$data$msg
    expect_match(logfile_postprediction, "Object passing through PipeOp info - Prediction", all = FALSE)
    appender_buffer$flush()
  }
  logger$remove_appender(1)
})

test_that("pattern - check", {
  # Creation of Prediction Object
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  mtcars_new = subset(mtcars[sample(nrow(mtcars), size = 10), ], select = -mpg)
  prediction = lrn_rpart$predict_newdata(mtcars)
  prediction_new = lrn_rpart$predict_newdata(mtcars_new)
  # Actual Test
  inputs = list(tsk("iris"), prediction, prediction_new, NULL, "default_string")
  output = list("cat", "warning", "message")
  capture_func = list(capture_output, capture_warning, capture_messages)
  regex_list = list("\\$task.*\\$data", "\\$prediction.*\\$score", "\\$prediction", "NULL", "default_string")
  for (j in seq_along(inputs)) {
    for (i in seq_along(output)) {
      poinfo = PipeOpInfo$new(id = "info", log_target = output[[i]])
      console_output_train = tryCatch(capture.output(poinfo$train(list(inputs[[j]]))),
                                      warning = function(w) {as.character(conditionMessage(w))},
                                      message = function(m) {as.character(conditionMessage(m))})
      expect_match(paste0(console_output_train, collapse = ""), regex_list[[j]], all = FALSE)
      console_output = as.character(capture_func[[i]](poinfo$train(list(inputs[[j]]))))
      #as.character() transformiert warning in character der gecheckt werden kann
      #expect_match(console_output, regex_list[[j]], all = FALSE)
    }
  }
})

test_that("malformed log_target handled accordingly", {
  malformed_log_target = list("malformed", "::", "::::::", "log::", "log::log_level", "log::log_level::", "log::log_level::message::", "::log")
  for (i in seq_along(malformed_log_target)) {
    expect_error(PipeOpInfo$new("info", log_target = malformed_log_target[[i]]))
  }
})

test_that("original printer can be overwritten", {
  # Creation of Prediction Object
  logger = lgr::get_logger("debug_logger")
  logger$set_propagate(FALSE)
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  prediction = lrn_rpart$predict_newdata(mtcars)
  # Actual Test
  inputs = list(tsk("iris"), prediction, NULL, "default_string")
  output = list("cat", "warning", "message")
  capture_func = list(capture_output, capture_warnings, capture_messages)
  regex_list = list("azbycxdw", "azbycxdwev", "azbycxdwevfu", "azbycxdwevfugt")
  for (j in seq_along(inputs)) {
    for (i in seq_along(output)) {
      poinfo = PipeOpInfo$new(id = "info", log_target = output[[i]],
      printer = list(Task = function(x) {"azbycxdw"},
                     Prediction = function(x) {"azbycxdwev"},
                     `NULL` = function(x) {"azbycxdwevfu"},
                     default= function(x) {"azbycxdwevfugt"}
                    ))
      console_output_train = tryCatch(capture.output(poinfo$train(list(output[[i]]))),
                                      warning = function(w) {conditionMessage(w)},
                                      message = function(m) {conditionMessage(m)})
      expect_match(console_output_train, regex_list[[j]], all = FALSE)
      console_output_predict = tryCatch({
        capture.output(poinfo$train(list(output[[i]])))
        capture.output(poinfo$predict(list(output[[i]])))
        },
        warning = function(w) conditionMessage(w),
        message = function(m) conditionMessage(m))
      expect_match(console_output_predict, regex_list[[j]], all = FALSE)
    }
  }
})

test_that("collect multiplicity works", {
  poovr = po("ovrsplit")
  OVR = poovr$train(list(tsk("iris")))
  # Actual Test
  output = list("cat", "warning", "message")
  capture_func = list(capture_output, capture_warnings, capture_messages)
  for (i in seq_along(output)) {
    po_cm_false = PipeOpInfo$new(id = "info", collect_multiplicity = FALSE, log_target = output[[i]], printer = list(default = function(x) "abc",  Multiplicity = function(x) "xyz"))
    expect_match(capture_func[[i]](po_cm_false$train(list(OVR))), "abc", all = FALSE)
    po_cm_true = PipeOpInfo$new(id = "info", collect_multiplicity = TRUE, log_target = output[[i]], printer = list(default = function(x) "abc",  Multiplicity = function(x) "xyz"))
    expect_match(capture_func[[i]](po_cm_true$train(OVR)), "xyz", all = FALSE)
    }
})
