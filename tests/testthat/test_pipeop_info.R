context("PipeOpInfo")

test_that("basic properties", {
  po = po("info")
  expect_pipeop(po)
  expect_pipeop_class(PipeOpInfo, list(id = "info"))
})

test_that("check whether input and output are equal", {
  skip_if_not_installed("rpart")
  # Creation of Prediction Object
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  mtcars_new = subset(mtcars[sample(nrow(mtcars), size = 10), ], select = -mpg)
  prediction = lrn_rpart$predict_newdata(mtcars)
  prediction_new = lrn_rpart$predict_newdata(mtcars_new)
  # Actual Test
  input = list(tsk("iris"), prediction, prediction_new, NULL, "default_string")
  output = list("lgr::mlr3/mlr3pipelines::info", "cat", "warning", "message", "none")
  for (j in input) {
    for (i in seq_along(output)) {
      poinfo = po("info", log_target = output[[i]])
      suppressMessages(suppressWarnings(invisible(utils::capture.output(expect_identical(poinfo$train(list(j))[[1]], j)))))
      suppressMessages(suppressWarnings(invisible(utils::capture.output(expect_identical(poinfo$predict(list(j))[[1]], j)))))
    }
  }
})

test_that("console output type depending on log_target is correct", {
  skip_if_not_installed("rpart")
  # Creation of Prediction Object
  lg = lgr::get_logger("mlr3")
  old_threshold = lg$threshold
  lg$set_threshold("info")
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  mtcars_new = subset(mtcars[sample(nrow(mtcars), size = 10), ], select = -mpg)
  prediction = lrn_rpart$predict_newdata(mtcars)
  prediction_new = lrn_rpart$predict_newdata(mtcars_new)
  # Actual Test
  input = list(tsk("iris"), prediction, prediction_new, NULL, "default_string")
  output = list("lgr::mlr3/mlr3pipelines::info", "cat", "warning", "message", "none")
  expect_func = list(expect_output, expect_output, expect_warning, expect_message, expect_silent)
  for (j in input) {
    for (i in seq_along(output)) {
      poinfo = po("info", log_target = output[[i]])
      expect_func[[i]](poinfo$train(list(j)))
      expect_func[[i]](poinfo$predict(list(j)))
    }
  }
  lg$set_threshold(old_threshold)
})

test_that("logger is addressed when log_target is set to a logger", {
  skip_if_not_installed("rpart")
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
  poinfo = po("info", log_target = "lgr::debug_logger::info")
  input = list(tsk("iris"), prediction, prediction_new, NULL, "default_string")
  for (j in input) {
    poinfo$train(list(j))
    logfile_posttrain = appender_buffer$data$msg
    expect_equal(length(logfile_posttrain), 1L)
    expect_match(logfile_posttrain, "Object passing through PipeOp info - Training", all = FALSE)
    appender_buffer$flush()
    poinfo$predict(list(j))
    logfile_postprediction = appender_buffer$data$msg
    expect_equal(length(logfile_postprediction), 1L)
    expect_match(logfile_postprediction, "Object passing through PipeOp info - Prediction", all = FALSE)
    appender_buffer$flush()
  }
  logger$remove_appender(1)
})

test_that("PipeOp recognizes class of input objects and prints information accordingly", {
  skip_if_not_installed("rpart")
  # Creation of Prediction Object
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  mtcars_new = subset(mtcars[sample(nrow(mtcars), size = 10), ], select = -mpg)
  prediction = lrn_rpart$predict_newdata(mtcars)
  prediction_new = lrn_rpart$predict_newdata(mtcars_new)
  # Actual Test
  input = list(tsk("iris"), prediction, prediction_new, NULL, "default_string")
  output = list("cat", "warning", "message")
  regex_list = list("\\$task.*\\$data", "\\$prediction.*\\$score", "\\$prediction", "NULL", "default_string")
  for (j in seq_along(input)) {
    for (i in seq_along(output)) {
      poinfo = po("info", log_target = output[[i]])
      console_output_train = tryCatch(utils::capture.output(poinfo$train(list(input[[j]]))),
        warning = function(w) as.character(conditionMessage(w)),
        message = function(m) as.character(conditionMessage(m)))
      expect_match(paste0(console_output_train, collapse = ""), regex_list[[j]], all = FALSE)
      suppressMessages(suppressWarnings(utils::capture.output(poinfo$train(list(input[[j]])))))
      console_output_predict = tryCatch(utils::capture.output(poinfo$predict(list(input[[j]]))),
        warning = function(w) as.character(conditionMessage(w)),
        message = function(m) as.character(conditionMessage(m)))
      expect_match(paste0(console_output_predict, collapse = ""), regex_list[[j]], all = FALSE)
    }
  }
})

test_that("Task printer only displays a preview", {
  task = tsk("iris")
  poinfo = po("info")
  preview = poinfo$printer$Task(task)
  row_preview = head(task$row_ids, 10L)
  col_preview = head(c(task$target_names, task$feature_names), 10L)
  expect_true(is.list(preview))
  expect_identical(names(preview), c("task", "data_preview"))
  expect_identical(preview$task, task)
  expect_equal(preview$data_preview,
    task$data(rows = row_preview, cols = col_preview))
})

test_that("malformed log_target handled accordingly", {
  malformed_log_target = list("malformed", "::", "::::::", "log::", "log::log_level", "log::log_level::", "log::log_level::message::", "::log")
  for (i in seq_along(malformed_log_target)) {
    expect_error(po("info", log_target = malformed_log_target[[i]]))
  }
})

test_that("original printer can be overwritten", {
  skip_if_not_installed("rpart")
  # Creation of Prediction Object
  logger = lgr::get_logger("debug_logger")
  logger$set_propagate(FALSE)
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  prediction = lrn_rpart$predict_newdata(mtcars)
  # Actual Test
  input = list(tsk("iris"), prediction, NULL, "default_string")
  output = list("cat", "warning", "message")
  regex_list = list("azbycxdw", "azbycxdwev", "azbycxdwevfu", "azbycxdwevfugt")
  for (j in seq_along(input)) {
    for (i in seq_along(output)) {
      poinfo = po("info", log_target = output[[i]],
        printer = list(Task = function(x) "azbycxdw",
                       Prediction = function(x) "azbycxdwev",
                       `NULL` = function(x) "azbycxdwevfu",
                       default = function(x) "azbycxdwevfugt"
                      ))
      console_output_train = tryCatch(utils::capture.output(poinfo$train(list(input[[j]]))),
        warning = function(w) conditionMessage(w),
        message = function(m) conditionMessage(m))
      expect_match(console_output_train, regex_list[[j]], all = FALSE)
      console_output_predict = tryCatch({
        utils::capture.output(poinfo$train(list(input[[j]])))
        utils::capture.output(poinfo$predict(list(input[[j]])))
        },
        warning = function(w) conditionMessage(w),
        message = function(m) conditionMessage(m))
      expect_match(console_output_predict, regex_list[[j]], all = FALSE)
    }
  }
})

test_that("handling of multiplicity objects controlled by field collect_multiplicity", {
  poovr = po("ovrsplit")
  OVR = poovr$train(list(tsk("iris")))
  # Actual Test
  output = list("cat", "warning", "message")
  collect_multiplicity = list(TRUE, FALSE)
  test_string = list("xyz", "abc")
  input = list(OVR, list(OVR))
  for (i in seq_along(output)) {
    for (j in seq_along(collect_multiplicity)) {
      poinfo = po("info", collect_multiplicity = collect_multiplicity[[j]], log_target = output[[i]], printer = list(default = function(x) "abc",  Multiplicity = function(x) "xyz"))
      console_output_train = tryCatch(utils::capture.output(poinfo$train(input[[j]])),
        warning = function(w) conditionMessage(w),
        message = function(m) conditionMessage(m))
      expect_match(paste0(console_output_train, collapse = ""), test_string[[j]], all = FALSE)
      suppressMessages(suppressWarnings(utils::capture.output(poinfo$train(input[[j]]))))
      console_output_predict = tryCatch(utils::capture.output(poinfo$predict(input[[j]])),
        warning = function(w) conditionMessage(w),
        message = function(m) conditionMessage(m))
      expect_match(paste0(console_output_predict, collapse = ""), test_string[[j]], all = FALSE)
    }
  }
})
