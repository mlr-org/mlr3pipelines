context("PipeOpDateFeatures")

dat = iris
set.seed(1)
dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"),
  size = 150L)

test_that("PipeOpDateFeatures - basic properties", {
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(date_var = "date"))
  expect_pipeop(po)
  train_pipeop(po, inputs = list(task))
  predict_pipeop(po, inputs = list(task))
  suppressWarnings(expect_datapreproc_pipeop_class(PipeOpDateFeatures, task = task)) # due to no POSIXct column
})

test_that("PipeOpDateFeatures - finds POSIXct column", {
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new()
  train_pipeop(po, inputs = list(task))
  expect_true(po$state$date_var == "date")
})

test_that("PipeOpDateFeatures - unaltered if no POSIXct column", {
  task = TaskClassif$new("iris_date", backend = iris, target = "Species")
  po = PipeOpDateFeatures$new()
  expect_warning(train_pipeop(po, inputs = list(task)))
  expect_identical(po$state$intasklayout, po$state$outtasklayout)
})

test_that("PipeOpDateFeatures - unaltered if no features specified", {
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(date_var = "date", cyclic = TRUE, year = FALSE,
    month = FALSE, week_of_year = FALSE, day_of_year = FALSE, day_of_month = FALSE,
    day_of_week = FALSE, hour = FALSE, minute = FALSE, second = FALSE, is_day = FALSE))
  expect_warning(train_pipeop(po, inputs = list(task)))
  expect_identical(po$state$intasklayout, po$state$outtasklayout)
})

test_that("PipeOpDateFeatures - correct basic features", {
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(date_var = "date"))
  trained_data = train_pipeop(po, inputs = list(task))$output$data()
  expect_true(all(trained_data$year == as.numeric(format(dat$date, "%Y"))))
  expect_true(all(trained_data$month == as.numeric(format(dat$date, "%m"))))
  expect_true(all(trained_data$week_of_year == as.numeric(format(dat$date, "%U"))))
  expect_true(all(trained_data$day_of_year == as.numeric(format(dat$date, "%j"))))
  expect_true(all(trained_data$day_of_month == as.numeric(format(dat$date, "%d"))))
  expect_true(all(trained_data$day_of_week == as.numeric(format(dat$date, "%w"))))
  expect_true(all(trained_data$hour == as.numeric(format(dat$date, "%H"))))
  expect_true(all(trained_data$minute == as.numeric(format(dat$date, "%M"))))
  expect_true(all(trained_data$second == as.numeric(format(dat$date, "%S"))))
  hours = as.numeric(format(dat$date, "%H"))
  expect_true(all(trained_data$is_day == ((6 <= hours) & (hours <= 20))))
})

test_that("PipeOpDateFeatures - correct cyclic features", {
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-02-01"), to = as.POSIXct("2020-02-29"), by = "sec"),
    size = 150L)

  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(date_var = "date", cyclic = TRUE))
  trained_data = train_pipeop(po, inputs = list(task))$output$data()

  month = as.numeric(format(dat$date, "%m")) - 1L
  value_scaled_month = 2L * pi * month / 11L
  expect_identical(trained_data$month_sin, sin(value_scaled_month))

  week_of_year = as.numeric(format(dat$date, "%U")) - 1L
  value_scaled_woy = 2L * pi * week_of_year / 51L
  expect_identical(trained_data$week_of_year_sin, sin(value_scaled_woy))

  day_of_year = as.numeric(format(dat$date, "%j")) - 1L
  value_scaled_doy = 2L * pi * day_of_year / (364L + 1L)
  expect_identical(trained_data$day_of_year_sin, sin(value_scaled_doy))

  day_of_month = as.numeric(format(dat$date, "%d")) - 1L
  value_scaled_dom = 2L * pi * day_of_month / 28L
  expect_identical(trained_data$day_of_month_sin, sin(value_scaled_dom))

  day_of_week = as.numeric(format(dat$date, "%w"))
  value_scaled_dow = 2L * pi * day_of_week / 6L
  expect_identical(trained_data$day_of_week_sin, sin(value_scaled_dow))

  hour = as.numeric(format(dat$date, "%H"))
  value_scaled_hour = 2L * pi * hour / 23L
  expect_identical(trained_data$hour_sin, sin(value_scaled_hour))

  minute = as.numeric(format(dat$date, "%M"))
  value_scaled_minute = 2L * pi * minute / 59L
  expect_identical(trained_data$minute_sin, sin(value_scaled_minute))

  second = as.numeric(format(dat$date, "%S"))
  value_scaled_second = 2L * pi * second / 59L
  expect_identical(trained_data$second_sin, sin(value_scaled_second))
})

test_that("PipeOpDateFeatures - feature selection works", {
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(date_var = "date", cyclic = TRUE, year = FALSE,
    second = FALSE))
  expect_identical(train_pipeop(po, inputs = list(task))$output$feature_names,
    c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width",
      "month", "week_of_year", "day_of_year", "day_of_month", "day_of_week",
      "hour", "minute", "is_day",
      "month_sin", "month_cos", "week_of_year_sin", "week_of_year_cos",
      "day_of_year_sin", "day_of_year_cos", "day_of_month_sin", "day_of_month_cos",
      "day_of_week_sin", "day_of_week_cos",
      "hour_sin", "hour_cos", "minute_sin", "minute_cos"))
})

test_that("PipeOpDateFeatures - keep_date_var works", {
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(date_var = "date", keep_date_var = TRUE))
  trained_task = train_pipeop(po, inputs = list(task))$output
  expect_true("date" %in% trained_task$feature_names)
})

test_that("PipeOpDateFeatures - automatic NA handling", {
  dat$date[1L] = NA
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(date_var = "date"))
  output = train_pipeop(po, inputs = list(task))$output
  expect_true(all(is.na(output$data(rows = 1L, cols = c("year", "month", "week_of_year",
    "day_of_year", "day_of_month", "day_of_week", "hour", "minute", "second", "is_day")))))
})
