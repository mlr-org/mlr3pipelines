context("PipeOpDateFeatures")

test_that("PipeOpDateFeatures - basic properties", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"),
    size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new()
  expect_pipeop(po)
  train_pipeop(po, inputs = list(task))
  predict_pipeop(po, inputs = list(task))
  expect_datapreproc_pipeop_class(PipeOpDateFeatures, task = task)
})

test_that("PipeOpDateFeatures - finds POSIXct column", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"),
    size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new()
  train_pipeop(po, inputs = list(task))
})

test_that("PipeOpDateFeatures - unaltered if no POSIXct column", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"),
    size = 150L)
  task = TaskClassif$new("iris_date", backend = iris, target = "Species")
  po = PipeOpDateFeatures$new()
  train_pipeop(po, inputs = list(task))
  expect_identical(po$state$intasklayout, po$state$outtasklayout)
})

test_that("PipeOpDateFeatures - unaltered if no features specified", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"),
    size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(cyclic = TRUE, year = FALSE,
    month = FALSE, week_of_year = FALSE, day_of_year = FALSE, day_of_month = FALSE,
    day_of_week = FALSE, hour = FALSE, minute = FALSE, second = FALSE, is_day = FALSE))
  train_pipeop(po, inputs = list(task))
  expect_identical(po$state$intasklayout, po$state$outtasklayout)
})

test_that("PipeOpDateFeatures - correct basic features", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"),
    size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new()
  trained_data = train_pipeop(po, inputs = list(task))$output$data()
  expect_true(all(trained_data$date.year == as.numeric(format(dat$date, "%Y"))))
  expect_true(all(trained_data$date.month == as.numeric(format(dat$date, "%m"))))
  expect_true(all(trained_data$date.week_of_year == as.numeric(format(dat$date, "%U"))))
  expect_true(all(trained_data$date.day_of_year == as.numeric(format(dat$date, "%j"))))
  expect_true(all(trained_data$date.day_of_month == as.numeric(format(dat$date, "%d"))))
  expect_true(all(trained_data$date.day_of_week == as.numeric(format(dat$date, "%w"))))
  expect_true(all(trained_data$date.hour == as.numeric(format(dat$date, "%H"))))
  expect_true(all(trained_data$date.minute == as.numeric(format(dat$date, "%M"))))
  expect_true(all(trained_data$date.second == as.numeric(format(dat$date, "%S"))))
  hours = as.numeric(format(dat$date, "%H"))
  expect_true(all(trained_data$date.is_day == ((6 <= hours) & (hours <= 20))))
})

test_that("PipeOpDateFeatures - correct cyclic features", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-02-01"), to = as.POSIXct("2020-02-29"), by = "sec"),
    size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(cyclic = TRUE))
  trained_data = train_pipeop(po, inputs = list(task))$output$data()

  month = as.numeric(format(dat$date, "%m")) - 1L
  value_scaled_month = 2L * pi * month / 12L
  expect_identical(trained_data$date.month_sin, sin(value_scaled_month))

  week_of_year = as.numeric(format(dat$date, "%U")) - 1L
  value_scaled_woy = 2L * pi * week_of_year / 52L
  expect_identical(trained_data$date.week_of_year_sin, sin(value_scaled_woy))

  day_of_year = as.numeric(format(dat$date, "%j")) - 1L
  value_scaled_doy = 2L * pi * day_of_year / (365L + 1L)
  expect_identical(trained_data$date.day_of_year_sin, sin(value_scaled_doy))

  day_of_month = as.numeric(format(dat$date, "%d")) - 1L
  value_scaled_dom = 2L * pi * day_of_month / 29L
  expect_identical(trained_data$date.day_of_month_sin, sin(value_scaled_dom))

  day_of_week = as.numeric(format(dat$date, "%w"))
  value_scaled_dow = 2L * pi * day_of_week / 7L
  expect_identical(trained_data$date.day_of_week_sin, sin(value_scaled_dow))

  hour = as.numeric(format(dat$date, "%H"))
  value_scaled_hour = 2L * pi * hour / 24L
  expect_identical(trained_data$date.hour_sin, sin(value_scaled_hour))

  minute = as.numeric(format(dat$date, "%M"))
  value_scaled_minute = 2L * pi * minute / 60L
  expect_identical(trained_data$date.minute_sin, sin(value_scaled_minute))

  second = as.numeric(format(dat$date, "%S"))
  value_scaled_second = 2L * pi * second / 60L
  expect_identical(trained_data$date.second_sin, sin(value_scaled_second))
})

test_that("PipeOpDateFeatures - feature selection works", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"),
    size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(cyclic = TRUE, year = FALSE,
    second = FALSE))
  expect_identical(train_pipeop(po, inputs = list(task))$output$feature_names,
    c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width",
      paste0("date.",
        c("month", "week_of_year", "day_of_year", "day_of_month", "day_of_week",
          "hour", "minute", "is_day",
          "month_sin", "month_cos", "week_of_year_sin", "week_of_year_cos",
          "day_of_year_sin", "day_of_year_cos", "day_of_month_sin", "day_of_month_cos",
          "day_of_week_sin", "day_of_week_cos",
          "hour_sin", "hour_cos", "minute_sin", "minute_cos")
      )
    )
  )
})

test_that("PipeOpDateFeatures - keep_date_var works", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"),
    size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(keep_date_var = TRUE))
  expect_true("date" %in% train_pipeop(po, inputs = list(task))$output$feature_names)
})

test_that("PipeOpDateFeatures - automatic NA handling", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"),
    size = 150L)
  dat$date[1L] = NA
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(cyclic = TRUE))
  output = train_pipeop(po, inputs = list(task))$output
  expect_true(all(is.na(output$data(rows = 1L, cols = output$feature_names[- (1L:4L)]))))
})

test_that("PipeOpDateFeatures - constant dates", {
  dat = iris
  dat$date = as.POSIXct("2020-01-31")
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(cyclic = TRUE))
  output = train_pipeop(po, inputs = list(task))$output
  expect_true(all(apply(output$data(cols = output$feature_names[- (1L:4L)]), 2, duplicated)[-1L, ]))
})

test_that("PipeOpDateFeatures - no year but day_of_year and day_of_month", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"),
    size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(cyclic = TRUE, year = FALSE))
  expect_true("date.year" %nin% train_pipeop(po, inputs = list(task))$output$feature_names)
})

test_that("PipeOpDateFeatures - only year and cyclic", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"),
    size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new()
  po$param_set$values$month = TRUE
  po$param_set$values$month = FALSE
  po$param_set$values$week_of_year = FALSE
  po$param_set$values$day_of_year = FALSE
  po$param_set$values$day_of_month = FALSE
  po$param_set$values$day_of_week = FALSE
  po$param_set$values$hour = FALSE
  po$param_set$values$minute = FALSE
  po$param_set$values$second = FALSE
  po$param_set$values$is_day = FALSE
  expect_true("date.year" %in% train_pipeop(po, inputs = list(task))$output$feature_names)  
})

test_that("PipeOpDateFeatures - two POSIXct variables", {
  dat = iris
  set.seed(1)
  dat$date1 = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"),
    size = 150L)
  dat$date2 = sample(seq(as.POSIXct("2020-02-29"), to = as.POSIXct("2020-04-01"), by = "sec"),
    size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(keep_date_var = TRUE, cyclic = TRUE))
  expect_identical(train_pipeop(po, inputs = list(task))$output$feature_names,
    c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "date1", "date2",
      c(paste0(rep(c("date1.", "date2."), each = 10L),
          c("year", "month", "week_of_year", "day_of_year", "day_of_month", "day_of_week",
            "hour", "minute", "second", "is_day")),
        paste0(rep(c("date1.", "date2."), each = 16L),
          c("month_sin", "month_cos", "week_of_year_sin", "week_of_year_cos",
            "day_of_year_sin", "day_of_year_cos", "day_of_month_sin", "day_of_month_cos",
            "day_of_week_sin", "day_of_week_cos",
            "hour_sin", "hour_cos", "minute_sin", "minute_cos", "second_sin", "second_cos"))
      )
    )
  )
})
