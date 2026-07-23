context("PipeOpDateFeatures")

test_that("PipeOpDateFeatures - basic properties", {
  dat = iris
  set.seed(1)
  dat$datetime = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"), size = 150L)
  dat$date = seq(as.Date("2020-01-31"), length.out = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new()
  expect_datapreproc_pipeop_class(PipeOpDateFeatures, task = task)
})

test_that("PipeOpDateFeatures - finds POSIXct column", {
  dat = iris
  set.seed(1)
  dat$datetime = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"), size = 150L)
  dat$date = seq(as.Date("2020-01-31"), length.out = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new()
  train_pipeop(po, inputs = list(task))
})

test_that("PipeOpDateFeatures - unaltered if no POSIXct column", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"), size = 150L)
  task = TaskClassif$new("iris_date", backend = iris, target = "Species")
  po = PipeOpDateFeatures$new()
  train_pipeop(po, inputs = list(task))
  expect_identical(po$state$intasklayout, po$state$outtasklayout)
})

test_that("PipeOpDateFeatures - unaltered if no features specified", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"), size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(
    param_vals = list(
      cyclic = TRUE,
      year = FALSE,
      quarter = FALSE,
      month = FALSE,
      week_of_year = FALSE,
      day_of_year = FALSE,
      day_of_month = FALSE,
      day_of_week = FALSE,
      hour = FALSE,
      minute = FALSE,
      second = FALSE,
      is_day = FALSE,
      is_month_start = FALSE,
      is_month_end = FALSE,
      is_quarter_start = FALSE,
      is_quarter_end = FALSE,
      is_year_start = FALSE,
      is_year_end = FALSE,
      is_leap_year = FALSE
    )
  )
  train_pipeop(po, inputs = list(task))
  expect_identical(po$state$intasklayout, po$state$outtasklayout)
})

test_that("PipeOpDateFeatures - correct basic features", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"), size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new()
  trained_data = train_pipeop(po, inputs = list(task))$output$data()
  expect_true(all(trained_data$date.year == year(dat$date)))
  expect_true(all(trained_data$date.month == month(dat$date)))
  expect_true(all(trained_data$date.week_of_year == isoweek(dat$date)))
  expect_true(all(trained_data$date.day_of_year == yday(dat$date)))
  expect_true(all(trained_data$date.day_of_month == mday(dat$date)))
  expect_true(all(trained_data$date.day_of_week == wday(dat$date)))
  expect_true(all(trained_data$date.hour == hour(dat$date)))
  expect_true(all(trained_data$date.minute == minute(dat$date)))
  expect_true(all(trained_data$date.second == second(dat$date)))
  hours = hour(dat$date)
  expect_true(all(trained_data$date.is_day == (6 <= hours & hours <= 20)))
  days_in_month = c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[month(dat$date)]  # 2020 is a leap year
  expect_true(all(trained_data$date.is_month_start == (mday(dat$date) == 1L)))
  expect_true(all(trained_data$date.is_month_end == (mday(dat$date) == days_in_month)))
  expect_true(all(trained_data$date.is_quarter_start == (mday(dat$date) == 1L & month(dat$date) %in% c(1L, 4L, 7L, 10L))))
  expect_true(all(trained_data$date.is_quarter_end == (mday(dat$date) == days_in_month & month(dat$date) %in% c(3L, 6L, 9L, 12L))))
  expect_true(all(trained_data$date.is_year_start == (yday(dat$date) == 1L)))
  expect_true(all(trained_data$date.is_year_end == (month(dat$date) == 12L & mday(dat$date) == 31L)))
  expect_true(all(trained_data$date.is_leap_year))  # all dates lie in 2020
})

test_that("PipeOpDateFeatures - correct cyclic features", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-02-01"), to = as.POSIXct("2020-02-29"), by = "sec"), size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(cyclic = TRUE))
  trained_data = train_pipeop(po, inputs = list(task))$output$data()

  month = month(dat$date) - 1L
  value_scaled_month = 2L * pi * month / 12L
  expect_identical(trained_data$date.month_sin, sin(value_scaled_month))

  week_of_year = isoweek(dat$date) - 1L
  value_scaled_woy = 2L * pi * week_of_year / 52L
  expect_identical(trained_data$date.week_of_year_sin, sin(value_scaled_woy))

  day_of_year = yday(dat$date) - 1L
  value_scaled_doy = 2L * pi * day_of_year / (365L + 1L)
  expect_identical(trained_data$date.day_of_year_sin, sin(value_scaled_doy))

  day_of_month = mday(dat$date) - 1L
  value_scaled_dom = 2L * pi * day_of_month / 29L
  expect_identical(trained_data$date.day_of_month_sin, sin(value_scaled_dom))

  day_of_week = wday(dat$date)
  value_scaled_dow = 2L * pi * day_of_week / 7L
  expect_identical(trained_data$date.day_of_week_sin, sin(value_scaled_dow))

  hour = hour(dat$date)
  value_scaled_hour = 2L * pi * hour / 24L
  expect_identical(trained_data$date.hour_sin, sin(value_scaled_hour))

  minute = minute(dat$date)
  value_scaled_minute = 2L * pi * minute / 60L
  expect_identical(trained_data$date.minute_sin, sin(value_scaled_minute))

  second = second(dat$date)
  value_scaled_second = 2L * pi * second / 60L
  expect_identical(trained_data$date.second_sin, sin(value_scaled_second))
})

test_that("PipeOpDateFeatures - feature selection works", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"), size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(cyclic = TRUE, year = FALSE, quarter = FALSE, second = FALSE))
  expect_identical(train_pipeop(po, inputs = list(task))$output$feature_names,
    c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width",
      paste0("date.",
        c("month", "week_of_year", "day_of_year", "day_of_month", "day_of_week",
          "hour", "minute", "is_day",
          "is_month_start", "is_month_end", "is_quarter_start", "is_quarter_end",
          "is_year_start", "is_year_end", "is_leap_year",
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
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"), size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(keep_date_var = TRUE))
  expect_true("date" %in% train_pipeop(po, inputs = list(task))$output$feature_names)
})

test_that("PipeOpDateFeatures - automatic NA handling", {
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"), size = 150L)
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
  dat$date = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"), size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new()
  po$param_set$values$quarter = FALSE
  po$param_set$values$month = FALSE
  po$param_set$values$week_of_year = FALSE
  po$param_set$values$day_of_year = FALSE
  po$param_set$values$day_of_month = FALSE
  po$param_set$values$day_of_week = FALSE
  po$param_set$values$hour = FALSE
  po$param_set$values$minute = FALSE
  po$param_set$values$second = FALSE
  po$param_set$values$is_day = FALSE
  po$param_set$values$is_month_start = FALSE
  po$param_set$values$is_month_end = FALSE
  po$param_set$values$is_quarter_start = FALSE
  po$param_set$values$is_quarter_end = FALSE
  po$param_set$values$is_year_start = FALSE
  po$param_set$values$is_year_end = FALSE
  po$param_set$values$is_leap_year = FALSE
  expect_true("date.year" %in% train_pipeop(po, inputs = list(task))$output$feature_names)
})

test_that("PipeOpDateFeatures - two POSIXct variables", {
  dat = iris
  set.seed(1)
  dat$date2 = sample(seq(as.POSIXct("2020-02-29"), to = as.POSIXct("2020-04-01"), by = "sec"), size = 150L)
  dat$date1 = sample(seq(as.POSIXct("2020-01-31"), to = as.POSIXct("2020-03-01"), by = "sec"), size = 150L)
  task = TaskClassif$new("iris_date", backend = dat, target = "Species")
  po = PipeOpDateFeatures$new(param_vals = list(keep_date_var = TRUE, cyclic = TRUE, quarter = FALSE))
  expect_identical(train_pipeop(po, inputs = list(task))$output$feature_names,
    c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "date1", "date2",
      c(paste0(rep(c("date1.", "date2."), each = 17L),
          c("year",  "month", "week_of_year", "day_of_year", "day_of_month", "day_of_week",
            "hour", "minute", "second", "is_day",
            "is_month_start", "is_month_end", "is_quarter_start", "is_quarter_end",
            "is_year_start", "is_year_end", "is_leap_year")),
        paste0(rep(c("date1.", "date2."), each = 16L),
          c("month_sin", "month_cos", "week_of_year_sin", "week_of_year_cos",
            "day_of_year_sin", "day_of_year_cos", "day_of_month_sin", "day_of_month_cos",
            "day_of_week_sin", "day_of_week_cos",
            "hour_sin", "hour_cos", "minute_sin", "minute_cos", "second_sin", "second_cos"))
      )
    )
  )
})

test_that("PipeOpDateFeatures - boundary date features", {
  # handpicked dates covering month/quarter/year boundaries and leap year rules
  dates = c(
    "2020-01-01",  # year, quarter, and month start; leap year
    "2020-02-29",  # month end in leap-year February
    "2021-02-28",  # month end in non-leap-year February
    "2020-03-31",  # quarter and month end, but not year end
    "2020-12-31",  # year, quarter, and month end
    "2000-02-01",  # month start; leap year (divisible by 400)
    "1900-03-15",  # no boundary; not a leap year (divisible by 100, not 400)
    "2021-07-15"   # no boundary
  )
  set.seed(1)
  dat = data.frame(y = rnorm(length(dates)), date = as.Date(dates))
  task = TaskRegr$new("dates", backend = dat, target = "y")
  po = PipeOpDateFeatures$new()
  output = train_pipeop(po, inputs = list(task))$output
  trained_data = output$data()

  expect_identical(trained_data$date.is_month_start, c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(trained_data$date.is_month_end, c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(trained_data$date.is_quarter_start, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(trained_data$date.is_quarter_end, c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(trained_data$date.is_year_start, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(trained_data$date.is_year_end, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(trained_data$date.is_leap_year, c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))

  new_features = paste0("date.", c("is_month_start", "is_month_end", "is_quarter_start", "is_quarter_end",
    "is_year_start", "is_year_end", "is_leap_year"))
  types = output$feature_types
  expect_true(all(types[types$id %in% new_features, ]$type == "logical"))

  # POSIXct columns yield the same boundary features as Date columns
  dat_posixct = data.frame(y = dat$y, date = as.POSIXct(dates))
  task_posixct = TaskRegr$new("dates_posixct", backend = dat_posixct, target = "y")
  po_posixct = PipeOpDateFeatures$new()
  trained_data_posixct = train_pipeop(po_posixct, inputs = list(task_posixct))$output$data()
  for (feature in new_features) {
    expect_identical(trained_data_posixct[[feature]], trained_data[[feature]])
  }
})
