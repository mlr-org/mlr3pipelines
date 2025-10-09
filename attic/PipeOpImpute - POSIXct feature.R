library("nycflights13")

flights_dt = as.data.table(flights)
missing_row = copy(flights_dt[1])[, time_hour := as.POSIXct(NA)]
flights_missing = rbind(flights_dt, missing_row, use.names = TRUE, fill = TRUE)

tsk_flights = as_task_regr(flights_missing, target = "dep_delay", id = "flights")


# PipeOpImputeConstant
po = po("imputeconstant", param_vals = list(constant = as.POSIXct("2022-05-23")), affect_columns = selector_name("time_hour"))
po$train(list(tsk_flights))[[1]]$data()

# PipeOpImputeHist - X
# PipeOpImputeMean - X
# PipeOpImputeMedian - V
# PipeOpImputeMode - V
# PipeOpImputeOOR - ?
# PipeOpImputeSample - V
