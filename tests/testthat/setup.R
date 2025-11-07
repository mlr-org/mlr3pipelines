lg = lgr::get_logger("mlr3")
old_threshold = lg$threshold
lg$set_threshold("warn")


options(warnPartialMatchArgs = TRUE)
options(warnPartialMatchAttr = TRUE)
options(warnPartialMatchDollar = TRUE)
options(mlr3.warn_deprecated = FALSE)  # avoid triggers when expect_identical() accesses deprecated fields


# simulate packages that extend existing task type
x = mlr3::mlr_reflections
x$task_types = data.table::setkeyv(rbind(x$task_types, x$task_types["regr", mult = "first"][, `:=`(package = "DUMMY", task = "DUMMY")]), "type")

x$task_types = data.table::setkeyv(rbind(x$task_types, x$task_types["classif", mult = "first"][, `:=`(package = "DUMMY", task = "DUMMY")]), "type")

mlr3::mlr_tasks$add("boston_housing_classic", function(id = "boston_housing_classic") {
  b = mlr3::as_data_backend(mlr3misc::load_dataset("BostonHousing2", "mlbench"))
  task = mlr3::TaskRegr$new(id, b, target = "medv", label = "Boston Housing Prices (target leakage, for mlr3pipelines tests only)")
  b$hash = "mlr3pipelines::mlr_tasks_boston_housing_classic"
  task
})


data.table::setDTthreads(threads = 1)
