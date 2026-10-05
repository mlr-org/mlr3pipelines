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

# simulate packages that define Task classes derived from TaskRegr / TaskClassif / TaskSupervised with their own
# task type (e.g. TaskRegrST / TaskClassifST from mlr3spatiotempcv, TaskSurv from mlr3proba),
# see helper_derived_tasks.R
derived_types = x$task_types[c("regr", "classif", "unsupervised"), mult = "first"][,
  `:=`(type = c("regr_derived", "classif_derived", "supervised_derived"), package = "DUMMY",
    task = c("TaskRegrDerived", "TaskClassifDerived", "TaskSupervisedDerived"))
]
x$task_types = data.table::setkeyv(rbind(x$task_types, derived_types), "type")
for (task_type in c("regr_derived", "supervised_derived")) {
  x$task_col_roles[[task_type]] = x$task_col_roles$regr
  x$task_properties[[task_type]] = x$task_properties$regr
}
x$task_col_roles$classif_derived = x$task_col_roles$classif
x$task_properties$classif_derived = x$task_properties$classif

mlr3::mlr_tasks$add("boston_housing_classic", function(id = "boston_housing_classic") {
  b = mlr3::as_data_backend(mlr3misc::load_dataset("BostonHousing2", "mlbench"))
  task = mlr3::TaskRegr$new(id, b, target = "medv", label = "Boston Housing Prices (target leakage, for mlr3pipelines tests only)")
  b$hash = "mlr3pipelines::mlr_tasks_boston_housing_classic"
  task
})


data.table::setDTthreads(threads = 1)
