lg$set_threshold(old_threshold)

x = mlr3::mlr_reflections
x$task_types = data.table::setkeyv(x$task_types[package != "DUMMY"], "type")
for (task_type in c("regr_derived", "classif_derived", "supervised_derived")) {
  x$task_col_roles[[task_type]] = NULL
  x$task_properties[[task_type]] = NULL
}
