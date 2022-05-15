lg$set_threshold(old_threshold)

x = mlr3::mlr_reflections
x$task_types = data.table::setkeyv(x$task_types[package != "DUMMY"], "type")
