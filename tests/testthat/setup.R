lg = lgr::get_logger("mlr3")
old_threshold = lg$threshold
lg$set_threshold("warn")


options(warnPartialMatchArgs = TRUE)
options(warnPartialMatchAttr = TRUE)
options(warnPartialMatchDollar = TRUE)


# simulate packages that extend existing task type
x = mlr3::mlr_reflections
x$task_types = data.table::setkeyv(rbind(x$task_types, x$task_types["regr", mult = "first"][, `:=`(package = "DUMMY", task = "DUMMY")]), "type")

x$task_types = data.table::setkeyv(rbind(x$task_types, x$task_types["classif", mult = "first"][, `:=`(package = "DUMMY", task = "DUMMY")]), "type")

