lg = lgr::get_logger("mlr3")
old_threshold = lg$threshold
lg$set_threshold("warn")


options(warnPartialMatchArgs = TRUE)
options(warnPartialMatchAttr = TRUE)
options(warnPartialMatchDollar = TRUE)
