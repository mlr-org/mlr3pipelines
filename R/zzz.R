#' @import data.table
#' @import checkmate
#' @import mlr3
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom utils tail head
#' @importFrom digest digest
#' @importFrom withr with_options
#' @importFrom stats setNames
"_PACKAGE"

register_mlr3 = function() {
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$pipeops$valid_tags = unique(c(x$pipeops$valid_tags,
    c("abstract", "meta", "missings", "feature selection", "imbalanced data",
    "data transform", "target transform", "ensemble", "robustify", "learner", "encode",
     "multiplicity")))
  x$pipeops$properties = c("validation", "internal_tuning")
}

paradox_info <- list2env(list(is_old = FALSE), parent = emptyenv())

.onLoad = function(libname, pkgname) {  # nocov start
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
  backports::import(pkgname)

  assign("lg", lgr::get_logger("mlr3/mlr3pipelines"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
  paradox_info$is_old = "set_id" %in% names(ps())
  supply_boston_housing()
}  # nocov end

.onUnload = function(libpath) { # nocov start
   event = packageEvent("mlr3", "onLoad")
   hooks = getHook(event)
   pkgname = vapply(hooks[-1], function(x) environment(x)$pkgname, NA_character_)
   setHook(event, hooks[pkgname != "mlr3pipelines"], action = "replace")
} # nocov end

# static code checks should not complain about commonly used data.table columns
utils::globalVariables(c("src_id", "dst_id", "name", "op.id", "response", "truth"))

leanify_package()
