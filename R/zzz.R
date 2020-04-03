#' @import data.table
#' @import checkmate
#' @import mlr3
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom utils tail
#' @importFrom digest digest
#' @importFrom withr with_options
"_PACKAGE"

register_mlr3 = function() {
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$pipeops$valid_tags = unique(c(x$pipeops$valid_tags,
    c("abstract", "meta", "missings", "feature selection", "imbalanced data",
    "data transform", "target transform", "ensemble", "robustify", "learner", "encode")))
}

.onLoad = function(libname, pkgname) {  # nocov start
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
  backports::import(pkgname)
}  # nocov end

.onUnload = function(libpath) { # nocov start
   event = packageEvent("mlr3", "onLoad")
   hooks = getHook(event)
   pkgname = vapply(hooks[-1], function(x) environment(x)$pkgname, NA_character_)
   setHook(event, hooks[pkgname != "mlr3pipelines"], action = "replace")
} # nocov end
