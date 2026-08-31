#' @import data.table
#' @import checkmate
#' @import cli
#' @import mlr3
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom utils tail head
#' @importFrom digest digest
#' @importFrom stats setNames
"_PACKAGE"

register_mlr3 = function() {
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$pipeops$valid_tags = unique(c(x$pipeops$valid_tags,
    c("abstract", "meta", "missings", "feature selection", "imbalanced data",
    "data transform", "target transform", "ensemble", "robustify", "learner", "encode",
     "multiplicity", "debug")))
  x$pipeops$properties = unique(c(x$pipeops$properties, c("validation", "internal_tuning")))
}

register_mlr3filters = function() {
  if ("mlr3filters" %in% loadedNamespaces()) {
    x = utils::getFromNamespace("mlr_filters", ns = "mlr3filters")
    x$add("ensemble", FilterEnsemble, .prototype_args = list(filters = list(R6Class("Filter", public = list(id = "filter", task_types = mlr_reflections$task_types$type, task_properties = character(), param_set = ps(), feature_types = mlr_reflections$task_feature_types, packages = "mlr3pipelines", label = NA_character_, man = NA_character_))$new()))) # nolint
  }
}


paradox_info <- list2env(list(is_old = FALSE), parent = emptyenv())

supply_diabetes = function() {
  if (tsk()$has("diabetes")) return(invisible(NULL))
  tsk()$add("diabetes", function(id = "diabetes") {
    warning(
      paste(
        "The installed version of mlr3 does not provide the 'diabetes' task; using the legacy 'pima' task instead.",
        "Update mlr3 to version 1.8.0 or later to use the synthetic diabetes task.",
        "This compatability alias will be removed in the future."
      ),
      call. = FALSE
    )
    tsk("pima", id = id)
  })
}

.onLoad = function(libname, pkgname) {  # nocov start
  register_mlr3()
  register_mlr3filters()
  setHook(packageEvent("mlr3", "onLoad"), function(...) {
    register_mlr3()
    register_mlr3filters()
  }, action = "append")
  setHook(packageEvent("mlr3filters", "onLoad"), function(...) {
    register_mlr3filters()
  }, action = "append")
  backports::import(pkgname)

  assign("lg", lgr::get_logger("mlr3/mlr3pipelines"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
  supply_boston_housing()
  supply_diabetes()
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
