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

.onLoad = function(libname, pkgname) {  # nocov start

  mlr_reflections$task_col_roles$regr = union(mlr_reflections$task_col_roles$regr, "unused")
  mlr_reflections$task_col_roles$classif = union(mlr_reflections$task_col_roles$classif, "unused")

  # FIXME: remove nonsense graph default if mlr-org/mlr3#328 can be fixed
  mlr_learners$add("graph", GraphLearner, graph = "classif.featureless")

  backports::import(pkgname)

}  # nocov end
