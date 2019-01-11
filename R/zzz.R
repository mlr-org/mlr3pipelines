#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import mlr3
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom utils tail
"_PACKAGE"

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
} #nocov end
