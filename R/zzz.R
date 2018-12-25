#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import mlr3
#' @import paradox
#' @import mlr3misc
#' @import methods
#' @importFrom stats setNames predict
#' @importFrom R6 R6Class
#' @importFrom graphics plot
"_PACKAGE"

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
} #nocov end
