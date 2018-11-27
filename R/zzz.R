#' @import checkmate
#' @import data.table
#' @import mlr3
#' @import paradox
#' @import mlr3misc
#' @importFrom stats setNames predict
#' @importFrom R6 R6Class
"_PACKAGE"

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
} #nocov end
