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


.onLoad = function(libname, pkgname) { # nocov start
  backports::import(pkgname)
} # nocov end
