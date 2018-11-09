#' @import checkmate
#' @import data.table
#' @import mlr3
#' @import paradox
#' @importFrom stats setNames predict
#' @importFrom BBmisc vlapply viapply vcapply vnapply seq_row seq_col
#' @importFrom R6 R6Class
#' @keywords internal
"_PACKAGE"

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
} #nocov end
