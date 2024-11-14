#' @title Housing Data for 506 Census Tracts of Boston
#'
#' @usage NULL
#' @name mlr_tasks_boston_housing
#' @format [`R6Class`][R6::R6Class] object inheriting from [`TaskRegr`][mlr3::TaskRegr].
#'
#' The [`BostonHousing2`][mlbench::BostonHousing2] dataset 
#' containing the corrected data from `r format_bib("freeman_1979")`
#' as provided by the `mlbench` package. See data description there.
#'
NULL

load_boston_housing = function(id = "boston_housing") {
  bh = mlr3misc::load_dataset("BostonHousing2", "mlbench")
  bh$medv = NULL
  bht = as_task_regr(bh, target = "cmedv", id = id, label = "Boston Housing Prices")
  bht$man = "mlr3pipelines::mlr_tasks_boston_housing"
  bht$backend$hash = "mlr3::mlr_tasks_boston_housing"
  bht
  tsk()$add("boston_housing", bht)
}

supply_boston_housing = function() {
  if (tsk()$has("boston_housing")) return(invisible(NULL))
  tsk()$add("boston_housing", load_boston_housing)
}
