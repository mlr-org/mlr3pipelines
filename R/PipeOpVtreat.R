#' @title PipeOpVtreat
#'
#' @usage NULL
#' @name mlr_pipeops_vtreat
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' FIXME:
#'
#' @section Construction:
#' ```
#' PipeOpVreat$new(id = "vtreat", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"vtreat"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' FIXME:
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as the following elements:
#' * FIXME:
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' FIXME:
#'
#' @section Internals:
#' FIXME:
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' set.seed(2020)
#'
#' make_data <- function(nrows) {
#'     d <- data.frame(x = 5 * rnorm(nrows))
#'     d["y"] = sin(d[["x"]]) + 0.01 * d[["x"]] + 0.1 * rnorm(nrows)
#'     d[4:10, "x"] = NA  # introduce NAs
#'     d["xc"] = paste0("level_", 5 * round(d$y / 5, 1))
#'     d["x2"] = rnorm(nrows)
#'     d[d["xc"] == "level_-1", "xc"] = NA  # introduce a NA level
#'     return(d)
#' }
#'
#' task = TaskRegr$new("vtreat_regr", backend = make_data(100), target = "y")
#'
#' pop = PipeOpVtreat$new()
#' pop$train(list(task))
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpVtreat = R6Class("PipeOpVtreat",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "vtreat", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamLgl$new("recommended", default = TRUE, tags = c("train", "predict")),
        ParamUty$new("cols_to_copy", default = NULL, tags = "train", custom_check = function(x) checkmate::check_list(x, null.ok = TRUE)),
        ParamUty$new("params", default = NULL, tags = "train", custom_check = function(x) checkmate::check_list(x, null.ok = TRUE)),
        ParamUty$new("imputation_map", default = NULL, tags = "train", custom_check = function(x) checkmate::check_list(x, null.ok = TRUE))
        # FIXME: parallelCluster
      ))
      ps$values = list(recommended = TRUE, cols_to_copy = NULL, params = NULL, imputation_map = NULL)
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "vtreat")
    }
  ),
  private = list(

    .train_task = function(task) {
      task_type = task$task_type

      if (task_type == "regr") {
        treatment = vtreat::NumericOutcomeTreatment
        params = vtreat::regression_parameters(self$param_set$values$params)
      } else if (task_type == "classif") {
        if (length(task$class_names) > 2L) {
          treatment = vtreat::MultinomialOutcomeTreatment
          params = vtreat::multinomial_parameters(self$param_set$values$params)
        } else {
          treatment = vtreat::BinomialOutcomeTreatment
          params = vtreat::classification_parameters(self$param_set$values$params)
        }
      }

      var_list = task$feature_names
      if (length(var_list) == 0) {
        return(task)  # early exit
      }

      if (length(self$param_set$values$cols_to_copy)) {
        checkmate::assert_subset(names(self$param_set$values$cols_to_copy), choices = var_list, empty.ok = TRUE)
      }

      if (length(self$param_set$values$imputation_map)) {
        checkmate::assert_subset(names(self$param_set$values$imputation_map), choices = var_list, empty.ok = TRUE)
      }

      transform_design = mlr3misc::invoke(treatment,
        var_list = var_list,
        outcome_name = task$target_names,
        cols_to_copy = self$param_set$values$cols_to_copy,
        params = params,
        imputation_map = self$param_set$values$imputation_map)

      # the following exception handling is necessary because vtreat sometimes fails with "no usable vars" if the data is already "clean" enough
      vtreat_res = try(
        mlr3misc::invoke(vtreat::fit_prepare,
          vps = transform_design,
          dframe = task$data(),
          weights = task$weights$weight),
        silent = TRUE
      )
      if (class(vtreat_res) == "try-error") {
        self$state$treatment_plan = NULL
        return(task)  # early exit
      }

      self$state$treatment_plan = vtreat_res$treatments

      d_prepared = data.table::setDT(vtreat_res$cross_frame)

      task$cbind(d_prepared)

      if (self$param_set$values$recommended) {
        score_frame = mlr3misc::invoke(vtreat::get_score_frame, vps = self$state$treatment_plan)
        task$select(score_frame$varName[score_frame$recommended])
      }
      task
    },

    .predict_task = function(task) {
      if (is.null(self$state$treatment_plan)) return(task)  # early exit

      # in the following we suppress warnings;
      # because if we predict on the same task that was used for training vtreat will tell us:
      #"possibly called transform() on same data frame as fit(), this can lead to over-fit. To avoid this, please use fit_transform()."
      # but this is fine, because we are in the predict step and all models are trained already
      d_prepared = suppressWarnings(mlr3misc::invoke(vtreat::prepare, treatmentplan = self$state$treatment_plan, dframe = task$data()))

      task$cbind(d_prepared)

      if (self$param_set$values$recommended) {
        score_frame = mlr3misc::invoke(vtreat::get_score_frame, vps = self$state$treatment_plan)
        task$select(score_frame$varName[score_frame$recommended])
      }
      task
    }
  )
)

mlr_pipeops$add("vtreat", PipeOpVtreat)
