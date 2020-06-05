#' @title PipeOpVtreat
#'
#' @usage NULL
#' @name mlr_pipeops_pca
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Extracts principle components from data. Only affects numerical features.
#' See [stats::prcomp()] for details.
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
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features replaced by their principal components.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as the elements of the class [stats::prcomp],
#' with the exception of the `$x` slot. These are in particular:
#' * `sdev` :: `numeric`\cr
#'   The standard deviations of the principal components.
#' * `rotation` :: `matrix`\cr
#'   The matrix of variable loadings.
#' * `center` :: `numeric` | `logical(1)`\cr
#'   The centering used, or `FALSE`.
#' * `scale` :: `numeric` | `logical(1)`\cr
#'   The scaling used, or `FALSE`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `center` :: `logical(1)`\cr
#'   Indicating whether the features should be centered. Default is `FALSE`. See [`prcomp()`][stats::prcomp].
#' * `scale.` :: `logical(1)`\cr
#'   Whether to scale features to unit variance before analysis. Default is `FALSE`, but scaling is advisable. See [`prcomp()`][stats::prcomp].
#' * `rank.` :: `integer(1)`\cr
#'   Maximal number of principal components to be used. Default is `NULL`: use all components. See [`prcomp()`][stats::prcomp].
#'
#' @section Internals:
#' Uses the [`prcomp()`][stats::prcomp] function.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
 library("mlr3")

 set.seed(2020)

 make_data <- function(nrows) {
     d <- data.frame(x = 5*rnorm(nrows))
     d['y'] = sin(d[['x']]) + 0.01*d[['x']] + 0.1*rnorm(n = nrows)
     d[4:10, 'x'] = NA                  # introduce NAs
     d['xc'] = paste0('level_', 5*round(d$y/5, 1))
     d['x2'] = rnorm(n = nrows)
     d[d['xc']=='level_-1', 'xc'] = NA  # introduce a NA level
     return(d)
 }

 d = make_data(500)

 task = TaskRegr$new("vtreat", backend = d, target = "y")
 pop = PipeOpVtreat$new()
 pop$train(list(task))

#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpVtreat = R6Class("PipeOpVtreat",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "vtreat", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamLgl$new("recommended", default = TRUE, tags = c("train", "predict"))
      ))
      ps$values$recommended = TRUE
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "vtreat")
    }
  ),
  private = list(

    .train_task = function(task) {
      treatment = if (task$task_type == "regr") {
        vtreat::NumericOutcomeTreatment
      } else if (task$task_type == "classif") {
        if (length(task$class_names) > 2L) {
          vtreat::MultinomialOutcomeTreatment
        } else {
          vtreat::BinomialOutcomeTreatment
        }
      }
      transform_design = treatment(var_list = task$feature_names, outcome_name = task$target_names)
      vtreat_res = vtreat::fit_prepare(transform_design, dframe = task$data())
      self$state$treatment_plan = vtreat_res$treatments
      d_prepared = setDT(vtreat_res$cross_frame)
      task$cbind(d_prepared)
      if (self$param_set$values$recommended) {
        score_frame = vtreat::get_score_frame(self$state$treatment_plan)
        task$select(score_frame$varName[score_frame$recommended])
      }
      task
    },

    .predict_task = function(task) {
      d_prepared = vtreat::prepare(self$state$treatment_plan, dframe = task$data())
      task$cbind(d_prepared)
      if (self$param_set$values$recommended) {
        score_frame = vtreat::get_score_frame(self$state$treatment_plan)
        task$select(score_frame$varName[score_frame$recommended])
      }
      task
    }
  )
)

mlr_pipeops$add("vtreat", PipeOpVtreat)
