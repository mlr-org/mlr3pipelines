#' @title PipeOpVtreat
#'
#' @usage NULL
#' @name mlr_pipeops_vtreat
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Provides an interface to the [vtreat] package.
#'
#' [`PipeOpVtreat`] naturally works for [classification Tasks][mlr3::TaskClassif] and [regression Tasks][mlr3::TaskRegr].
#' Internallly [`PipeOpVtreat`] follows the fit/prepare interface of [vtreat], i.e., first creating a data treatment transform object via
#' [vtreat::NumericOutcomeTreatment], [vtreat::BinomialOutcomeTreatment], or [vtreat::MultinomialOutcomeTreatment], followed by calling
#' [vtreat::fit_prepare] on the training data and [vtreat::prepare] during prediciton.
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
#' The output is the input [`Task`][mlr3::Task] with all affected features "prepared" by [vtreat].
#' If [vtreat] found "no usable vars", the input [`Task`][mlr3::Task] is returned unaltered.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `treatment_plan` :: object of class `vtreat_pipe_step` | `NULL`\cr
#'   The treatment plan as constructed by [vtreat] based on the training data, i.e., an object of class `treatment_plan`.
#'   If [vtreat] found "no usable vars" and designing the treatment would have failed, this is `list(NULL)`.
#'
#' @section Parameters:
#' FIXME:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `recommended` :: `logical(1)`\cr
#'   Whether only the "recommended" prepared features should be returned, i.e., non constant
#'   variables with a significance value smaller than [vtreat]'s threshold. Initialized to `TRUE`.
#' * `cols_to_copy` :: `list`\cr
#'   List of extra columns to copy. See [vtreat::NumericOutcomeTreatment], [vtreat::BinomialOutcomeTreatment], or
#'   [vtreat::MultinomialOutcomeTreatment] for details.
#' * `imputation_map` :: `list`\cr
#'   List of map from column names to functions of signature f(values: numeric, weights: numeric),
#'   simple missing value imputers as used by [vtreat].
#'
#' @section Internals:
#' Follows [vtreat]'s fit/prepare interface.
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
        # FIXME: tags predict?
        ParamLgl$new("recommended", tags = c("train", "predict")),
        ParamUty$new("cols_to_copy", custom_check = checkmate::check_function, tags = "train"),
        # tags stand for: regression vtreat::regression_parameters() / classification vtreat::classification_parameters() / multinomial vtreat::multinomial_parameters()
        ParamDbl$new("minFraction", lower = 0, upper = 1, default = 0.02, tags = c("train", "regression", "classification", "multinomial")),
        ParamDbl$new("smFactor", lower = 0, upper = Inf, default = 0, tags = c("train", "regression", "classification", "multinomial")),
        ParamInt$new("rareCount", lower = 0L, upper = Inf, default = 0, tags = c("train", "regression", "classification", "multinomial")),
        ParamDbl$new("rareSig", lower = 0, upper = 1, special_vals = list(NULL), tags = c("train", "regression", "classification", "multinomial")),  # default NULL for regression, classification, 1 for multinomial
        ParamDbl$new("collarProb", lower = 0, upper = 1, default = 0, tags = c("train", "regression", "classification", "multinomial")),
        ParamUty$new("codeRestriction", default = NULL, custom_check = function(x) checkmate::check_character(x, any.missing = FALSE, null.ok = TRUE), tags = c("train", "regression", "classification", "multinomial")),
        ParamUty$new("customCoders", default = NULL, custom_check = function(x) checkmate::check_list(x, null.ok = TRUE), tags = c("train", "regression", "classification", "multinomial")),
        ParamUty$new("splitFunction", default = NULL, custom_check = function(x) checkmate::check_list(x, null.ok = TRUE), tags = c("train", "regression", "classification", "multinomial")),
        ParamInt$new("ncross", lower = 2L, upper = Inf, default = 3L, tags = c("train", "regression", "classification", "multinomial")),
        ParamLgl$new("forceSplit", default = FALSE, tags = c("train", "regression", "classification", "multinomial")),
        ParamLgl$new("catScaling", tags = c("train", "regression", "classification", "multinomial")),  # default TRUE for regression, classification, FALSE for multinomial
        ParamLgl$new("verbose", default = FALSE, tags = c("train", "regression", "classification", "multinomial")),
        ParamLgl$new("use_paralell", default = TRUE, tags = c("train", "regression", "classification", "multinomial")),
        ParamUty$new("missingness_imputation", default = NULL, custom_check = function(x) checkmate::check_list(x, null.ok = TRUE), tags = c("train", "regression", "classification", "multinomial")),
        ParamDbl$new("pruneSig", lower = 0, upper = 1, special_vals = list(NULL), default = NULL, tags = c("train", "regression", "classification")),
        ParamLgl$new("scale", default = FALSE, tags = c("train", "regression", "classification", "multinomial")),
        ParamLgl$new("doCollar", default = FALSE, tags = c("train", "regression", "classification", "multinomial")),
        ParamUty$new("varRestriction", default = NULL, custom_check = function(x) checkmate::check_list(x, null.ok = TRUE), tags = c("train", "regression", "classification")),
        ParamUty$new("trackedValues", default = NULL, custom_check = function(x) checkmate::check_list(x, null.ok = TRUE), tags = c("train", "regression", "classification")),
        ParamLgl$new("check_for_duplicate_frames", default = TRUE, tags = c("train", "regression", "classification", "multinomial")),  # default TRUE but initialized to FALSE
        ParamUty$new("y_dependent_treatments", default = "catB", custom_check = function(x) checkmate::check_character(x, any.missing = FALSE), tags = c("train", "multinomial")),
        # NOTE: imputation_map is also in multinomial_parameters(); this is redundant so only include it here
        ParamUty$new("imputation_map", default = NULL, custom_check = function(x) checkmate::check_list(x, null.ok = TRUE), tags = c("train", "predict"))
        # NOTE: parallelCluster missing intentionally and will be set to NULL
      ))
      ps$add_dep("collarProb", on = "doCollar", cond = CondEqual$new(TRUE))
      ps$values = list(recommended = TRUE, cols_to_copy = selector_none(), check_for_duplicate_frames = FALSE)
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "vtreat")
    }
  ),
  private = list(

    .train_task = function(task) {

      var_list = task$feature_names
      if (length(var_list) == 0L) {
        return(task)  # early exit
      }

      task_type = task$task_type

      if (task_type == "regr") {
        treatment = vtreat::NumericOutcomeTreatment
        params = vtreat::regression_parameters(self$param_set$get_values(tags = "regression"))
      } else if (task_type == "classif") {
        if (length(task$class_names) > 2L) {
          treatment = vtreat::MultinomialOutcomeTreatment
          params = vtreat::multinomial_parameters(self$param_set$get_values(tags = "multinomial"))
        } else {
          treatment = vtreat::BinomialOutcomeTreatment
          params = vtreat::classification_parameters(self$param_set$get_values(tags = "classification"))
        }
      }

      # FIXME:
      if (length(self$param_set$values$cols_to_copy)) {
        checkmate::assert_subset(unlist(self$param_set$values$cols_to_copy), choices = var_list, empty.ok = TRUE)
      }
      if (length(self$param_set$values$imputation_map)) {
        checkmate::assert_subset(names(self$param_set$values$imputation_map), choices = var_list, empty.ok = TRUE)
      }

      transform_design = mlr3misc::invoke(treatment,
        var_list = var_list,
        outcome_name = task$target_names,
        cols_to_copy = self$param_set$values$cols_to_copy(task),
        params = params,
        imputation_map = self$param_set$values$imputation_map)

      # the following exception handling is necessary because vtreat sometimes fails with "no usable vars" if the data is already "clean" enough
      vtreat_res = try(
        mlr3misc::invoke(vtreat::fit_prepare,
          vps = transform_design,
          dframe = task$data(),
          weights = task$weights$weight,
          parallelCluster = NULL),
        silent = TRUE
      )

      if (class(vtreat_res) == "try-error") {
        self$state = list(NULL)
        return(task)  # early exit
      }

      self$state$treatment_plan = vtreat_res$treatments

      d_prepared = data.table::setDT(vtreat_res$cross_frame)

      task$cbind(d_prepared)

      feature_subset = self$state$treatment_plan$get_feature_names()  # subset to vtreat features

      if (self$param_set$values$recommended) {
        score_frame = mlr3misc::invoke(vtreat::get_score_frame, vps = self$state$treatment_plan)
        feature_subset[feature_subset %in% score_frame$varName[score_frame$recommended]]  # subset to only recommended
      }

      feature_subset = c(feature_subset, unlist(self$state$treatment_plan$settings$cols_to_copy))  # respect cols_to_copy

      task$select(feature_subset)

      task
    },

    .predict_task = function(task) {
      if (is.null(self$state$treatment_plan) || (length(task$feature_names) == 0L)) return(task)  # early exit

      # in the following we suppress warnings and catch errors
      # because if we predict on the same task that was used for training vtreat will tell us:
      # "possibly called transform() on same data frame as fit(), this can lead to over-fit. To avoid this, please use fit_transform()."
      # but this is fine, because we are in the predict step and all models are trained already
      # also, in rare case vtreat will have dropped all features (if recommended = TRUE) and now will fail with "no usable vars"
      d_prepared = try(
        suppressWarnings(mlr3misc::invoke(vtreat::prepare,
          treatmentplan = self$state$treatment_plan,
          dframe = task$data())),
        silent = TRUE
      )

      if (class(d_prepared) != "try-error") {
        task$cbind(d_prepared)
      }

      feature_subset = self$state$treatment_plan$get_feature_names()  # subset to vtreat features

      if (self$param_set$values$recommended) {
        score_frame = mlr3misc::invoke(vtreat::get_score_frame, vps = self$state$treatment_plan)
        feature_subset[feature_subset %in% score_frame$varName[score_frame$recommended]]  # subset to only recommended
      }

      feature_subset = c(feature_subset, unlist(self$state$treatment_plan$settings$cols_to_copy))  # respect cols_to_copy

      task$select(feature_subset)

      task
    },

    # we need to overload the deep_clone method because state$treatment_plan$settings$state is an environment
    deep_clone = function(name, value) {
      if (name == "state" && "NO_OP" %nin% class(value)) {
        if (!is.null(value$treatment_plan)) {
          state = value
          state$treatment_plan = value$treatment_plan$fresh_copy()
          state$treatment_plan$settings$params = value$treatment_plan$settings$params
          state$treatment_plan$settings$state$score_frame = value$treatment_plan$settings$state$score_frame
          state$treatment_plan$settings$state$transform = value$treatment_plan$settings$state$transform
          state
        } else {
          super$deep_clone(name, value)
        }
      } else super$deep_clone(name, value)
    }
  )
)

mlr_pipeops$add("vtreat", PipeOpVtreat)
