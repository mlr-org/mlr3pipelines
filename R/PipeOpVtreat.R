#' @title Interface to the vtreat Package
#'
#' @usage NULL
#' @name mlr_pipeops_vtreat
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Provides an interface to the vtreat package.
#'
#' `PipeOpVtreat` naturally works for [classification tasks][mlr3::TaskClassif] and [regression tasks][mlr3::TaskRegr].
#' Internally, `PipeOpVtreat` follows the fit/prepare interface of vtreat, i.e., first creating a data treatment transform object via
#' [vtreat::NumericOutcomeTreatment()], [vtreat::BinomialOutcomeTreatment()], or [vtreat::MultinomialOutcomeTreatment()], followed by calling
#' [vtreat::fit_prepare()] on the training data and [vtreat::prepare()] during predicton.
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
#' The output is the input [`Task`][mlr3::Task] with all affected features "prepared" by vtreat.
#' If vtreat found "no usable vars", the input [`Task`][mlr3::Task] is returned unaltered.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `treatment_plan` :: object of class `vtreat_pipe_step` | `NULL`\cr
#'   The treatment plan as constructed by vtreat based on the training data, i.e., an object of class `treatment_plan`.
#'   If vtreat found "no usable vars" and designing the treatment would have failed, this is `NULL`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `recommended` :: `logical(1)`\cr
#'   Whether only the "recommended" prepared features should be returned, i.e., non constant
#'   variables with a significance value smaller than vtreat's threshold. Initialized to `TRUE`.
#' * `cols_to_copy` :: `function` | [`Selector`] \cr
#'   [`Selector`] function, takes a [`Task`][mlr3::Task] as argument and returns a `character()` of features to copy.\cr
#'   See [`Selector`] for example functions. Initialized to `selector_none()`.
#' * `minFraction` :: `numeric(1)`\cr
#'   Minimum frequency a categorical level must have to be converted to an indicator column.
#' * `smFactor` :: `numeric(1)`\cr
#'   Smoothing factor for impact coding models.
#' * `rareCount` :: `integer(1)`\cr
#'   Allow levels with this count or below to be pooled into a shared rare-level.
#' * `rareSig` :: `numeric(1)`\cr
#'   Suppress levels from pooling at this significance value greater.
#' * `collarProb` :: `numeric(1)`\cr
#'   What fraction of the data (pseudo-probability) to collar data at if `doCollar = TRUE`.
#' * `doCollar` :: `logical(1)`\cr
#'   If `TRUE` collar numeric variables by cutting off after a tail-probability specified by `collarProb` during treatment design.
#' * `codeRestriction` :: `character()`\cr
#'   What types of variables to produce.
#' * `customCoders` :: named `list`\cr
#'   Map from code names to custom categorical variable encoding functions.
#' * `splitFunction` :: `function`\cr
#'   Function taking arguments nSplits, nRows, dframe, and y; returning a user desired split.
#' * `ncross` :: `integer(1)`\cr
#'   Integer larger than one, number of cross-validation rounds to design.
#' * `forceSplit` :: `logical(1)`\cr
#'   If `TRUE` force cross-validated significance calculations on all variables.
#' * `catScaling` :: `logical(1)`\cr
#'   If `TRUE` use [stats::glm()] linkspace, if FALSE use [stats::lm()] for scaling.
#' * `verbose` :: `logical(1)`\cr
#'   If `TRUE` print progress.
#' * `use_paralell` :: `logical(1)`\cr
#'   If `TRUE` use parallel methods.
#' * `missingness_imputation` :: `function`\cr
#'   Function of signature f(values: numeric, weights: numeric), simple missing value imputer.\cr
#'   Typically, an imputation via a [`PipeOp`] should be preferred, see [`PipeOpImpute`].
#' * `pruneSig` :: `numeric(1)`\cr
#'   Suppress variables with significance above this level.
#'   Only effects [regression tasks[mlr3::TaskRegr] and binary [classification tasks][mlr3::TaskClassif].
#' * `scale` :: `logical(1)`\cr
#'   If `TRUE` replace numeric variables with single variable model regressions ("move to outcome-scale").
#'   These have mean zero and (for variables with significant less than 1) slope 1 when regressed (lm for regression problems/glm for classification problems) against outcome.
#' * `varRestriction` :: `list()`\cr
#'   List of treated variable names to restrict to.
#'   Only effects [regression tasks[mlr3::TaskRegr] and binary [classification tasks][mlr3::TaskClassif].
#' * `trackedValues` :: named `list()`\cr
#'   Named list mapping variables to know values, allows warnings upon novel level appearances (see [vtreat::track_values()]).
#'   Only effects [regression tasks[mlr3::TaskRegr] and binary [classification tasks][mlr3::TaskClassif].
#' * `y_dependent_treatments` :: `character()`\cr
#'   Character what treatment types to build per-outcome level.
#'   Only effects multiclass [classification tasks][mlr3::TaskClassif].
#' * `imputation_map` :: named `list`\cr
#'   List of map from column names to functions of signature f(values: numeric, weights: numeric), simple missing value imputers.\cr
#'   Typically, an imputation via a [`PipeOp`] is to be preferred, see [`PipeOpImpute`].
#'
#' For more information, see [vtreat::regression_parameters()], [vtreat::classification_parameters()], or [vtreat::multinomial_parameters()].
#'
#' @section Internals:
#' Follows vtreat's fit/prepare interface. See [vtreat::NumericOutcomeTreatment()], [vtreat::BinomialOutcomeTreatment()],
#' [vtreat::MultinomialOutcomeTreatment()], [vtreat::fit_prepare()] and [vtreat::prepare()].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examplesIf requireNamespace("vtreat")
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
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpVtreat = R6Class("PipeOpVtreat",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "vtreat", param_vals = list()) {
      ps = ps(
        recommended = p_lgl(tags = c("train", "predict")),
        cols_to_copy = p_uty(custom_check = checkmate::check_function, tags = c("train", "predict")),
        # tags stand for: regression vtreat::regression_parameters() / classification vtreat::classification_parameters() / multinomial vtreat::multinomial_parameters()
        minFraction = p_dbl(lower = 0, upper = 1, default = 0.02, tags = c("train", "regression", "classification", "multinomial")),
        smFactor = p_dbl(lower = 0, upper = Inf, default = 0, tags = c("train", "regression", "classification", "multinomial")),
        rareCount = p_int(lower = 0L, upper = Inf, default = 0, tags = c("train", "regression", "classification", "multinomial")),
        rareSig = p_dbl(lower = 0, upper = 1, special_vals = list(NULL), tags = c("train", "regression", "classification", "multinomial")),  # default NULL for regression, classification, 1 for multinomial
        collarProb = p_dbl(lower = 0, upper = 1, default = 0, tags = c("train", "regression", "classification", "multinomial"), depends = quote(doCollar == TRUE)),
        doCollar = p_lgl(default = FALSE, tags = c("train", "regression", "classification", "multinomial")),
        codeRestriction = p_uty(
          default = NULL,
          custom_check = crate(function(x) checkmate::check_character(x, any.missing = FALSE, null.ok = TRUE)),
          tags = c("train", "regression", "classification", "multinomial")
        ),
        customCoders = p_uty(
          default = NULL,
          custom_check = crate(function(x) checkmate::check_list(x, null.ok = TRUE)),
          tags = c("train", "regression", "classification", "multinomial")
        ),
        splitFunction = p_uty(
          default = NULL,
          custom_check = crate(function(x) checkmate::check_function(x, args = c("nSplits", "nRows", "dframe", "y"), null.ok = TRUE)),
          tags = c("train", "regression", "classification", "multinomial")
        ),
        ncross = p_int(lower = 2L, upper = Inf, default = 3L, tags = c("train", "regression", "classification", "multinomial")),
        forceSplit = p_lgl(default = FALSE, tags = c("train", "regression", "classification", "multinomial")),
        catScaling = p_lgl(tags = c("train", "regression", "classification", "multinomial")),  # default TRUE for regression, classification, FALSE for multinomial
        verbose = p_lgl(default = FALSE, tags = c("train", "regression", "classification", "multinomial")),
        use_paralell = p_lgl(default = TRUE, tags = c("train", "regression", "classification", "multinomial")),
        missingness_imputation = p_uty(
          default = NULL,
          custom_check = crate(function(x) checkmate::check_function(x, args = c("values", "weights"), null.ok = TRUE)),
          tags = c("train", "regression", "classification", "multinomial")
        ),
        pruneSig = p_dbl(lower = 0, upper = 1, special_vals = list(NULL), default = NULL, tags = c("train", "regression", "classification")),
        scale = p_lgl(default = FALSE, tags = c("train", "regression", "classification", "multinomial")),
        varRestriction = p_uty(
          default = NULL,
          custom_check = crate(function(x) checkmate::check_list(x, null.ok = TRUE)),
          tags = c("train", "regression", "classification")
        ),
        trackedValues = p_uty(
          default = NULL,
          custom_check = crate(function(x) checkmate::check_list(x, null.ok = TRUE)),
          tags = c("train", "regression", "classification")
        ),
        # NOTE: check_for_duplicate_frames not needed
        y_dependent_treatments = p_uty(
          default = "catB",
          custom_check = crate(function(x) checkmate::check_character(x, any.missing = FALSE)),
          tags = c("train", "multinomial")
        ),
        # NOTE: imputation_map is also in multinomial_parameters(); this is redundant so only include it here
        imputation_map = p_uty(
          default = NULL,
          custom_check = crate(function(x) checkmate::check_list(x, null.ok = TRUE)),
          tags = c("train", "predict")
        )
        # NOTE: parallelCluster missing intentionally and will be set to NULL
      )
      ps$set_values(recommended = TRUE, cols_to_copy = selector_none())
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "vtreat", tags = c("encode", "missings"))
    }
  ),
  private = list(

    .train_task = function(task) {

      var_list = task$feature_names
      if (length(var_list) == 0L) {
        return(task)  # early exit
      }

      if (length(self$param_set$values$imputation_map)) {
        checkmate::assert_subset(names(self$param_set$values$imputation_map), choices = var_list, empty.ok = TRUE)
      }

      task_type = task$task_type
      transform_design = if (task_type == "regr") {
        mlr3misc::invoke(vtreat::NumericOutcomeTreatment,
          var_list = var_list,
          outcome_name = task$target_names,
          cols_to_copy = self$param_set$values$cols_to_copy(task),
          params = vtreat::regression_parameters(mlr3misc::insert_named(self$param_set$get_values(tags = "regression"), list(check_for_duplicate_frames = FALSE))),
          imputation_map = self$param_set$values$imputation_map)
      } else if (task_type == "classif") {
        if (length(task$class_names) > 2L) {
          mlr3misc::invoke(vtreat::MultinomialOutcomeTreatment,
            var_list = var_list,
            outcome_name = task$target_names,
            cols_to_copy = self$param_set$values$cols_to_copy(task),
            params = vtreat::multinomial_parameters(mlr3misc::insert_named(self$param_set$get_values(tags = "multinomial"), list(check_for_duplicate_frames = FALSE))),
            imputation_map = self$param_set$values$imputation_map)
        } else {
          mlr3misc::invoke(vtreat::BinomialOutcomeTreatment,
            var_list = var_list,
            outcome_name = task$target_names,
            outcome_target = task$positive,
            cols_to_copy = self$param_set$values$cols_to_copy(task),
            params = vtreat::classification_parameters(mlr3misc::insert_named(self$param_set$get_values(tags = "classification"), list(check_for_duplicate_frames = FALSE))),
            imputation_map = self$param_set$values$imputation_map)
        }
      }

      # the following exception handling is necessary because vtreat sometimes fails with "no usable vars" if the data is already "clean" enough
      vtreat_res = tryCatch(
        mlr3misc::invoke(vtreat::fit_prepare,
          vps = transform_design,
          dframe = task$data(),
          weights = if ("weights_learner" %in% names(task)) task$weights_learner$weight else task$weights$weight,
          parallelCluster = NULL),
        error = function(error_condition) {
          if (grepl("no usable vars", x = error_condition$message)) {
            NULL
          } else {
            stopf(error_condition$message)
          }
        }
      )

      if (is.null(vtreat_res)) {
        self$state = list(NULL)
        return(task)  # early exit
      }

      self$state$treatment_plan = vtreat_res$treatments

      d_prepared = data.table::setDT(vtreat_res$cross_frame)

      feature_subset = self$state$treatment_plan$get_feature_names()  # subset to vtreat features
      if (self$param_set$values$recommended) {
        score_frame = mlr3misc::invoke(vtreat::get_score_frame, vps = self$state$treatment_plan)
        feature_subset = feature_subset[feature_subset %in% score_frame$varName[score_frame$recommended]]  # subset to only recommended
      }
      feature_subset = c(feature_subset, self$param_set$values$cols_to_copy(task))  # respect cols_to_copy

      task$select(cols = character())  # drop all original features
      task$cbind(d_prepared[, feature_subset, with = FALSE])  # cbind the new features

      task
    },

    .predict_task = function(task) {
      if (is.null(self$state$treatment_plan) || (length(task$feature_names) == 0L)) {
        return(task)  # early exit
      }

      # the following exception handling is necessary because vtreat sometimes fails with "no usable vars" if the data is already "clean" enough
      d_prepared = tryCatch(
        data.table::setDT(mlr3misc::invoke(vtreat::prepare,
          treatmentplan = self$state$treatment_plan,
          dframe = task$data())),
        error = function(error_condition) {
          if (grepl("no useable vars", x = error_condition$message)) {
            data.table()
          } else {
            stopf(error_condition$message)
          }
        }
      )

      feature_subset = self$state$treatment_plan$get_feature_names()  # subset to vtreat features
      if (self$param_set$values$recommended) {
        score_frame = mlr3misc::invoke(vtreat::get_score_frame, vps = self$state$treatment_plan)
        feature_subset = feature_subset[feature_subset %in% score_frame$varName[score_frame$recommended]]  # subset to only recommended
      }
      feature_subset = c(feature_subset, self$param_set$values$cols_to_copy(task))  # respect cols_to_copy

      task$select(cols = character())  # drop all original features
      task$cbind(d_prepared[, feature_subset, with = FALSE])  # cbind the new features

      task
    },

    # we need to overload the deep_clone method because state$treatment_plan$settings$state is an environment
    deep_clone = function(name, value) {
      if (name == "state" && "NO_OP" %nin% class(value)) {
        if (!is.null(value$treatment_plan)) {
          # NOTE: not sure if multiplicity_recurse is actually needed
          multiplicity_recurse(value, .fun = function(value) {
            state = value
            state$treatment_plan = value$treatment_plan$fresh_copy()
            state$treatment_plan$settings$params = value$treatment_plan$settings$params
            state$treatment_plan$settings$state$score_frame = value$treatment_plan$settings$state$score_frame
            state$treatment_plan$settings$state$transform = value$treatment_plan$settings$state$transform
            state
          })
        } else {
          super$deep_clone(name, value)
        }
      } else {
        super$deep_clone(name, value)
      }
    }
  )
)

mlr_pipeops$add("vtreat", PipeOpVtreat)
