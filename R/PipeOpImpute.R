#' @title Imputation Base Class
#'
#' @usage NULL
#' @format Abstract [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`].
#'
#' @description
#' Abstract base class for feature imputation.
#'
#' @section Construction:
#' ```
#' PipeOpImpute$$new(id, param_set = ps(), param_vals = list(), whole_task_dependent = FALSE, empty_level_control = FALSE,
#'   packages = character(0), task_type = "Task")
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. See `$id` slot of [`PipeOp`].
#' * `param_set` :: [`ParamSet`][paradox::ParamSet]\cr
#'   Parameter space description. This should be created by the subclass and given to `super$initialize()`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings given in `param_set`. The
#'   subclass should have its own `param_vals` parameter and pass it on to `super$initialize()`. Default `list()`.
#' * `whole_task_dependent` :: `logical(1)`\cr
#'   Whether the `context_columns` parameter should be added which lets the user limit the columns that are
#'   used for imputation inference. This should generally be `FALSE` if imputation depends only on individual features
#'   (e.g. mode imputation), and `TRUE` if imputation depends on other features as well (e.g. kNN-imputation).
#' * `empty_level_control` :: `logical(1)`\cr
#'   Control how to handle edge cases where `NA`s occur in `factor` or `ordered` features only during prediction but not
#'   during training. Can be one of `"never"`, `"always"`, or `"param"`:
#'   - If set to `"never"`, no empty level is introduced during training, but columns that have missing values only
#'   during prediction will *not* be imputed.
#'   - If set to `"always"`, an unseen level is added to the feature during training and missing values are imputed as
#'   that value during prediction.
#'   - Finally, if set to `"param"`, the hyperparameter `create_empty_level` is added and control over this behavior is
#'   left to the user.
#'
#'   For implementation details, see Internals below. Default is `"never"`.
#' * `packages` :: `character`\cr
#'   Set of all required packages for the [`PipeOp`]'s `private$.train` and `private$.predict` methods. See `$packages` slot.
#'   Default is `character(0)`.
#' * `task_type` :: `character(1)`\cr
#'   The class of [`Task`][mlr3::Task] that should be accepted as input and will be returned as output. This
#'   should generally be a `character(1)` identifying a type of [`Task`][mlr3::Task], e.g. `"Task"`, `"TaskClassif"` or
#'   `"TaskRegr"` (or another subclass introduced by other packages). Default is `"Task"`.
#' * `feature_types` :: `character`\cr
#'   Feature types affected by the `PipeOp`. See `private$.select_cols()` for more information.
#'
#' @section Input and Output Channels:
#' `PipeOpImpute` has one input channel named `"input"`, taking a [`Task`][mlr3::Task], or a subclass of
#' [`Task`][mlr3::Task] if the `task_type` construction argument is given as such; both during training and prediction.
#'
#' `PipeOpImpute` has one output channel named `"output"`, producing a [`Task`][mlr3::Task], or a subclass;
#' the [`Task`][mlr3::Task] type is the same as for input; both during training and prediction.
#'
#' The output [`Task`][mlr3::Task] is the modified input [`Task`][mlr3::Task] with features imputed according to the `private$.impute()` function.
#'
#' @section State:
#' The `$state` is a named `list`; besides members added by inheriting classes, the members are:
#' * `affected_cols` :: `character`\cr
#'   Names of features being selected by the `affect_columns` parameter.
#' * `context_cols` :: `character`\cr
#'   Names of features being selected by the `context_columns` parameter.
#' * `intasklayout` :: [`data.table`][data.table::data.table]\cr
#'   Copy of the training [`Task`][mlr3::Task]'s `$feature_types` slot. This is used during prediction to ensure that
#'   the prediction [`Task`][mlr3::Task] has the same features, feature layout, and feature types as during training.
#' * `outtasklayout` :: [`data.table`][data.table::data.table]\cr
#'   Copy of the trained [`Task`][mlr3::Task]'s `$feature_types` slot. This is used during prediction to ensure that
#'   the [`Task`][mlr3::Task] resulting from the prediction operation has the same features, feature layout, and feature types as after training.
#' * `model` :: named `list`\cr
#'   Model used for imputation. This is a list named by [`Task`][mlr3::Task] features, containing the result of the `private$.train_imputer()` or
#'   `private$.train_nullmodel()` function for each one.
#' * `imputed_train` :: `character`\cr
#'   Names of features that were imputed during training. This is used to ensure that factor levels that were added during training are also added during prediction.
#'   Note that features that are imputed during prediction but not during training will still have inconsistent factor levels.
#'
#' @section Parameters:
#' * `affect_columns` :: `function` | [`Selector`] | `NULL` \cr
#'   What columns the `PipeOpImpute` should operate on.
#'   The parameter must be a [`Selector`] function, which takes a [`Task`][mlr3::Task] as argument and returns a `character`
#'   of features to use.\cr
#'   See [`Selector`] for example functions. Defaults to `NULL`, which selects all features.
#' * `context_columns` :: `function` | [`Selector`] | `NULL` \cr
#'   What columns the `PipeOpImpute` imputation may depend on. This parameter is only present if the constructor is called with
#'   the `whole_task_dependent` argument set to `TRUE`.\cr
#'   The parameter must be a [`Selector`] function, which takes a [`Task`][mlr3::Task] as argument and returns a `character`
#'   of features to use.\cr
#'   See [`Selector`] for example functions. Defaults to `NULL`, which selects all features.
#' * `create_empty_level` :: `logical(1)`\cr
#'   Whether an empty level should always be created for `factor` or `ordered` columns during training. If `FALSE`,
#'   columns that had no `NA`s during training but have `NA`s during prediction will not be imputed. This parameter is
#'   only present if the constructor is called with the `empty_level_control` argument set to `"param"`.
#'   Initialized to `FALSE`.\cr
#'
#' @section Internals:
#' `PipeOpImpute` is an abstract class inheriting from [`PipeOp`] that makes implementing imputer [`PipeOp`]s simple.
#'
#' Internally, the construction argument `empty_level_control` and the hyperparameter `create_empty_level` (should it
#' exist) modify the `private$.create_empty_level` field. Behavior then depends on whether this field is set to `TRUE`
#' or `FALSE` and works by controlling for which cases imputation is performed on `factor` or `ordered` columns. Its
#' setting has no impact on columns of other types.\cr
#' If `private$.create_empty_level` is set to `TRUE`, `private$.impute()` is called for all `factor` or `ordered`
#' columns during training, regardless of whether they have any missing values. For this to lead to the creation of an
#' empty level for columns with no missing values, inheriting `PipeOp`s must implement `private$.train_imputer()` in
#' such a way that it returns the name of the level to be created for the feature types `factor` and `ordered`.\cr
#' If `private$.create_empty_level` is set to `FALSE`, `private$.impute()` is not called during prediction for `factor`
#' or `ordered` columns which were not modified during training. This means that `NA`s will not be imputed for these
#' columns.\cr
#' See [`PipeOpImputeOOR`], for a detailed explanation of why these controls are necessary.
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [`PipeOp`], as well as:
#' * `.select_cols(task)` \cr
#'   ([`Task`][mlr3::Task]) -> `character` \cr
#'   Selects which columns the [`PipeOp`] operates on. In contrast to
#'   the `affect_columns` parameter. `private$.select_cols()` is for the *inheriting class* to determine which columns
#'   the operator should function on, e.g. based on feature type, while `affect_columns` is a way for the *user*
#'   to limit the columns that a [`PipeOpTaskPreproc`] should operate on.
#'   This method can optionally be overloaded when inheriting `PipeOpImpute`;
#'   If this method is not overloaded, it defaults to selecting the columns of type indicated by the `feature_types` construction argument.
#' * `.train_imputer(feature, type, context)`\cr
#'   (`atomic`, `character(1)`, [`data.table`][data.table::data.table]) -> `any`\cr
#'   Abstract function that must be overloaded when inheriting.
#'   Called once for each feature selected by `affect_columns` to create the model entry to be used for `private$.impute()`. This function
#'   is only called for features with at least one non-missing value.
#' * `.train_nullmodel(feature, type, context)`\cr
#'   (`atomic`, `character(1)`, [`data.table`][data.table::data.table]) -> `any`\cr
#'   Like `.train_imputer()`, but only called for each feature that only contains missing values. This is not an abstract function
#'   and, if not overloaded, gives a default response of `0` (`integer`, `numeric`), `c(TRUE, FALSE)` (`logical`), all available levels (`factor`/`ordered`),
#'   or the empty string (`character`).
#' * `.impute(feature, type, model, context)`\cr
#'   (`atomic`, `character(1)`, `any`, [`data.table`][data.table::data.table]) -> `atomic`\cr
#'   Imputes the features. `model` is the model created by `private$.train_imputer()`. Default behaviour is to assume `model` is an atomic vector
#'   from which values are sampled to impute missing values of `feature`. `model` may have an attribute `probabilities` for non-uniform sampling.
#'   If `model` has length zero, `feature` is returned unchanged.
#'
#' @family PipeOps
#' @family Imputation PipeOps
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
PipeOpImpute = R6Class("PipeOpImpute",
  inherit = PipeOp,
  public = list(

    initialize = function(id, param_set = ps(), param_vals = list(), whole_task_dependent = FALSE, empty_level_control = "never",
      packages = character(0), task_type = "Task", feature_types = mlr_reflections$task_feature_types) {
      # Add one or two parameters: affect_columns (always) and context_columns (if whole_task_dependent is TRUE)
      addparams = list(affect_columns = p_uty(custom_check = check_function_or_null, tags = "train"))
      if (whole_task_dependent) {
        addparams = c(addparams, list(context_columns = p_uty(custom_check = check_function_or_null, tags = "train")))
      }
      affectcols_ps = do.call(ps, addparams)

      if (empty_level_control == "always") {
        private$.create_empty_level = TRUE
        emplvls_control_ps = ps()  # by setting ps(), we can avoid conditions after this
      } else if (empty_level_control == "never") {
        private$.create_empty_level = FALSE
        emplvls_control_ps = ps()
      } else if (empty_level_control == "param") {
        private$.create_empty_level = NULL
        # Setting create_empty_level modifies private$.create_empty_field later in train and predict
        emplvls_control_ps = ps(create_empty_level = p_lgl(init = FALSE, tags = c("train", "predict")))
      }

      # ParamSetCollection handles adding of new parameters differently
      if (inherits(param_set, "ParamSet")) {
        param_set = c(param_set, affectcols_ps, emplvls_control_ps)
      } else {
        private$.affectcols_ps = affectcols_ps
        private$.emplvls_control_ps = emplvls_control_ps
        param_set = c(param_set, alist(private$.affectcols_ps), alist(private$.emplvls_control_ps))
      }

      private$.feature_types = assert_subset(feature_types, mlr_reflections$task_feature_types)
      private$.whole_task_dependent = whole_task_dependent

      super$initialize(id = id, param_set = param_set, param_vals = param_vals,
        input = data.table(name = "input", train = task_type, predict = task_type),
        output = data.table(name = "output", train = task_type, predict = task_type),
        packages = packages, tags = "missings"
      )
    }

  ),
  active = list(
    feature_types = function(types) {
      if (!missing(types)) stop("feature_types can not be changed. Use the 'affect_columns' hyperparameter instead!")
      private$.feature_types
    }
  ),
  private = list(
    .feature_types = NULL,
    .whole_task_dependent = NULL,
    .affectcols_ps = NULL,
    .create_empty_level = NULL,
    .emplvls_control_ps = NULL,

    .train = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      pv = self$param_set$get_values(tags = "train")

      # If the hyperparameter exists, then private$.create_empty_level is NULL and will be ignored
      create_empty_level = private$.create_empty_level
      if (!is.null(pv$create_empty_level)) {
        create_empty_level = pv$create_empty_level
      }

      affected_cols = (pv$affect_columns %??% selector_all())(intask)
      affected_cols = intersect(affected_cols, private$.select_cols(intask))

      self$state = list(
        affected_cols = affected_cols,
        intasklayout = copy(intask$feature_types)
      )

      if (private$.whole_task_dependent) {
        context_cols = (pv$context_columns %??% selector_all())(intask)
        context_data = intask$data(cols = context_cols)
        self$state$context_cols = context_cols
      }

      imputanda = intask$data(cols = affected_cols)
      if (create_empty_level) {
        # Also run impute on all factor/ordered columns that don't have any NAs
        imputanda = imputanda[, map_lgl(imputanda, function(x) is.factor(x) || anyMissing(x)), with = FALSE]
      } else {
        imputanda = imputanda[, map_lgl(imputanda, function(x) anyMissing(x)), with = FALSE]
      }

      ..col = NULL  # avoid static checker complaints

      self$state$model = imap(intask$data(cols = affected_cols), function(col, colname) {
        type = intask$feature_types[colname, get("type")]
        if (private$.whole_task_dependent) {
          context = copy(context_data)[, (colname) := NULL]
        } else {
          context = NULL
        }
        if (all(is.na(col))) {
          model = private$.train_nullmodel(col, type, context)
        } else {
          model = private$.train_imputer(col, type, context)
        }
        if (colname %in% names(imputanda)) {
          col = private$.impute(col, type, model, context)
          imputanda[, (colname) := ..col]
        }
        model
      })

      intask$select(setdiff(intask$feature_names, colnames(imputanda)))$cbind(imputanda)

      self$state$outtasklayout = copy(intask$feature_types)

      self$state$imputed_train = names(imputanda)

      if (!is.null(intask$internal_valid_task)) {
        intask$internal_valid_task = private$.predict(list(intask$internal_valid_task))[[1L]]
      }

      list(intask)
    },

    .predict = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      if (!isTRUE(all.equal(self$state$intasklayout, intask$feature_types, ignore.row.order = TRUE))) {
        stopf("Input task during prediction of %s does not match input task during training.", self$id)
      }

      if (private$.whole_task_dependent) {
        context_data = intask$data(cols = self$state$context_cols)
      }

      # If the hyperparameter exists and is set to FALSE, we do not impute factor cols that had no missings during train.
      # If the HP does not exist, then we always call impute, since imputing will either not add a new factor
      # (empty_level_control = "never") or the new factor will have been taken care of (empty_level_control = "always")
      pv = self$param_set$get_values(tags = "predict")
      if (!is.null(pv$create_empty_level)) {
        predict_all_factors = pv$create_empty_level
      } else {
        predict_all_factors = TRUE
      }

      imputanda = intask$data(cols = self$state$affected_cols)
      if (!predict_all_factors) {
        # Don't run impute for factor/ordered columns that were not imputed during training
        imputanda = imputanda[,
          colnames(imputanda) %in% self$state$imputed_train | map_lgl(imputanda, function(x) !is.factor(x) && anyMissing(x)),
        with = FALSE]
      } else {
        imputanda = imputanda[,
          colnames(imputanda) %in% self$state$imputed_train | map_lgl(imputanda, function(x) anyMissing(x)),
        with = FALSE]
      }

      ..col = NULL  # avoid static checker complaints

      imap(imputanda, function(col, colname) {
        type = intask$feature_types[colname, get("type")]
        model = self$state$model[[colname]]
        if (private$.whole_task_dependent) {
          context = copy(context_data)[, (colname) := NULL]
        } else {
          context = NULL
        }
        col = private$.impute(col, type, model, context)
        imputanda[, (colname) := ..col]
      })

      intask$select(setdiff(intask$feature_names, colnames(imputanda)))$cbind(imputanda)

      if (!isTRUE(all.equal(self$state$outtasklayout, intask$feature_types, ignore.row.order = TRUE))) {
        stopf("Processed output task during prediction of %s does not match output task during training.", self$id)
      }
      list(intask)
    },

    .select_cols = function(task) selector_type(private$.feature_types)(task),

    .train_imputer = function(feature, type, context) stop("Abstract."),

    .train_nullmodel = function(feature, type, context) {
      switch(type,
        factor = levels(feature),  # Note that this can be character(0) in case of zero level factors created by FixFactors
        integer = 0L, # see PipeOpImputeMean and PipeOpImputeMedian
        logical = c(TRUE, FALSE),
        numeric = 0, # see PipeOpImputeMean and PipeOpImputeMedian
        ordered = levels(feature),  # see above
        character = ""
      )
    },

    .impute = function(feature, type, model, context) {
      if (type %in% c("factor", "ordered")) {
        # in some edge cases there may be levels during training that are missing during predict.
        # Adds level to columns with no NAs if create_empty_level = TRUE
        levels(feature) = c(levels(feature), as.character(model))
      }
      nas = which(is.na(feature))
      if (!length(nas)) return(feature)

      if (!length(model)) {
        return(feature)
      } else if (length(model) == 1) {
        feature[nas] = model
      } else {
        outlen = count_missing(feature)
        feature[nas] = sample(model, outlen, replace = TRUE, prob = attr(model, "probabilities"))
      }
      feature
    }

  )
)
