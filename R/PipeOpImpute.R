#' @title PipeOpImpute
#'
#' @usage NULL
#' @format Abstract [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Abstract base class for feature imputation.
#'
#' @section Construction:
#' ```
#' PipeOpImpute$$new(id, param_set = ParamSet$new(), param_vals = list(), whole_task_dependent = FALSE, packages = character(0), task_type = "Task")
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
#' * packages :: `character`\cr
#'   Set of all required packages for the [`PipeOp`]'s `private$.train` and `private$.predict` methods. See `$packages` slot.
#'   Default is `character(0)`.
#' * `task_type` :: `character(1)`\cr
#'   The class of [`Task`][mlr3::Task] that should be accepted as input and will be returned as output. This
#'   should generally be a `character(1)` identifying a type of [`Task`][mlr3::Task], e.g. `"Task"`, `"TaskClassif"` or
#'   `"TaskRegr"` (or another subclass introduced by other packages). Default is `"Task"`.
#'
#' @section Input and Output Channels:
#' [`PipeOpImpute`] has one input channel named `"input"`, taking a [`Task`][mlr3::Task], or a subclass of
#' [`Task`][mlr3::Task] if the `task_type` construction argument is given as such; both during training and prediction.
#'
#' [`PipeOpImpute`] has one output channel named `"output"`, producing a [`Task`][mlr3::Task], or a subclass;
#' the [`Task`][mlr3::Task] type is the same as for input; both during training and prediction.
#'
#' The output [`Task`][mlr3::Task] is the modified input [`Task`][mlr3::Task] with features imputed according to the `private$.impute()` function.
#'
#' @section State:
#' The `$state` is a named `list`; besides members added by inheriting classes, the members are:
#' * `affect_cols` :: `character`\cr
#'   Names of features being selected by the `affect_columns` parameter.
#' * `inference_cols` :: `character`\cr
#'   Names of features being selected by the `context_columns` parameter.
#' * `intasklayout` :: [`data.table`]\cr
#'   Copy of the training [`Task`][mlr3::Task]'s `$feature_types` slot. This is used during prediction to ensure that
#'   the prediction [`Task`][mlr3::Task] has the same features, feature layout, and feature types as during training.
#' * `outtasklayout` :: [`data.table`]\cr
#'   Copy of the trained [`Task`][mlr3::Task]'s `$feature_types` slot. This is used during prediction to ensure that
#'   the [`Task`][mlr3::Task] resulting from the prediction operation has the same features, feature layout, and feature types as after training.
#' * `model` :: named `list`\cr
#'   Model used for imputation. This is a list named by [`Task`][mlr3::Task] features, containing the result of the `private$.train_imputer()` function for each one.
#'
#' @section Parameters:
#' * `affect_columns` :: `function` | [`Selector`] | `NULL` \cr
#'   What columns the [`PipeOpImpute`] should operate on.
#'   The parameter must be a [`Selector`] function, which takes a [`Task`][mlr3::Task] as argument and returns a `character`
#'   of features to use.\cr
#'   See [`Selector`] for example functions. Defaults to `NULL`, which selects all features.
#' * `context_columns` :: `function` | [`Selector`] | `NULL` \cr
#'   What columns the [`PipeOpImpute`] imputation may depend on. This parameter is only present if the constructor is called with
#'   the `whole_task_dependent` argument set to `TRUE`.\cr
#'   The parameter must be a [`Selector`] function, which takes a [`Task`][mlr3::Task] as argument and returns a `character`
#'   of features to use.\cr
#'   See [`Selector`] for example functions. Defaults to `NULL`, which selects all features.
#'
#' @section Internals:
#' [`PipeOpImpute`] is an abstract class inheriting from [`PipeOp`] that makes implementing imputer [`PipeOp`]s simple.
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
#' * `.train_imputer(feature, type, context)`\cr
#'   (`atomic`, `character(1)`, [`data.table`]) -> `any`\cr
#'   Called once for each feature selected by `affect_columns` to create the model entry to be used for `private$.impute()`.
#' * `.impute(feature, type, model, context)`\cr
#'   (`atomic`, `character(1)`, `any`, [`data.table`]) -> `atomic`\cr
#'   Imputes the features. `model` is the model created by `private$.train_imputer()`
#'
#' @family PipeOps
#' @family Imputation PipeOps
#' @include PipeOp.R
#' @export
PipeOpImpute = R6Class("PipeOpImpute",
  inherit = PipeOp,
  public = list(
    whole_task_dependent = NULL,

    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), whole_task_dependent = FALSE, packages = character(0), task_type = "Task") {
      # add one or two parameters: affect_columns (always) and context_columns (if whole_task_dependent is TRUE)
      addparams = list(ParamUty$new("affect_columns", custom_check = check_function_or_null, tags = "train"))
      if (whole_task_dependent) {
        addparams = c(addparams, list(ParamUty$new("context_columns", custom_check = check_function_or_null, tags = "train")))
      }

      # ParamSetCollection handles adding of new parameters differently
      if (inherits(param_set, "ParamSet")) {
        lapply(addparams, param_set$add)
      } else {
        private$.affectcols_ps = ParamSet$new(addparams)
        param_set = c(param_set, alist(private$.affectcols_ps))
      }

      self$whole_task_dependent = whole_task_dependent

      super$initialize(id = id, param_set = param_set, param_vals = param_vals,
        input = data.table(name = "input", train = task_type, predict = task_type),
        output = data.table(name = "output", train = task_type, predict = task_type),
        packages = packages, tags = "missings"
      )
    }

  ),
  private = list(

    .affectcols_ps = NULL,

    .train = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)

      affected_cols = (self$param_set$values$affect_columns %??% selector_all())(intask)
      affected_cols = intersect(affected_cols, private$.select_cols(intask))

      self$state = list(
        affected_cols = affected_cols,
        intasklayout = copy(intask$feature_types)
      )

      if (self$whole_task_dependent) {
        context_cols = (self$param_set$values$context_columns %??% selector_all())(intask)
        context_data = intask$data(cols = context_cols)
        self$state$context_cols = context_cols
      }

      ..col = NULL  # avoid static checker complaints

      imputanda = intask$data(cols = affected_cols)
      imputanda = imputanda[, map_lgl(imputanda, function(x) any(is.na(x))), with = FALSE]

      self$state$model = imap(intask$data(cols = affected_cols), function(col, colname) {
        type = intask$feature_types[colname, get("type")]
        if (self$whole_task_dependent) {
          context = copy(context_data)
          context[[colname]] = NULL
        } else {
          context = NULL
        }
        model = private$.train_imputer(col, type, context)
        if (colname %in% names(imputanda)) {
          col = private$.impute(col, type, model, context)
          imputanda[, (colname) := ..col]
        }
        model
      })

      intask$select(setdiff(intask$feature_names, colnames(imputanda)))$cbind(imputanda)

      self$state$outtasklayout = copy(intask$feature_types)

      list(intask)
    },

    .predict = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      if (!isTRUE(all.equal(self$state$intasklayout, intask$feature_types, ignore.row.order = TRUE))) {
        stopf("Input task during prediction of %s does not match input task during training.", self$id)
      }

      if (self$whole_task_dependent) {
        context_data = intask$data(cols = self$state$context_cols)
      }

      ..col = NULL  # avoid static checker complaints

      imputanda = intask$data(cols = self$state$affected_cols)
      imputanda = imputanda[, map_lgl(imputanda, function(x) any(is.na(x))), with = FALSE]

      imap(imputanda, function(col, colname) {
        type = intask$feature_types[colname, get("type")]
        if (self$whole_task_dependent) {
          context = copy(context_data)
          context[[colname]] = NULL
        } else {
          context = NULL
        }
        model = self$state$model[[colname]]
        col = private$.impute(col, type, model, context)
        imputanda[, (colname) := ..col]
      })

      intask$select(setdiff(intask$feature_names, colnames(imputanda)))$cbind(imputanda)

      if (!isTRUE(all.equal(self$state$outtasklayout, intask$feature_types, ignore.row.order = TRUE))) {
        stopf("Processed output task during prediction of %s does not match output task during training.", self$id)
      }
      list(intask)
    },

    .select_cols = function(task) task$feature_names,

    .train_imputer = function(feature, type, context) stop("Abstract."),

    .impute = function(feature, type, model, context) stop("Abstract.")

  )
)
