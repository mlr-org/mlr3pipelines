#' @title Conditional Target Value Impact Encoding
#'
#' @usage NULL
#' @name mlr_pipeops_encodeimpact
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Encodes columns of type `factor`, and `ordered`.
#'
#' Impact coding for [classification Tasks][mlr3::TaskClassif] converts factor levels of each
#' (factorial) column to the difference between each target level's conditional log-likelihood given
#' this level, and the target level's global log-likelihood.
#'
#' Impact coding for [regression Tasks][mlr3::TaskRegr] converts factor levels of each (factorial)
#' column to the difference between the target's conditional mean given this level, and the target's
#' global mean.
#'
#' During training, the impact coding is done using a cross-method. This means that the training
#' [`Task`][mlr3::Task] is split into several folds via [`ResamplingCV`][mlr3::ResamplingCV] and for
#' each fold, impact coding is performed for each test set based on the respective training set.
#' This is helpful to prevent nested model bias.
#'
#' Treats new levels during prediction like missing values.
#'
#' @section Construction:
#' ```
#' PipeOpEncodeImpact$new(id = "encodeimpact", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"encodeimpact"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would
#'   otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected `factor`, or `ordered` parameters encoded.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `train_task_hash` :: `character(1)`\cr
#'   The hash (unique identifier) for the training [`Task`][mlr3::Task].
#' * `rsmp_cv_instance` :: a `data.table`\cr
#'   If `folds` is larger than one, the resampling instance of the [`ResamplingCV`][mlr3::ResamplingCV] used during training.
#' * `impact_predict` :: a named `list`\cr
#'   A list with an element for each affected feature:\cr
#'   For regression, each element is a single column matrix of impact values for each level of that feature.\cr
#'   For classification, this is a list with an element for each *feature level*, which is a vector
#'   giving the impact of this feature level on each *outcome level*.
#'   This list is used to encode impact of the prediction [`Task`][mlr3::Task].
#' * `impact_cv` :: a `list` of named `lists`\cr
#'   A list of length `folds` with each element holding a list like `impact_predict` above.
#'   These lists are used to encode impact of the training [`Task`][mlr3::Task].
#'
#' @section Parameters:
#' * `smoothing` :: `numeric(1)` \cr
#'   A finite positive value used for smoothing. Mostly relevant for [classification Tasks][mlr3::TaskClassif] if
#'   a factor does not coincide with a target factor level (and would otherwise give an infinite logit value).
#'   Initialized to `1e-4`.
#' * `impute_zero` :: `logical(1)`\cr
#'   If `TRUE`, impute missing values as impact 0; otherwise the respective impact is coded as `NA`. Default is `FALSE`.
#' * `folds` :: `integer(1)`\cr
#'   Number of folds used in the cross-method and passed to [`ResamplingCV`][mlr3::ResamplingCV]. Default is `3`.
#'   If set to `1`, no cross-method will be applied during training, i.e., the whole training
#'   [`Task`][mlr3::Task] is used to encode impact during training.
#'
#' @section Internals:
#' Uses laplace smoothing, mostly to avoid infinite values for [classification Task][mlr3::TaskClassif].
#'
#' @section Methods:
#' Only methods inherited [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#' poe = po("encodeimpact")
#'
#' task = TaskClassif$new("task",
#'   backend = data.table::data.table(
#'     x = factor(c("a", "a", "b", "b", "b")),
#'     y = factor(c("a", "a", "a", "b", "b"))),
#'   target = "y")
#'
#' poe$train(list(task))[[1]]$data()
#'
#' poe$state
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpEncodeImpact = R6Class("PipeOpEncodeImpact",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "encodeimpact", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("smoothing", lower = 0, upper = Inf, tags = c("train", "required"), default = 1e-4),
        ParamLgl$new("impute_zero", tags = c("train", "required"), default = FALSE),
        ParamInt$new("folds", lower = 1L, tags = c("train", "required"), default = 3L)
      ))
      ps$values = list(smoothing = 1e-4, impute_zero = FALSE, folds = 3L)
      super$initialize(id, param_set = ps, param_vals = param_vals, tags = "encode", feature_types = c("factor", "ordered"))
    }
  ),
  private = list(

    .train_task = function(task) {
      dt_columns = private$.select_cols(task)
      cols = dt_columns
      if (!length(cols)) {
        self$state = list(dt_columns = dt_columns)
        return(task)  # early exit
      }
      dt = task$data(cols = cols)
      target = task$truth()

      task_type = task$task_type
      row_ids = task$row_ids
      row_seq = seq_len(task$nrow)
      smoothing = self$param_set$values$smoothing
      impute_zero = self$param_set$values$impute_zero
      folds = self$param_set$values$folds
      folds_seq = seq_len(folds)

      # note that matching the row_ids below is necessary because of the resampling

      # impact encoding for the prediction task
      impact_predict = get_impact(task_type, folds_seq = 1L, train_sets = list(row_seq), dt = dt, target = target, smoothing = smoothing, impute_zero = impute_zero)[[1L]]

      if (folds > 1L) {
        # cross-method
        rcv = ResamplingCV$new()
        rcv$param_set$values$folds = folds
        rcv$instantiate(task)

        train_sets = map(folds_seq, function(fold) match(rcv$train_set(fold), row_ids))
        test_sets = map(folds_seq, .f = function(fold) match(rcv$test_set(fold), row_ids))

        impact_cv = get_impact(task_type, folds_seq = folds_seq, train_sets = train_sets, dt = dt, target = target, smoothing = smoothing, impute_zero = impute_zero)

      } else {
        # no cross-method
        test_sets = list(row_seq)

        impact_cv = list(impact_predict)
      }

      self$state = list(train_task_hash = task$hash, rsmp_cv_instance = if (folds > 1L) rcv$instance else data.table(), impact_predict = impact_predict, impact_cv = impact_cv, dt_columns = dt_columns)

      # cross-method (folds > 1) will encode test_set of fold i using the impact encoding trained on train_set of fold i
      dt = imap(dt, .f = function(curdat, idx) {
        fold_dt = map(folds_seq, .f = function(fold) {
          impact_test = self$state$impact_cv[[fold]]
          test_set = test_sets[[fold]]
          curdat = as.character(curdat[test_set])
          curdat[is.na(curdat)] = ".TEMP.MISSING"
          curdat[curdat %nin% rownames(impact_test[[idx]])] = ".TEMP.MISSING"
          # we only want to "drop" if there are no column names.
          # otherwise we want the naming scheme <original feature name>.<target level>
          impact_test[[idx]][match(curdat, rownames(impact_test[[idx]])), , drop = is.null(colnames(impact_test[[idx]]))]
        })
        switch(task_type,
          classif = do.call(rbind, fold_dt),
          regr = unlist(fold_dt)
        )
      })

      dt = as.data.table(dt)
      dt = dt[match(row_seq, unlist(test_sets)), ]  # row ids have to be reordered because of resampling
      task$select(setdiff(task$feature_names, cols))$cbind(dt)
    },

    .predict_task = function(task) {
      cols = self$state$dt_columns
      if (!length(cols)) {
        return(task)
      }
      dt = task$data(cols = cols)

      # impact encoding for the prediction task always relies on the encoding of the whole training task
      impact = self$state$impact_predict
      dt = imap(dt, function(curdat, idx) {
        curdat = as.character(curdat)
        curdat[is.na(curdat)] = ".TEMP.MISSING"
        curdat[curdat %nin% rownames(impact[[idx]])] = ".TEMP.MISSING"
        # we only want to "drop" if there are no column names.
        # otherwise we want the naming scheme <original feature name>.<target level>
        impact[[idx]][match(curdat, rownames(impact[[idx]])), , drop = is.null(colnames(impact[[idx]]))]
      })

      dt = as.data.table(dt)
      task$select(setdiff(task$feature_names, cols))$cbind(dt)
    }
  )
)

mlr_pipeops$add("encodeimpact", PipeOpEncodeImpact)

get_impact = function(task_type, folds_seq, train_sets, dt, target, smoothing, impute_zero) {
  switch(task_type,
    classif = map(folds_seq, .f = function(fold) {
      target_lvls = levels(target)
      train_set = train_sets[[fold]]
      dt_train = dt[train_set, ]
      target_train = target[train_set]

      map(dt_train, .f = function(col) {
        col_lvls = levels(col)

        do.call(cbind, stats::setNames(map(target_lvls, .f = function(tl) {
          tprop = (sum(target_train == tl) + smoothing) / (length(target_train) + 2 * smoothing)
          tplogit = log(tprop / (1 - tprop))

          map_dbl(c(stats::setNames(col_lvls, nm = col_lvls), c(.TEMP.MISSING = NA)), .f = function(cl) {
            if (!impute_zero && is.na(cl)) return(NA_real_)  # early exit
            condprob = (sum(target_train[is.na(cl) | (col == cl)] == tl, na.rm = TRUE) + smoothing) / (sum(is.na(cl) | (col == cl), na.rm = TRUE) + 2 * smoothing)
            cplogit = log(condprob / (1 - condprob))
            cplogit - tplogit
          })
        }), nm = target_lvls))
      })
    }),
    regr = map(folds_seq, .f = function(fold) {
      train_set = train_sets[[fold]]
      dt_train = dt[train_set, ]
      target_train = target[train_set]

      meanimp = mean(target_train)

      map(dt_train, .f = function(col) {
        col_lvls = levels(col)

        as.matrix(c(stats::setNames(map_dbl(col_lvls, .f = function(lvl) {
          (sum(target_train[col == lvl], na.rm = TRUE) + smoothing * meanimp) / (sum(col == lvl, na.rm = TRUE) + smoothing) - meanimp
        }), nm = col_lvls), if (impute_zero) c(.TEMP.MISSING = 0) else c(.TEMP.MISSING = NA)))
      })
    })
  )
}

LearnerEncodeImpact = R6Class("LearnerEncodeImpact", inherit = Learner)

LearnerEncodeImpactClassif = R6Class("LearnerEncodeImpactClassif", inherit = LearnerEncodeImpact,
  public = list(
    initialize = function(id, param_set = ParamSet$new(), predict_types = "impact", feature_types = character(), properties = character(), data_formats = "data.table", packages = character(), man = NA_character_) {
      super$initialize(id = id, task_type = "classif", param_set = param_set, feature_types = feature_types,
        predict_types = predict_types, properties = properties, data_formats = data_formats, packages = packages, man = man)
    }
  )
)

LearnerEncodeImpactClassifSimple = R6Class("LearnerEncodeImpactClassifSimple", inherit = LearnerEncodeImpactClassif,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamUty$new("affect_columns", custom_check = check_function_or_null, default = selector_all(), tags = "train"),
        ParamDbl$new("smoothing", lower = 0, upper = Inf, tags = c("train", "required")),
        ParamLgl$new("impute_zero", tags = c("train", "required"))
      ))
      ps$values = list(smoothing = 1e-4, impute_zero = FALSE)
      super$initialize(
        id = "encode.impact.classif.simple",
        feature_types = c("factor", "ordered"),
        predict_types = "impact",
        param_set = ps,
        properties = c("twoclass", "multiclass", "missings"),
        man = NA_character_
      )
    }
  ),

  private = list(
    .train = function(task) {
      # FIXME: affect_columns
      dt = task$data(cols = task$feature_names)
      target = task$truth()
      smoothing = self$param_set$values$smoothing
      model = sapply(dt, function(col) {
        sapply(levels(target), function(tl) {
          tprop = (sum(target == tl) + smoothing) / (length(target) + 2 * smoothing)
          tplogit = log(tprop / (1 - tprop))
          map_dbl(c(stats::setNames(levels(col), levels(col)), c(.TEMP.MISSING = NA)),
            function(cl) {
              if (!self$param_set$values$impute_zero && is.na(cl)) return(NA_real_)
              condprob = (sum(target[is.na(cl) | col == cl] == tl, na.rm = TRUE) + smoothing) / (sum(is.na(cl) | col == cl, na.rm = TRUE) + 2 * smoothing)
              cplogit = log(condprob / (1 - condprob))
              cplogit - tplogit
            }
          )
        })
      }, simplify = FALSE)
      set_class(model, "encode.impact.classif.simple_model")
    },

    .predict = function(task) {
      model = self$state$model
      dt = task$data(cols = task$feature_names)
      impact = imap(dt, function(curdat, idx) {
        curdat = as.character(curdat)
        curdat[is.na(curdat)] = ".TEMP.MISSING"
        curdat[curdat %nin% rownames(model[[idx]])] = ".TEMP.MISSING"
        # we only want to "drop" if there are no column names.
        # otherwise we want the naming scheme <original feature name>.<target level>
        model[[idx]][match(curdat, rownames(model[[idx]])), , drop = is.null(colnames(model[[idx]]))]
      })
      list(impact = impact)

    }
  )
)

check_prediction_data.PredictionDataEncodeImpact = function(pdata) {
  browser()
  pdata
}

as_prediction.PredictionDataEncodeImpact = function(x, check = TRUE) {
  invoke(PredictionEncodeImpact$new, check = check, .args = x)
}
