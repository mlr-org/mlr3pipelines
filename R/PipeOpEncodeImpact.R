#' @title Impact Encoding with Random Intercept Models
#'
#' @usage NULL
#' @name mlr_pipeops_encode
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Encodes columns of type `factor`, `character` and `ordered`.
#'
#' PipeOpEncodeLmer() converts factor levels of each factorial column to the
#' estimated coefficients of a simple random intercept model.
#' Models are fitted with the glmer function of the lme4 package and are
#' of the type \code{target ~ 1 + (1 | factor)}. Thus, the binary target
#' variable is used as dependent variable and the factor is used for grouping.
#' For training, multiple models can be estimated in a cross-validation scheme
#' to ensure that the same factor level does not always result in identical
#' values in the converted numerical feature.
#' For prediction, a global model (which was fitted on all observations
#' during training) is used for each factor.
#' New factor levels are converted to the value of the intercept coefficient
#' of the global model for prediction.
#' NAs are ignored by the CPO.
#'
#' Use the [`PipeOpTaskPreproc`] `$affect_columns` functionality to only encode a subset of columns, or only encode columns of a certain type.
#'
#' @section Construction:
#' ```
#' PipeOpEncode$new(id = "encode", param_vals = list())
#' ```
#" * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"encode"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected `factor`, `character` or `ordered` parameters encoded according to the `method`
#' parameter.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `constrasts` :: named `list` of `matrix`\cr
#'   List of contrast matrices, one for each affected discrete feature. The rows of each matrix correspond to (training task) levels, the the
#'   columns to the new columns that replace the old discrete feature. See [`stats::contrasts`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `method`  :: `character(1)` \cr
#'   Initialized to `"one-hot"`. One of:
#'   * `"one-hot"`: create a new column for each factor level.
#'   * `"treatment"`: create $n-1$ columns leaving out the first factor level of each factor variable (see `stats::contr.treatment()`).
#'   * `"helmert"`: create columns according to Helmert contrasts (see `stats::contr.helmert()`).
#'   * `"poly"`: create columns with contrasts based on orthogonal polynomials (see `stats::contr.poly()`).
#'   * `"sum"`: create columns with contrasts summing to zero, (see `stats::contr.sum()`).
#'
#' @section Internals:
#' Uses the [`stats::contrasts`] functions. This is relatively inefficient for features with a large number of levels.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' poe = mlr_pipeops$get("encode")
#'
#' task = mlr3::TaskClassif$new("task",
#'   data.table::data.table(x = letters[1:3], y = letters[1:3]), "x")
#'
#' # poe is initialized with encoding: "one-hot"
#' poe$train(list(task))[[1]]$data()
#'
#' # other kinds of encoding:
#' poe$param_set$values$method = "treatment"
#' poe$train(list(task))[[1]]$data()
#'
#' poe$param_set$values$method = "helmert"
#' poe$train(list(task))[[1]]$data()
#'
#' poe$param_set$values$method = "poly"
#' poe$train(list(task))[[1]]$data()
#'
#' poe$param_set$values$method = "sum"
#' poe$train(list(task))[[1]]$data()
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpEncodeLmer = R6Class("PipeOpEncode",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "encode", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamFct$new("method", levels = c("one-hot", "treatment", "helmert", "poly", "sum"), tags = c("train", "predict"))
      ))
      ps$values = list(method = "one-hot")
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "stats")
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("factor", "ordered", "character"), get("id")]
    },

    get_state_dt = function(dt, levels) {
      # for prediction, use complete encoding model
      control = lapply(data, function(col) {
      fitlmer(col, target[[1]])
      })


      contrasts = switch(self$param_set$values$method,
        "one-hot" = function(x) stats::contr.treatment(x, contrasts = FALSE),
        treatment = stats::contr.treatment,
        helmert = stats::contr.helmert,
        poly = function(x) {
          cont = stats::contr.poly(x)
          rownames(cont) = x
          colnames(cont) = NULL
          cont
        },
        sum = stats::contr.sum,
        stop("Unknown 'method' parameter value.")
      )
      list(contrasts = lapply(levels, function(x) {
        con = contrasts(x)
        if (is.null(colnames(con))) {
          colnames(con) = as.character(seq_len(ncol(con)))
        }
        con
      }))
    },

    transform_dt = function(dt, levels) {
      cols = imap(self$state$contrasts, function(contrasts, id) {
        x = dt[[id]]
        contrasts[as.character(x), , drop = FALSE]
      })
      cols = as.data.table(cols)
      setnames(cols, names(cols), make.names(names(cols), unique = TRUE))
      cols
    }
  )
)

mlr_pipeops$add("encode", PipeOpEncode)




#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoLmerEncodeRegr() converts factor levels of each factorial column to the
#' estimated coefficients of a simple random intercept model.
#' Models are fitted with the lmer function of the lme4 package and are
#' of the type \code{target ~ 1 + (1 | factor)}. Thus, the numeric target
#' variable is used as dependent variable and the factor is used for grouping.
#' For training, multiple models can be estimated in a cross-validation scheme
#' to ensure that the same factor level does not always result in identical
#' values in the converted numerical feature.
#' For prediction, a global model (which was fitted on all observations
#' during training) is used for each factor.
#' New factor levels are converted to the value of the intercept coefficient
#' of the global model for prediction.
#' NAs are ignored by the CPO.
#'
#' @section CPOTrained State:
#' The state's \code{$control} slot is a named list of named vectors for each
#' factorial data column. Each vector contains the estimates for each factor level
#' from the global model fitted to that factor with all observations during training.
#'
#' @param n.folds [\code{integer(1)}]\cr
#'   Number of folds used in the cross-validation scheme during training.
#'   With \code{n.folds = 1} the single global model for each factor
#'   (which is used during predictions) is also used during training.
#'   Default is \code{5L}.
#' @param fast.optim [\code{logical}]\cr
#'   If \dQuote{fast.optim} is \code{TRUE} (default), a faster (up to 50 percent)
#'   optimizer from the nloptr package is used when fitting the lmer models.
#'   This uses additional stopping criteria which can give suboptimal results.
#' @template cpo_doc_outro
#' @export
cpoLmerEncodeRegr = makeCPOExtendedTrafo("lmer.encode.regr",
  pSS(n.folds = 5: integer [1, ], fast.optim = TRUE: logical),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  properties.target = "regr",
  packages = "lme4",
  fix.factors = FALSE,
  cpo.trafo = function(data, target, n.folds, fast.optim) {



    # for prediction, use complete encoding model
    control = lapply(data, function(col) {
      fitLmer(col, target[[1]])
      })

    # if n.folds == 1 use only the complete model in training
    if (n.folds == 1) {
      data[] = lapply(colnames(data), function(cname) {
        as.numeric(control[[cname]][as.character(data[[cname]])])
      })
      return(data)
    }

    # else use n.folds encoding models in crossvalidation scheme in training
    rinst = makeResampleInstance(makeResampleDesc("CV", iters = n.folds),
      size = nrow(data))
    # list with n.folds models for each data column
    mod_list = lapply(colnames(data), function(cname) {
      lapply(rinst$train.inds, function(inds) {
        fitLmer(data[[cname]][inds], target[[1]][inds])
        })
      })
    names(mod_list) = names(data)
    # list with replaced values in n.folds for each data column
    num_vals_list = lapply(colnames(data), function(cname) {
      lapply(seq_len(n.folds), function(fold) {
        # add new level for all values not present during training
        fac = flagNewLvls(data[[cname]][rinst$test.inds[[fold]]],
          names(mod_list[[cname]][[fold]]))
        mod = mod_list[[cname]][[fold]]

        as.numeric(mod[as.character(fac)])
        })
      })
    names(num_vals_list) = names(mod_list)
    # recombine replaced values from n.folds for each data column
    data[] = lapply(seq_along(data), function(cnum) {
      unlist(num_vals_list[[cnum]])[order(unlist(rinst$test.inds))]
      })
    data
  },
  cpo.retrafo = function(data, control, n.folds, fast.optim) {
    # use complete encoding model for new prediction data
    data[] = lapply(colnames(data), function(cname) {
      # add new level for all values not present during training
      fac = flagNewLvls(data[[cname]], names(control[[cname]]))

      as.numeric(control[[cname]][as.character(fac)])
      })
    data
  },
  cpo.retrafo = function(data, control, n.folds, fast.optim) {
    # use complete encoding model for new prediction data
    data[] = lapply(colnames(data), function(cname) {
      # add new level for all values not present during training
      fac = flagNewLvls(data[[cname]], names(control[[cname]]))

      as.numeric(control[[cname]][as.character(fac)])
      })
    data
  },
  cpo.retrafo = function(data, control, n.folds, fast.optim) {
    target_lvls = names(control[[1]])
    # use complete encoding model for new prediction data
    num_vals_list = sapply(colnames(data), function(cname) {
      # add new level for all values not present during training
      sapply(target_lvls, function(lvl) {
        fac = flagNewLvls(data[[cname]], names(control[[cname]][[lvl]]))

        as.numeric(control[[cname]][[lvl]][as.character(fac)])
        }, simplify = FALSE)
      }, simplify = FALSE)
    # return df with new feature names: feature_name.target_level
    # NOTE: special symbols in target levels (e.g. "-") are transformed to "."
    # by as.data.frame to allign with nameing rules for data.frame columns
    as.data.frame(num_vals_list, row.names = rownames(data))
  }

  )

#' @title Impact Encoding with Random Intercept Models
#'
#' @template cpo_doc_intro
#'

#'
#' @section CPOTrained State:
#' The state's \code{$control} slot is a named list of named vectors for each
#' factorial data column. Each vector contains the estimates for each factor level
#' from the global model fitted to that factor with all observations during training.
#'
#' @param n.folds [\code{integer(1)}]\cr
#'   Number of folds used in the cross-validation scheme during training.
#'   With \code{n.folds = 1} the single global model for each factor
#'   (which is used during predictions) is also used during training.
#'   Default is \code{5L}.
#' @param fast.optim [\code{logical}]\cr
#'   If \dQuote{fast.optim} is \code{TRUE} (default), a faster (up to 50 percent)
#'   optimizer from the nloptr package is used when fitting the glmer models.
#'   This uses additional stopping criteria which can give suboptimal results.
#' @template cpo_doc_outro
#' @export
cpoLmerEncodeTwoClassif = makeCPOExtendedTrafo("lmer.encode.two.classif",
  pSS(n.folds = 5: integer [1, ], fast.optim = TRUE: logical),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  properties.target = c("twoclass", "classif"),
  packages = "lme4",
  fix.factors = FALSE,
  cpo.trafo = function(data, target, n.folds, fast.optim) {


    # for prediction, use complete encoding model
    control = lapply(data, function(col) {
      fitGlmer(col, target[[1]])
      })

    # if n.folds == 1 use only the complete model in training
    if (n.folds == 1) {
      data[] = lapply(colnames(data), function(cname) {
        as.numeric(control[[cname]][as.character(data[[cname]])])
      })
      return(data)
    }

    # else use n.folds encoding models in crossvalidation scheme in training
    rinst = makeResampleInstance(makeResampleDesc("CV", iters = n.folds),
      size = nrow(data))
    # list with n.folds models for each data column
    mod_list = lapply(colnames(data), function(cname) {
      lapply(rinst$train.inds, function(inds) {
        fitGlmer(data[[cname]][inds], target[[1]][inds])
        })
      })
    names(mod_list) = names(data)
    # list with replaced values in n.folds for each data column
    num_vals_list = lapply(colnames(data), function(cname) {
      lapply(seq_len(n.folds), function(fold) {
        # add new level for all values not present during training
        fac = flagNewLvls(data[[cname]][rinst$test.inds[[fold]]],
          names(mod_list[[cname]][[fold]]))
        mod = mod_list[[cname]][[fold]]

        as.numeric(mod[as.character(fac)])
        })
      })
    names(num_vals_list) = names(mod_list)
    # recombine replaced values from n.folds for each data column
    data[] = lapply(seq_along(data), function(cnum) {
      unlist(num_vals_list[[cnum]])[order(unlist(rinst$test.inds))]
      })
    data
  },
)

#' @title Impact Encoding with Random Intercept Models
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoLmerMultiClassif() converts factor levels of each factorial column to the
#' estimated coefficients of simple random intercept models.
#' For each level of the multiclass target variable, binary "one vs. rest" models
#' are fitted with the glmer function of the lme4 package. Models are
#' of the type \code{target ~ 1 + (1 | factor)}. Thus, the binary "one vs. rest"
#' target variable for the respective level of the target variable is used as
#' dependent variable and the factor is used for grouping.
#' For training, multiple models can be estimated in a cross-validation scheme
#' to ensure that the same factor level does not always result in identical
#' values in the converted numerical features.
#' For prediction, a global model (which was fitted on all observations
#' during training) is used for each combination of factor and target level.
#' New factor levels are converted to the value of the intercept coefficient
#' of the global models for prediction.
#' NAs are ignored by the CPO.
#'
#' @section CPOTrained State:
#' The state's \code{$control} slot is a named nested list with named vectors for
#' each level of the target variable nested within each factorial data column.
#' Each vector contains the estimates for each factor level from the global model
#' fitted with all observations during training to the combination of factorial
#' feature and level of the target variable.
#'
#' @param n.folds [\code{integer(1)}]\cr
#'   Number of folds used in the cross-validation scheme during training.
#'   With \code{n.folds = 1} the single global model for each combination of
#'   factorial feature and level of the target variable
#'   (which is used during predictions) is also used during training.
#'   Default is \code{5L}.
#' @param fast.optim [\code{logical}]\cr
#'   If \dQuote{fast.optim} is \code{TRUE} (default), a faster (up to 50 percent)
#'   optimizer from the nloptr package is used when fitting the glmer models.
#'   This uses additional stopping criteria which can give suboptimal results.
#' @template cpo_doc_outro
#' @export
cpoLmerEncodeMultiClassif = makeCPOExtendedTrafo("lmer.encode.multi.classif",
  pSS(n.folds = 5: integer [1, ], fast.optim = TRUE: logical),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  properties.target = c("multiclass", "classif"),
  packages = "lme4",
  fix.factors = FALSE,
  cpo.trafo = function(data, target, n.folds, fast.optim) {


    # create list with binary "one vs. rest" target variables
    bin_targets = sapply(levels(target[[1]]), function(x) factor(x == target[[1]]),
      simplify = FALSE)

    # for prediction, use complete encoding model
    control = sapply(colnames(data), function(cname) {
      sapply(levels(target[[1]]), function(lvl) {
        fitGlmer(data[[cname]], bin_targets[[lvl]])
        }, simplify = FALSE)
      }, simplify = FALSE)

    # if n.folds == 1 use only the complete model in training
    if (n.folds == 1) {
      num_vals_list = sapply(colnames(data), function(cname) {
        sapply(levels(target[[1]]), function(lvl) {
          as.numeric(control[[cname]][[lvl]][as.character(data[[cname]])])
        }, simplify = FALSE)
      }, simplify = FALSE)
      # return df with new feature names: feature_name.target_level
      # NOTE: special symbols in target levels (e.g. "-") are transformed to "."
      # by as.data.frame to allign with nameing rules for data.frame columns
      return(as.data.frame(num_vals_list, row.names = rownames(data)))
    }

    # else, use n.folds encoding models in crossvalidation scheme in training
    rinst = makeResampleInstance(makeResampleDesc("CV", iters = n.folds),
      size = nrow(data))
    # list with n.folds models for each target level for each data column
    mod_list = sapply(colnames(data), function(cname) {
      sapply(levels(target[[1]]), function(lvl) {
        lapply(rinst$train.inds, function(inds) {
          fitGlmer(data[[cname]][inds], bin_targets[[lvl]][inds])
          })
        }, simplify = FALSE)
      }, simplify = FALSE)
    # list with replaced values in n.folds for each target level for each data column
    num_vals_list = sapply(colnames(data), function(cname) {
      sapply(levels(target[[1]]), function(lvl) {
        lapply(seq_len(n.folds), function(fold) {
        # add new level for all values not present during training
          fac = flagNewLvls(data[[cname]][rinst$test.inds[[fold]]],
            names(mod_list[[cname]][[lvl]][[fold]]))
          mod = mod_list[[cname]][[lvl]][[fold]]

          as.numeric(mod[as.character(fac)])
          })
        }, simplify = FALSE)
      }, simplify = FALSE)
    # recombine replaced values from n.folds for each data column
    num_vals_list = sapply(colnames(data), function(cname) {
      sapply(levels(target[[1]]), function(lvl) {
        unlist(num_vals_list[[cname]][[lvl]])[order(unlist(rinst$test.inds))]
        }, simplify = FALSE)
      }, simplify = FALSE)
    # return df with new feature names: feature_name.target_level
    # NOTE: special symbols in target levels (e.g. "-") are transformed to "."
    # by as.data.frame to allign with nameing rules for data.frame columns
    as.data.frame(num_vals_list, row.names = rownames(data))
  },
)




# heavily influenced by the embed package
fit_lmer_regr = function(feature, target) {
  # performance tips from https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html
  nlopt <- function(par, fn, lower, upper, control) {
  .nloptr <<- res <- nloptr::nloptr(par, fn, lb = lower, ub = upper,
    opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
    maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
  list(par = res$solution,
    fval = res$objective,
    conv = if (res$status > 0) 0 else res$status,
    message = res$message)
  }
  args = list(formula = y ~ 1 + (1 | lvl),
    data = data.frame(lvl = feature, y = target),
    na.action = na.omit,
    control = if (fast.optim) {
      lme4::lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
    } else {
      lme4::lmerControl()
    })
  mod = do.call(lme4::lmer, args)
  coefs = coef(mod)$lvl
  lvls = rownames(coefs)
  coefs = coefs[,1]
  names(coefs) = lvls
  intercept = unname(lme4::fixef(mod))
  # replace missing coefs with intercept value
  coefs[is.na(coefs)] = intercept
  # save intercept value for new levels during prediction
  coefs = c(coefs, ..new..level.. = intercept)
  coefs
}

# heavily influenced by the embed package
fit_lmer_binaryclass = function(feature, target) {
  # performance tips from https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html
  nlopt <- function(par, fn, lower, upper, control) {
  .nloptr <<- res <- nloptr::nloptr(par, fn, lb = lower, ub = upper,
    opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
    maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
  list(par = res$solution,
    fval = res$objective,
    conv = if (res$status > 0) 0 else res$status,
    message = res$message)
  }
  args = list(formula = y ~ 1 + (1 | lvl),
    data = data.frame(lvl = feature, y = target),
    family = stats::binomial,
    na.action = na.omit,
    control = if (fast.optim) {
      lme4::glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
    } else {
      lme4::glmerControl()
    })
  mod = do.call(lme4::glmer, args)
  coefs = coef(mod)$lvl
  lvls = rownames(coefs)
  coefs = coefs[,1]
  names(coefs) = lvls
  intercept = unname(lme4::fixef(mod))
  # replace missing coefs with intercept value
  coefs[is.na(coefs)] = intercept
  # save intercept value for new levels during prediction
  coefs = c(coefs, ..new..level.. = intercept)
  coefs
}

# heavily influenced by the embed package
fit_lmer_multiclass = function(feature, target) {
  # performance tips from https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html
  nlopt <- function(par, fn, lower, upper, control) {
  .nloptr <<- res <- nloptr::nloptr(par, fn, lb = lower, ub = upper,
    opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
    maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
  list(par = res$solution,
    fval = res$objective,
    conv = if (res$status > 0) 0 else res$status,
    message = res$message)
  }
  args = list(formula = y ~ 1 + (1 | lvl),
    data = data.frame(lvl = feature, y = target),
    family = stats::binomial,
    na.action = na.omit,
    control = if (fast.optim) {
      lme4::glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
    } else {
      lme4::glmerControl()
    })
  mod = do.call(lme4::glmer, args)
  coefs = coef(mod)$lvl
  lvls = rownames(coefs)
  coefs = coefs[,1]
  names(coefs) = lvls
  intercept = unname(lme4::fixef(mod))
  # replace missing coefs with intercept value
  coefs[is.na(coefs)] = intercept
  # save intercept value for new levels during prediction
  coefs = c(coefs, ..new..level.. = intercept)
  coefs
}

