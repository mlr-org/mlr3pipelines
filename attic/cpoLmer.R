
# #'
# #' @template cpo_doc_intro
# #'
# #' @description
# #' cpoLmerEncodeRegr() converts factor levels of each factorial column to the
# #' estimated coefficients of a simple random intercept model.
# #' Models are fitted with the lmer function of the lme4 package and are
# #' of the type \code{target ~ 1 + (1 | factor)}. Thus, the numeric target
# #' variable is used as dependent variable and the factor is used for grouping.
# #' For training, multiple models can be estimated in a cross-validation scheme
# #' to ensure that the same factor level does not always result in identical
# #' values in the converted numerical feature.
# #' For prediction, a global model (which was fitted on all observations
# #' during training) is used for each factor.
# #' New factor levels are converted to the value of the intercept coefficient
# #' of the global model for prediction.
# #' NAs are ignored by the CPO.
# #'
# #' @section CPOTrained State:
# #' The state's \code{$control} slot is a named list of named vectors for each
# #' factorial data column. Each vector contains the estimates for each factor level
# #' from the global model fitted to that factor with all observations during training.
# #'
# #' @param n.folds [\code{integer(1)}]\cr
# #'   Number of folds used in the cross-validation scheme during training.
# #'   With \code{n.folds = 1} the single global model for each factor
# #'   (which is used during predictions) is also used during training.
# #'   Default is \code{5L}.
# #' @param fast.optim [\code{logical}]\cr
# #'   If \dQuote{fast.optim} is \code{TRUE} (default), a faster (up to 50 percent)
# #'   optimizer from the nloptr package is used when fitting the lmer models.
# #'   This uses additional stopping criteria which can give suboptimal results.
# #' @template cpo_doc_outro
# #' @export
# cpoLmerEncodeRegr = makeCPOExtendedTrafo("lmer.encode.regr",
#   pSS(n.folds = 5: integer [1, ], fast.optim = TRUE: logical),
#   dataformat = "factor",
#   properties.data = c("missings", "numerics", "factors", "ordered"),
#   properties.adding = c("factors", "ordered"),
#   properties.needed = c("numerics", "missings.sometimes"),
#   properties.target = "regr",
#   packages = "lme4",
#   fix.factors = FALSE,
#   cpo.trafo = function(data, target, n.folds, fast.optim) {



#     # if n.folds == 1 use only the complete model in training
#     if (n.folds == 1) {
#       data[] = lapply(colnames(data), function(cname) {
#         as.numeric(control[[cname]][as.character(data[[cname]])])
#       })
#       return(data)
#     }

#     # else use n.folds encoding models in crossvalidation scheme in training
#     rinst = makeResampleInstance(makeResampleDesc("CV", iters = n.folds),
#       size = nrow(data))
#     # list with n.folds models for each data column
#     mod_list = lapply(colnames(data), function(cname) {
#       lapply(rinst$train.inds, function(inds) {
#         fitLmer(data[[cname]][inds], target[[1]][inds])
#         })
#       })
#     names(mod_list) = names(data)
#     # list with replaced values in n.folds for each data column
#     num_vals_list = lapply(colnames(data), function(cname) {
#       lapply(seq_len(n.folds), function(fold) {
#         # add new level for all values not present during training
#         fac = flagNewLvls(data[[cname]][rinst$test.inds[[fold]]],
#           names(mod_list[[cname]][[fold]]))
#         mod = mod_list[[cname]][[fold]]

#         as.numeric(mod[as.character(fac)])
#         })
#       })
#     names(num_vals_list) = names(mod_list)
#     # recombine replaced values from n.folds for each data column
#     data[] = lapply(seq_along(data), function(cnum) {
#       unlist(num_vals_list[[cnum]])[order(unlist(rinst$test.inds))]
#       })
#     data
#   },
#   cpo.retrafo = function(data, control, n.folds, fast.optim) {
#     # use complete encoding model for new prediction data
#     data[] = lapply(colnames(data), function(cname) {
#       # add new level for all values not present during training
#       fac = flagNewLvls(data[[cname]], names(control[[cname]]))

#       as.numeric(control[[cname]][as.character(fac)])
#       })
#     data
#   },
#   cpo.retrafo = function(data, control, n.folds, fast.optim) {
#     # use complete encoding model for new prediction data
#     data[] = lapply(colnames(data), function(cname) {
#       # add new level for all values not present during training
#       fac = flagNewLvls(data[[cname]], names(control[[cname]]))

#       as.numeric(control[[cname]][as.character(fac)])
#       })
#     data
#   },
#   cpo.retrafo = function(data, control, n.folds, fast.optim) {
#     target_lvls = names(control[[1]])
#     # use complete encoding model for new prediction data
#     num_vals_list = sapply(colnames(data), function(cname) {
#       # add new level for all values not present during training
#       sapply(target_lvls, function(lvl) {
#         fac = flagNewLvls(data[[cname]], names(control[[cname]][[lvl]]))

#         as.numeric(control[[cname]][[lvl]][as.character(fac)])
#         }, simplify = FALSE)
#       }, simplify = FALSE)
#     # return df with new feature names: feature_name.target_level
#     # NOTE: special symbols in target levels (e.g. "-") are transformed to "."
#     # by as.data.frame to allign with nameing rules for data.frame columns
#     as.data.frame(num_vals_list, row.names = rownames(data))
#   }

#   )

# #' @title Impact Encoding with Random Intercept Models
# #'
# #' @template cpo_doc_intro
# #'

# #'
# #' @section CPOTrained State:
# #' The state's \code{$control} slot is a named list of named vectors for each
# #' factorial data column. Each vector contains the estimates for each factor level
# #' from the global model fitted to that factor with all observations during training.
# #'
# #' @param n.folds [\code{integer(1)}]\cr
# #'   Number of folds used in the cross-validation scheme during training.
# #'   With \code{n.folds = 1} the single global model for each factor
# #'   (which is used during predictions) is also used during training.
# #'   Default is \code{5L}.
# #' @param fast.optim [\code{logical}]\cr
# #'   If \dQuote{fast.optim} is \code{TRUE} (default), a faster (up to 50 percent)
# #'   optimizer from the nloptr package is used when fitting the glmer models.
# #'   This uses additional stopping criteria which can give suboptimal results.
# #' @template cpo_doc_outro
# #' @export
# cpoLmerEncodeTwoClassif = makeCPOExtendedTrafo("lmer.encode.two.classif",
#   pSS(n.folds = 5: integer [1, ], fast.optim = TRUE: logical),
#   dataformat = "factor",
#   properties.data = c("missings", "numerics", "factors", "ordered"),
#   properties.adding = c("factors", "ordered"),
#   properties.needed = c("numerics", "missings.sometimes"),
#   properties.target = c("twoclass", "classif"),
#   packages = "lme4",
#   fix.factors = FALSE,
#   cpo.trafo = function(data, target, n.folds, fast.optim) {



#     # if n.folds == 1 use only the complete model in training
#     if (n.folds == 1) {
#       data[] = lapply(colnames(data), function(cname) {
#         as.numeric(control[[cname]][as.character(data[[cname]])])
#       })
#       return(data)
#     }

#     # else use n.folds encoding models in crossvalidation scheme in training
#     rinst = makeResampleInstance(makeResampleDesc("CV", iters = n.folds),
#       size = nrow(data))
#     # list with n.folds models for each data column
#     mod_list = lapply(colnames(data), function(cname) {
#       lapply(rinst$train.inds, function(inds) {
#         fitGlmer(data[[cname]][inds], target[[1]][inds])
#         })
#       })
#     names(mod_list) = names(data)
#     # list with replaced values in n.folds for each data column
#     num_vals_list = lapply(colnames(data), function(cname) {
#       lapply(seq_len(n.folds), function(fold) {
#         # add new level for all values not present during training
#         fac = flagNewLvls(data[[cname]][rinst$test.inds[[fold]]],
#           names(mod_list[[cname]][[fold]]))
#         mod = mod_list[[cname]][[fold]]

#         as.numeric(mod[as.character(fac)])
#         })
#       })
#     names(num_vals_list) = names(mod_list)
#     # recombine replaced values from n.folds for each data column
#     data[] = lapply(seq_along(data), function(cnum) {
#       unlist(num_vals_list[[cnum]])[order(unlist(rinst$test.inds))]
#       })
#     data
#   },
# )

# #' @title Impact Encoding with Random Intercept Models
# #'
# #' @template cpo_doc_intro
# #'
# #' @description
# #' cpoLmerMultiClassif() converts factor levels of each factorial column to the
# #' estimated coefficients of simple random intercept models.
# #' For each level of the multiclass target variable, binary "one vs. rest" models
# #' are fitted with the glmer function of the lme4 package. Models are
# #' of the type \code{target ~ 1 + (1 | factor)}. Thus, the binary "one vs. rest"
# #' target variable for the respective level of the target variable is used as
# #' dependent variable and the factor is used for grouping.
# #' For training, multiple models can be estimated in a cross-validation scheme
# #' to ensure that the same factor level does not always result in identical
# #' values in the converted numerical features.
# #' For prediction, a global model (which was fitted on all observations
# #' during training) is used for each combination of factor and target level.
# #' New factor levels are converted to the value of the intercept coefficient
# #' of the global models for prediction.
# #' NAs are ignored by the CPO.
# #'
# #' @section CPOTrained State:
# #' The state's \code{$control} slot is a named nested list with named vectors for
# #' each level of the target variable nested within each factorial data column.
# #' Each vector contains the estimates for each factor level from the global model
# #' fitted with all observations during training to the combination of factorial
# #' feature and level of the target variable.
# #'
# #' @param n.folds [\code{integer(1)}]\cr
# #'   Number of folds used in the cross-validation scheme during training.
# #'   With \code{n.folds = 1} the single global model for each combination of
# #'   factorial feature and level of the target variable
# #'   (which is used during predictions) is also used during training.
# #'   Default is \code{5L}.
# #' @param fast.optim [\code{logical}]\cr
# #'   If \dQuote{fast.optim} is \code{TRUE} (default), a faster (up to 50 percent)
# #'   optimizer from the nloptr package is used when fitting the glmer models.
# #'   This uses additional stopping criteria which can give suboptimal results.
# #' @template cpo_doc_outro
# #' @export
# cpoLmerEncodeMultiClassif = makeCPOExtendedTrafo("lmer.encode.multi.classif",
#   pSS(n.folds = 5: integer [1, ], fast.optim = TRUE: logical),
#   dataformat = "factor",
#   properties.data = c("missings", "numerics", "factors", "ordered"),
#   properties.adding = c("factors", "ordered"),
#   properties.needed = c("numerics", "missings.sometimes"),
#   properties.target = c("multiclass", "classif"),
#   packages = "lme4",
#   fix.factors = FALSE,
#   cpo.trafo = function(data, target, n.folds, fast.optim) {


#     # if n.folds == 1 use only the complete model in training
#     if (n.folds == 1) {
#       num_vals_list = sapply(colnames(data), function(cname) {
#         sapply(levels(target[[1]]), function(lvl) {
#           as.numeric(control[[cname]][[lvl]][as.character(data[[cname]])])
#         }, simplify = FALSE)
#       }, simplify = FALSE)
#       # return df with new feature names: feature_name.target_level
#       # NOTE: special symbols in target levels (e.g. "-") are transformed to "."
#       # by as.data.frame to allign with nameing rules for data.frame columns
#       return(as.data.frame(num_vals_list, row.names = rownames(data)))
#     }

#     # else, use n.folds encoding models in crossvalidation scheme in training
#     rinst = makeResampleInstance(makeResampleDesc("CV", iters = n.folds),
#       size = nrow(data))
#     # list with n.folds models for each target level for each data column
#     mod_list = sapply(colnames(data), function(cname) {
#       sapply(levels(target[[1]]), function(lvl) {
#         lapply(rinst$train.inds, function(inds) {
#           fitGlmer(data[[cname]][inds], bin_targets[[lvl]][inds])
#           })
#         }, simplify = FALSE)
#       }, simplify = FALSE)
#     # list with replaced values in n.folds for each target level for each data column
#     num_vals_list = sapply(colnames(data), function(cname) {
#       sapply(levels(target[[1]]), function(lvl) {
#         lapply(seq_len(n.folds), function(fold) {
#         # add new level for all values not present during training
#           fac = flagNewLvls(data[[cname]][rinst$test.inds[[fold]]],
#             names(mod_list[[cname]][[lvl]][[fold]]))
#           mod = mod_list[[cname]][[lvl]][[fold]]

#           as.numeric(mod[as.character(fac)])
#           })
#         }, simplify = FALSE)
#       }, simplify = FALSE)
#     # recombine replaced values from n.folds for each data column
#     num_vals_list = sapply(colnames(data), function(cname) {
#       sapply(levels(target[[1]]), function(lvl) {
#         unlist(num_vals_list[[cname]][[lvl]])[order(unlist(rinst$test.inds))]
#         }, simplify = FALSE)
#       }, simplify = FALSE)
#     # return df with new feature names: feature_name.target_level
#     # NOTE: special symbols in target levels (e.g. "-") are transformed to "."
#     # by as.data.frame to allign with nameing rules for data.frame columns
#     as.data.frame(num_vals_list, row.names = rownames(data))
#   },
# )


