% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PipeOpVtreat.R
\name{mlr_pipeops_vtreat}
\alias{mlr_pipeops_vtreat}
\alias{PipeOpVtreat}
\title{Interface to the vtreat Package}
\format{
\code{\link[R6:R6Class]{R6Class}} object inheriting from \code{\link{PipeOpTaskPreproc}}/\code{\link{PipeOp}}.
}
\description{
Provides an interface to the vtreat package.

\code{PipeOpVtreat} naturally works for \link[mlr3:TaskClassif]{classification tasks} and \link[mlr3:TaskRegr]{regression tasks}.
Internally, \code{PipeOpVtreat} follows the fit/prepare interface of vtreat, i.e., first creating a data treatment transform object via
\code{\link[vtreat:NumericOutcomeTreatment]{vtreat::NumericOutcomeTreatment()}}, \code{\link[vtreat:BinomialOutcomeTreatment]{vtreat::BinomialOutcomeTreatment()}}, or \code{\link[vtreat:MultinomialOutcomeTreatment]{vtreat::MultinomialOutcomeTreatment()}}, followed by calling
\code{\link[vtreat:fit_prepare]{vtreat::fit_prepare()}} on the training data and \code{\link[vtreat:prepare]{vtreat::prepare()}} during predicton.
}
\section{Construction}{


\if{html}{\out{<div class="sourceCode">}}\preformatted{PipeOpVreat$new(id = "vtreat", param_vals = list())
}\if{html}{\out{</div>}}
\itemize{
\item \code{id} :: \code{character(1)}\cr
Identifier of resulting object, default \code{"vtreat"}.
\item \code{param_vals} :: named \code{list}\cr
List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default \code{list()}.
}
}

\section{Input and Output Channels}{

Input and output channels are inherited from \code{\link{PipeOpTaskPreproc}}. Instead of a \code{\link[mlr3:Task]{Task}}, a
\code{\link[mlr3:TaskSupervised]{TaskSupervised}} is used as input and output during training and prediction.

The output is the input \code{\link[mlr3:Task]{Task}} with all affected features "prepared" by vtreat.
If vtreat found "no usable vars", the input \code{\link[mlr3:Task]{Task}} is returned unaltered.
}

\section{State}{

The \verb{$state} is a named \code{list} with the \verb{$state} elements inherited from \code{\link{PipeOpTaskPreproc}}, as well as:
\itemize{
\item \code{treatment_plan} :: object of class \code{vtreat_pipe_step} | \code{NULL}\cr
The treatment plan as constructed by vtreat based on the training data, i.e., an object of class \code{treatment_plan}.
If vtreat found "no usable vars" and designing the treatment would have failed, this is \code{NULL}.
}
}

\section{Parameters}{

The parameters are the parameters inherited from \code{\link{PipeOpTaskPreproc}}, as well as:
\itemize{
\item \code{recommended} :: \code{logical(1)}\cr
Whether only the "recommended" prepared features should be returned, i.e., non constant
variables with a significance value smaller than vtreat's threshold. Initialized to \code{TRUE}.
\item \code{cols_to_copy} :: \code{function} | \code{\link{Selector}} \cr
\code{\link{Selector}} function, takes a \code{\link[mlr3:Task]{Task}} as argument and returns a \code{character()} of features to copy.\cr
See \code{\link{Selector}} for example functions. Initialized to \code{selector_none()}.
\item \code{minFraction} :: \code{numeric(1)}\cr
Minimum frequency a categorical level must have to be converted to an indicator column.
\item \code{smFactor} :: \code{numeric(1)}\cr
Smoothing factor for impact coding models.
\item \code{rareCount} :: \code{integer(1)}\cr
Allow levels with this count or below to be pooled into a shared rare-level.
\item \code{rareSig} :: \code{numeric(1)}\cr
Suppress levels from pooling at this significance value greater.
\item \code{collarProb} :: \code{numeric(1)}\cr
What fraction of the data (pseudo-probability) to collar data at if \code{doCollar = TRUE}.
\item \code{doCollar} :: \code{logical(1)}\cr
If \code{TRUE} collar numeric variables by cutting off after a tail-probability specified by \code{collarProb} during treatment design.
\item \code{codeRestriction} :: \code{character()}\cr
What types of variables to produce.
\item \code{customCoders} :: named \code{list}\cr
Map from code names to custom categorical variable encoding functions.
\item \code{splitFunction} :: \code{function}\cr
Function taking arguments nSplits, nRows, dframe, and y; returning a user desired split.
\item \code{ncross} :: \code{integer(1)}\cr
Integer larger than one, number of cross-validation rounds to design.
\item \code{forceSplit} :: \code{logical(1)}\cr
If \code{TRUE} force cross-validated significance calculations on all variables.
\item \code{catScaling} :: \code{logical(1)}\cr
If \code{TRUE} use \code{\link[stats:glm]{stats::glm()}} linkspace, if FALSE use \code{\link[stats:lm]{stats::lm()}} for scaling.
\item \code{verbose} :: \code{logical(1)}\cr
If \code{TRUE} print progress.
\item \code{use_parallel} :: \code{logical(1)}\cr
If \code{TRUE} use parallel methods.
\item \code{missingness_imputation} :: \code{function}\cr
Function of signature f(values: numeric, weights: numeric), simple missing value imputer.\cr
Typically, an imputation via a \code{\link{PipeOp}} should be preferred, see \code{\link{PipeOpImpute}}.
\item \code{pruneSig} :: \code{numeric(1)}\cr
Suppress variables with significance above this level.
Only effects [regression tasks\link[mlr3:TaskRegr]{mlr3::TaskRegr} and binary \link[mlr3:TaskClassif]{classification tasks}.
\item \code{scale} :: \code{logical(1)}\cr
If \code{TRUE} replace numeric variables with single variable model regressions ("move to outcome-scale").
These have mean zero and (for variables with significant less than 1) slope 1 when regressed (lm for regression problems/glm for classification problems) against outcome.
\item \code{varRestriction} :: \code{list()}\cr
List of treated variable names to restrict to.
Only effects [regression tasks\link[mlr3:TaskRegr]{mlr3::TaskRegr} and binary \link[mlr3:TaskClassif]{classification tasks}.
\item \code{trackedValues} :: named \code{list()}\cr
Named list mapping variables to know values, allows warnings upon novel level appearances (see \code{\link[vtreat:track_values]{vtreat::track_values()}}).
Only effects [regression tasks\link[mlr3:TaskRegr]{mlr3::TaskRegr} and binary \link[mlr3:TaskClassif]{classification tasks}.
\item \code{y_dependent_treatments} :: \code{character()}\cr
Character what treatment types to build per-outcome level.
Only effects multiclass \link[mlr3:TaskClassif]{classification tasks}.
\item \code{imputation_map} :: named \code{list}\cr
List of map from column names to functions of signature f(values: numeric, weights: numeric), simple missing value imputers.\cr
Typically, an imputation via a \code{\link{PipeOp}} is to be preferred, see \code{\link{PipeOpImpute}}.
}

For more information, see \code{\link[vtreat:regression_parameters]{vtreat::regression_parameters()}}, \code{\link[vtreat:classification_parameters]{vtreat::classification_parameters()}}, or \code{\link[vtreat:multinomial_parameters]{vtreat::multinomial_parameters()}}.
}

\section{Internals}{

Follows vtreat's fit/prepare interface. See \code{\link[vtreat:NumericOutcomeTreatment]{vtreat::NumericOutcomeTreatment()}}, \code{\link[vtreat:BinomialOutcomeTreatment]{vtreat::BinomialOutcomeTreatment()}},
\code{\link[vtreat:MultinomialOutcomeTreatment]{vtreat::MultinomialOutcomeTreatment()}}, \code{\link[vtreat:fit_prepare]{vtreat::fit_prepare()}} and \code{\link[vtreat:prepare]{vtreat::prepare()}}.
}

\section{Fields}{

Only fields inherited from \code{\link{PipeOp}}.
}

\section{Methods}{

Only methods inherited from \code{\link{PipeOpTaskPreproc}}/\code{\link{PipeOp}}.
}

\examples{
\dontshow{if (requireNamespace("vtreat")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library("mlr3")

set.seed(2020)

make_data <- function(nrows) {
    d <- data.frame(x = 5 * rnorm(nrows))
    d["y"] = sin(d[["x"]]) + 0.01 * d[["x"]] + 0.1 * rnorm(nrows)
    d[4:10, "x"] = NA  # introduce NAs
    d["xc"] = paste0("level_", 5 * round(d$y / 5, 1))
    d["x2"] = rnorm(nrows)
    d[d["xc"] == "level_-1", "xc"] = NA  # introduce a NA level
    return(d)
}

task = TaskRegr$new("vtreat_regr", backend = make_data(100), target = "y")

pop = PipeOpVtreat$new()
pop$train(list(task))
\dontshow{\}) # examplesIf}
}
\seealso{
https://mlr-org.com/pipeops.html

Other PipeOps: 
\code{\link{PipeOp}},
\code{\link{PipeOpEncodePL}},
\code{\link{PipeOpEnsemble}},
\code{\link{PipeOpImpute}},
\code{\link{PipeOpTargetTrafo}},
\code{\link{PipeOpTaskPreproc}},
\code{\link{PipeOpTaskPreprocSimple}},
\code{\link{mlr_pipeops}},
\code{\link{mlr_pipeops_adas}},
\code{\link{mlr_pipeops_blsmote}},
\code{\link{mlr_pipeops_boxcox}},
\code{\link{mlr_pipeops_branch}},
\code{\link{mlr_pipeops_chunk}},
\code{\link{mlr_pipeops_classbalancing}},
\code{\link{mlr_pipeops_classifavg}},
\code{\link{mlr_pipeops_classweights}},
\code{\link{mlr_pipeops_colapply}},
\code{\link{mlr_pipeops_collapsefactors}},
\code{\link{mlr_pipeops_colroles}},
\code{\link{mlr_pipeops_copy}},
\code{\link{mlr_pipeops_datefeatures}},
\code{\link{mlr_pipeops_decode}},
\code{\link{mlr_pipeops_encode}},
\code{\link{mlr_pipeops_encodeimpact}},
\code{\link{mlr_pipeops_encodelmer}},
\code{\link{mlr_pipeops_encodeplquantiles}},
\code{\link{mlr_pipeops_encodepltree}},
\code{\link{mlr_pipeops_featureunion}},
\code{\link{mlr_pipeops_filter}},
\code{\link{mlr_pipeops_fixfactors}},
\code{\link{mlr_pipeops_histbin}},
\code{\link{mlr_pipeops_ica}},
\code{\link{mlr_pipeops_imputeconstant}},
\code{\link{mlr_pipeops_imputehist}},
\code{\link{mlr_pipeops_imputelearner}},
\code{\link{mlr_pipeops_imputemean}},
\code{\link{mlr_pipeops_imputemedian}},
\code{\link{mlr_pipeops_imputemode}},
\code{\link{mlr_pipeops_imputeoor}},
\code{\link{mlr_pipeops_imputesample}},
\code{\link{mlr_pipeops_kernelpca}},
\code{\link{mlr_pipeops_learner}},
\code{\link{mlr_pipeops_learner_pi_cvplus}},
\code{\link{mlr_pipeops_learner_quantiles}},
\code{\link{mlr_pipeops_missind}},
\code{\link{mlr_pipeops_modelmatrix}},
\code{\link{mlr_pipeops_multiplicityexply}},
\code{\link{mlr_pipeops_multiplicityimply}},
\code{\link{mlr_pipeops_mutate}},
\code{\link{mlr_pipeops_nearmiss}},
\code{\link{mlr_pipeops_nmf}},
\code{\link{mlr_pipeops_nop}},
\code{\link{mlr_pipeops_ovrsplit}},
\code{\link{mlr_pipeops_ovrunite}},
\code{\link{mlr_pipeops_pca}},
\code{\link{mlr_pipeops_proxy}},
\code{\link{mlr_pipeops_quantilebin}},
\code{\link{mlr_pipeops_randomprojection}},
\code{\link{mlr_pipeops_randomresponse}},
\code{\link{mlr_pipeops_regravg}},
\code{\link{mlr_pipeops_removeconstants}},
\code{\link{mlr_pipeops_renamecolumns}},
\code{\link{mlr_pipeops_replicate}},
\code{\link{mlr_pipeops_rowapply}},
\code{\link{mlr_pipeops_scale}},
\code{\link{mlr_pipeops_scalemaxabs}},
\code{\link{mlr_pipeops_scalerange}},
\code{\link{mlr_pipeops_select}},
\code{\link{mlr_pipeops_smote}},
\code{\link{mlr_pipeops_smotenc}},
\code{\link{mlr_pipeops_spatialsign}},
\code{\link{mlr_pipeops_subsample}},
\code{\link{mlr_pipeops_targetinvert}},
\code{\link{mlr_pipeops_targetmutate}},
\code{\link{mlr_pipeops_targettrafoscalerange}},
\code{\link{mlr_pipeops_textvectorizer}},
\code{\link{mlr_pipeops_threshold}},
\code{\link{mlr_pipeops_tomek}},
\code{\link{mlr_pipeops_tunethreshold}},
\code{\link{mlr_pipeops_unbranch}},
\code{\link{mlr_pipeops_updatetarget}},
\code{\link{mlr_pipeops_yeojohnson}}
}
\concept{PipeOps}
