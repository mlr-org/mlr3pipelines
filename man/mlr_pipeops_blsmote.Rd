% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PipeOpBLSmote.R
\name{mlr_pipeops_blsmote}
\alias{mlr_pipeops_blsmote}
\alias{PipeOpBLSmote}
\title{BLSMOTE Balancing}
\format{
\code{\link[R6:R6Class]{R6Class}} object inheriting from \code{\link{PipeOpTaskPreproc}}/\code{\link{PipeOp}}.
}
\description{
Adds new data points by generating synthetic instances for the minority class using the Borderline-SMOTE algorithm.
This can only be applied to \link[mlr3:TaskClassif]{classification tasks} with numeric features that have no missing values.
See \code{\link[smotefamily:BLSMOTE]{smotefamily::BLSMOTE}} for details.
}
\section{Construction}{


\if{html}{\out{<div class="sourceCode">}}\preformatted{PipeOpBLSmote$new(id = "blsmote", param_vals = list())
}\if{html}{\out{</div>}}
\itemize{
\item \code{id} :: \code{character(1)}\cr
Identifier of resulting object, default \code{"smote"}.
\item \code{param_vals} :: named \code{list}\cr
List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default \code{list()}.
}
}

\section{Input and Output Channels}{

Input and output channels are inherited from \code{\link{PipeOpTaskPreproc}}. Instead of a \code{\link[mlr3:Task]{Task}}, a
\code{\link[mlr3:TaskClassif]{TaskClassif}} is used as input and output during training and prediction.

The output during training is the input \code{\link[mlr3:Task]{Task}} with added synthetic rows for the minority class.
The output during prediction is the unchanged input.
}

\section{State}{

The \verb{$state} is a named \code{list} with the \verb{$state} elements inherited from \code{\link{PipeOpTaskPreproc}}.
}

\section{Parameters}{

The parameters are the parameters inherited from \code{\link{PipeOpTaskPreproc}}, as well as:
\itemize{
\item \code{K} :: \code{numeric(1)} \cr
The number of nearest neighbors used for sampling from the minority class. Default is \code{5}.
See \code{\link[smotefamily:BLSMOTE]{BLSMOTE()}}.
\item \code{C} :: \code{numeric(1)} \cr
The number of nearest neighbors used for classifying sample points as SAFE/DANGER/NOISE. Default is \code{5}.
See \code{\link[smotefamily:BLSMOTE]{BLSMOTE()}}.
\item \code{dup_size} :: \code{numeric(1)} \cr
Desired times of synthetic minority instances over the original number of majority instances. \code{0} leads to balancing minority and majority class.
Default is \code{0}. See \code{\link[smotefamily:BLSMOTE]{BLSMOTE()}}.
\item \code{method} :: \code{character(1)} \cr
The type of Borderline-SMOTE algorithm to use. Default is \code{"type1"}.
See \code{\link[smotefamily:BLSMOTE]{BLSMOTE()}}.
\item \code{quiet} :: \code{logical(1)} \cr
Whether to suppress printing status during training. Initialized to \code{TRUE}.
}
}

\section{Internals}{

If a target level is unobserved during training, no synthetic data points will be generated for that class.
No error is raised; the unobserved class is simply ignored.
}

\section{Fields}{

Only fields inherited from \code{\link{PipeOp}}.
}

\section{Methods}{

Only methods inherited from \code{\link{PipeOpTaskPreproc}}/\code{\link{PipeOp}}.
}

\examples{
\dontshow{if (requireNamespace("smotefamily")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library("mlr3")

# Create example task
data = smotefamily::sample_generator(500, 0.8)
data$result = factor(data$result)
task = TaskClassif$new(id = "example", backend = data, target = "result")
task$head()
table(task$data(cols = "result"))

# Generate synthetic data for minority class
pop = po("blsmote")
bls_result = pop$train(list(task))[[1]]$data()
nrow(bls_result)
table(bls_result$result)
\dontshow{\}) # examplesIf}
}
\references{
Han H, Wang W, Mao B (2005).
\dQuote{Borderline-SMOTE: A New Over-Sampling Method in Imbalanced Data Sets Learning.}
In Huang D, Zhang X, Huang G (eds.), \emph{Advances in Intelligent Computing}, 878--887.
ISBN 978-3-540-31902-3, \doi{10.1007/11538059_91}.
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
\code{\link{mlr_pipeops_vtreat}},
\code{\link{mlr_pipeops_yeojohnson}}
}
\concept{PipeOps}
