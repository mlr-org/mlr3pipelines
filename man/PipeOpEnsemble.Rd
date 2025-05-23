% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PipeOpEnsemble.R
\name{PipeOpEnsemble}
\alias{PipeOpEnsemble}
\title{Ensembling Base Class}
\format{
Abstract \code{\link[R6:R6Class]{R6Class}} inheriting from \code{\link{PipeOp}}.
}
\description{
Parent class for \code{\link{PipeOp}}s that aggregate predictions. Implements the \code{private$.train()} and \code{private$.predict()} methods necessary
for a \code{PipeOp} and requires deriving classes to create the \code{private$weighted_avg_predictions()} function.
}
\section{Construction}{

Note: This object is typically constructed via a derived class, e.g. \code{\link{PipeOpClassifAvg}} or \code{\link{PipeOpRegrAvg}}.

\if{html}{\out{<div class="sourceCode">}}\preformatted{PipeOpEnsemble$new(innum = 0, collect_multiplicity = FALSE, id, param_set = ps(), param_vals = list(), packages = character(0), prediction_type = "Prediction")
}\if{html}{\out{</div>}}
\itemize{
\item \code{innum} :: \code{numeric(1)}\cr
Determines the number of input channels.
If \code{innum} is 0 (default), a vararg input channel is created that can take an arbitrary number of inputs.
\item \code{collect_multiplicity} :: \code{logical(1)}\cr
If \code{TRUE}, the input is a \code{\link{Multiplicity}} collecting channel. This means, a
\code{\link{Multiplicity}} input, instead of multiple normal inputs, is accepted and the members are aggregated. This requires \code{innum} to be 0.
Default is \code{FALSE}.
\item \code{id} :: \code{character(1)}\cr
Identifier of the resulting  object.
\item \code{param_set} :: \code{\link[paradox:ParamSet]{ParamSet}}\cr
("Hyper"-)Parameters in form of a \code{\link[paradox:ParamSet]{ParamSet}} for the resulting \code{\link{PipeOp}}.
\item \code{param_vals} :: named \code{list}\cr
List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default \code{list()}.
\item \code{packages} :: \code{character}\cr
Set of packages required for this \code{PipeOp}. These packages are loaded during \verb{$train()} and \verb{$predict()}, but not attached.
Default \code{character(0)}.
\item \code{prediction_type} :: \code{character(1)}\cr
The \code{predict} entry of the \verb{$input} and \verb{$output} type specifications.
Should be \code{"Prediction"} (default) or one of its subclasses, e.g. \code{"PredictionClassif"}, and correspond to the type accepted by
\code{private$.train()} and \code{private$.predict()}.
}
}

\section{Input and Output Channels}{

\code{PipeOpEnsemble} has multiple input channels depending on the \code{innum} construction argument, named \code{"input1"}, \code{"input2"}, ...
if \code{innum} is nonzero; if \code{innum} is 0, there is only one \emph{vararg} input channel named \code{"..."}.
All input channels take only \code{NULL} during training and take a \code{\link[mlr3:Prediction]{Prediction}} during prediction.

\code{PipeOpEnsemble} has one output channel named \code{"output"}, producing \code{NULL} during training and a \code{\link[mlr3:Prediction]{Prediction}} during prediction.

The output during prediction is in some way a weighted averaged representation of the input.
}

\section{State}{

The \verb{$state} is left empty (\code{list()}).
}

\section{Parameters}{

\itemize{
\item \code{weights} :: \code{numeric}\cr
Relative weights of input predictions. If this has length 1, it is ignored and weighs all inputs equally. Otherwise it must have
length equal to the number of connected inputs. Initialized to 1 (equal weights).
}
}

\section{Internals}{

The commonality of ensemble methods using \code{PipeOpEnsemble} is that they take a \code{NULL}-input during training and save an empty \verb{$state}. They can be
used following a set of \code{\link{PipeOpLearner}} \code{\link{PipeOp}}s to perform (possibly weighted) prediction averaging. See e.g.
\code{\link{PipeOpClassifAvg}} and \code{\link{PipeOpRegrAvg}} which both inherit from this class.

Should it be necessary to use the output of preceding \code{\link[mlr3:Learner]{Learner}}s
during the "training" phase, then \code{PipeOpEnsemble} should not be used. In fact, if training time behaviour of a \code{\link[mlr3:Learner]{Learner}} is important, then
one should use a \code{\link{PipeOpLearnerCV}} instead of a \code{\link{PipeOpLearner}}, and the ensemble can be created with a \code{\link[mlr3:Learner]{Learner}} encapsulated by a \code{\link{PipeOpLearner}}.
See \code{\link{LearnerClassifAvg}} and \code{\link{LearnerRegrAvg}} for examples.
}

\section{Fields}{

Only fields inherited from \code{\link{PipeOp}}.
}

\section{Methods}{

Methods inherited from \code{\link{PipeOp}} as well as:
\itemize{
\item \code{weighted_avg_prediction(inputs, weights, row_ids, truth)}\cr
(\code{list} of \code{\link[mlr3:Prediction]{Prediction}}, \code{numeric}, \code{integer} | \code{character}, \code{list}) -> \code{NULL}\cr
Create \code{\link[mlr3:Prediction]{Prediction}}s that correspond to the weighted average of incoming \code{\link[mlr3:Prediction]{Prediction}}s. This is
called by \code{private$.predict()} with cleaned and sanity-checked values: \code{inputs} are guaranteed to fit together,
\code{row_ids} and \code{truth} are guaranteed to be the same as each one in \code{inputs}, and \code{weights} is guaranteed to have the same length as \code{inputs}.\cr
This method is abstract, it must be implemented by deriving classes.
}
}

\seealso{
https://mlr-org.com/pipeops.html

Other PipeOps: 
\code{\link{PipeOp}},
\code{\link{PipeOpEncodePL}},
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
\code{\link{mlr_pipeops_vtreat}},
\code{\link{mlr_pipeops_yeojohnson}}

Other Multiplicity PipeOps: 
\code{\link{Multiplicity}()},
\code{\link{mlr_pipeops_classifavg}},
\code{\link{mlr_pipeops_featureunion}},
\code{\link{mlr_pipeops_multiplicityexply}},
\code{\link{mlr_pipeops_multiplicityimply}},
\code{\link{mlr_pipeops_ovrsplit}},
\code{\link{mlr_pipeops_ovrunite}},
\code{\link{mlr_pipeops_regravg}},
\code{\link{mlr_pipeops_replicate}}

Other Ensembles: 
\code{\link{mlr_learners_avg}},
\code{\link{mlr_pipeops_classifavg}},
\code{\link{mlr_pipeops_ovrunite}},
\code{\link{mlr_pipeops_regravg}}
}
\concept{Ensembles}
\concept{Multiplicity PipeOps}
\concept{PipeOps}
