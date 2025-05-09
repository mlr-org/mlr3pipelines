% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PipeOpCollapseFactors.R
\name{mlr_pipeops_collapsefactors}
\alias{mlr_pipeops_collapsefactors}
\alias{PipeOpCollapseFactors}
\title{Collapse Factors}
\format{
\code{\link[R6:R6Class]{R6Class}} object inheriting from \code{\link{PipeOpTaskPreprocSimple}}/\code{\link{PipeOpTaskPreproc}}/\code{\link{PipeOp}}.
}
\description{
Collapses factors of type \code{factor}, \code{ordered}: Collapses the rarest factors in the training samples, until \code{target_level_count}
levels remain. Levels that have prevalence strictly above \code{no_collapse_above_prevalence} or absolute count strictly above \code{no_collapse_above_absolute}
are retained, however. For \code{factor} variables, these are collapsed to the next larger level, for \code{ordered} variables, rare variables
are collapsed to the neighbouring class, whichever has fewer samples.
In case both \code{no_collapse_above_prevalence} and \code{no_collapse_above_absolute} are given, the less strict threshold of the two will be used, i.e. if
\code{no_collapse_above_prevalence} is 1 and \code{no_collapse_above_absolute} is 10 for a task with 100 samples, levels that are seen more than 10 times
will not be collapsed.

Levels not seen during training are not touched during prediction; Therefore it is useful to combine this with the
\code{\link{PipeOpFixFactors}}.
}
\section{Construction}{


\if{html}{\out{<div class="sourceCode">}}\preformatted{PipeOpCollapseFactors$new(id = "collapsefactors", param_vals = list())
}\if{html}{\out{</div>}}
\itemize{
\item \code{id} :: \code{character(1)}\cr
Identifier of resulting object, default \code{"collapsefactors"}.
\item \code{param_vals} :: named \code{list}\cr
List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default \code{list()}.
}
}

\section{Input and Output Channels}{

Input and output channels are inherited from \code{\link{PipeOpTaskPreproc}}.

The output is the input \code{\link[mlr3:Task]{Task}} with rare affected \code{factor} and \code{ordered} feature levels collapsed.
}

\section{State}{

The \verb{$state} is a named \code{list} with the \verb{$state} elements inherited from \code{\link{PipeOpTaskPreproc}}, as well as:
\itemize{
\item \code{collapse_map} :: named \code{list} of named \code{list} of \code{character}\cr
List of factor level maps. For each factor, \code{collapse_map} contains a named \code{list} that indicates what levels
of the input task get mapped to what levels of the output task. If \code{collapse_map} has an entry \code{feat_1} with
an entry \code{a = c("x", "y")}, it means that levels \code{"x"} and \code{"y"} get collapsed to level \code{"a"} in feature \code{"feat_1"}.
}
}

\section{Parameters}{

The parameters are the parameters inherited from \code{\link{PipeOpTaskPreproc}}, as well as:
\itemize{
\item \code{no_collapse_above_prevalence}  :: \code{numeric(1)} \cr
Fraction of samples below which factor levels get collapsed. Default is 1, which causes all levels
to be collapsed until \code{target_level_count} remain.
\item \code{no_collapse_above_absolute}  :: \code{integer(1)} \cr
Number of samples below which factor levels get collapsed. Default is \code{Inf}, which causes all levels
to be collapsed until \code{target_level_count} remain.
\item \code{target_level_count}  :: \code{integer(1)} \cr
Number of levels to retain. Default is 2.
}
}

\section{Internals}{

Makes use of the fact that \code{levels(fact_var) = list(target1 = c("source1", "source2"), target2 = "source2")} causes
renaming of level \code{"source1"} and \code{"source2"} both to \code{"target1"}, and also \code{"source2"} to \code{"target2"}.
}

\section{Fields}{

Only fields inherited from \code{\link{PipeOp}}.
}

\section{Methods}{

Only methods inherited from \code{\link{PipeOpTaskPreprocSimple}}/\code{\link{PipeOpTaskPreproc}}/\code{\link{PipeOp}}.
}

\examples{
library("mlr3")
op = PipeOpCollapseFactors$new()

# Create example training task
df = data.frame(
  target = runif(100),
  fct = factor(rep(LETTERS[1:6], times = c(25, 30, 5, 15, 5, 20))),
  ord = factor(rep(1:6, times = c(20, 25, 30, 5, 5, 15)), ordered = TRUE)
)
task = TaskRegr$new(df, target = "target", id = "example_train")

# Training
train_task_collapsed = op$train(list(task))[[1]]
train_task_collapsed$levels(c("fct", "ord"))

# Create example prediction task
df_pred = data.frame(
  target = runif(7),
  fct = factor(LETTERS[1:7]),
  ord = factor(1:7, ordered = TRUE)
)
pred_task = TaskRegr$new(df_pred, target = "target", id = "example_pred")

# Prediction
pred_task_collapsed = op$predict(list(pred_task))[[1]]
pred_task_collapsed$levels(c("fct", "ord"))
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
