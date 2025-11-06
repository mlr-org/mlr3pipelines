# Optimized Weighted Average of Features for Classification and Regression

Computes a weighted average of inputs. Used in the context of computing
weighted averages of predictions.

Predictions are averaged using `weights` (in order of appearance in the
data) which are optimized using nonlinear optimization from the package
[nloptr](https://CRAN.R-project.org/package=nloptr) for a measure
provided in `measure`. (defaults to `classif.ce` for `LearnerClassifAvg`
and `regr.mse` for `LearnerRegrAvg`). Learned weights can be obtained
from `$model`. This Learner implements and generalizes an approach
proposed in LeDell (2015) that uses non-linear optimization in order to
learn base-learner weights that optimize a given performance metric (e.g
`AUC`). The approach is similar but not exactly the same as the one
implemented as `AUC` in the
[SuperLearner](https://CRAN.R-project.org/package=SuperLearner) R
package (when `metric` is `"classif.auc"`). For a more detailed analysis
and the general idea, the reader is referred to LeDell (2015).

Note, that weights always sum to 1 by division by `sum(weights)` before
weighting incoming features.

## Usage

``` r
mlr_learners_classif.avg

mlr_learners_regr.avg
```

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`mlr3::LearnerClassif`](https://mlr3.mlr-org.com/reference/LearnerClassif.html)/[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html).

## Parameters

The parameters are the parameters inherited from
[`LearnerClassif`](https://mlr3.mlr-org.com/reference/LearnerClassif.html),
as well as:

- `measure` ::
  [`Measure`](https://mlr3.mlr-org.com/reference/Measure.html) \|
  `character`  
  [`Measure`](https://mlr3.mlr-org.com/reference/Measure.html) to
  optimize for. Will be converted to a
  [`Measure`](https://mlr3.mlr-org.com/reference/Measure.html) in case
  it is `character`. Initialized to `"classif.ce"`, i.e.
  misclassification error for classification and `"regr.mse"`, i.e. mean
  squared error for regression.

- `optimizer` ::
  [`Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html) \|
  `character(1)`  
  [`Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html) used
  to find optimal thresholds. If `character`, converts to
  [`Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html) via
  [`opt`](https://bbotk.mlr-org.com/reference/opt.html). Initialized to
  `OptimizerNLoptr`. Nloptr hyperparameters are initialized to
  `xtol_rel = 1e-8`, `algorithm = "NLOPT_LN_COBYLA"` and equal initial
  weights for each learner. For more fine-grained control, it is
  recommended to supply a instantiated
  [`Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html).

- `log_level` :: `character(1)` \| `integer(1)`  
  Set a temporary log-level for `lgr::get_logger("mlr3/bbotk")`.
  Initialized to: "warn".

## Methods

- `LearnerClassifAvg$new(), id = "classif.avg")`  
  (`chr`) -\> `self`  
  Constructor.

- `LearnerRegrAvg$new(), id = "regr.avg")`  
  (`chr`) -\> `self`  
  Constructor.

## References

LeDell, Erin (2015). *Scalable Ensemble Learning and Computationally
Efficient Variance Estimation*. Ph.D. thesis, UC Berkeley.

## See also

Other Learners:
[`mlr_learners_graph`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md)

Other Ensembles:
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_ovrunite`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrunite.md),
[`mlr_pipeops_regravg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_regravg.md)
