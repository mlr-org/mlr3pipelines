# Robustify a learner

Creates a
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) that
can be used to robustify any subsequent learner. Performs the following
steps:

- Drops empty factor levels using
  [`PipeOpFixFactors`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_fixfactors.md)

- Imputes `numeric` features using
  [`PipeOpImputeHist`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputehist.md)
  and
  [`PipeOpMissInd`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_missind.md)

- Imputes `factor` features using
  [`PipeOpImputeOOR`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeoor.md)

- Encodes `factors` using `one-hot-encoding`. Factors with a cardinality
  \> max_cardinality are collapsed using
  [`PipeOpCollapseFactors`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_collapsefactors.md)

The graph is built conservatively, i.e. the function always tries to
assure everything works. If a learner is provided, some steps can be
left out, i.e. if the learner can deal with factor variables, no
encoding is performed.

All input arguments are cloned and have no references in common with the
returned
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md).

## Usage

``` r
pipeline_robustify(
  task = NULL,
  learner = NULL,
  impute_missings = NULL,
  factors_to_numeric = NULL,
  max_cardinality = 1000,
  ordered_action = "factor",
  character_action = "factor",
  POSIXct_action = "numeric"
)
```

## Arguments

- task:

  [`Task`](https://mlr3.mlr-org.com/reference/Task.html)  
  A [`Task`](https://mlr3.mlr-org.com/reference/Task.html) to create a
  robustifying pipeline for. Optional, if omitted, the "worst possible"
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) is assumed and
  the full pipeline is created.

- learner:

  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)  
  A learner to create a robustifying pipeline for. Optional, if omitted,
  the "worst possible"
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) is
  assumed and a more conservative pipeline is built.

- impute_missings:

  `logical(1)` \| `NULL`  
  Should missing values be imputed? Defaults to `NULL`: imputes if the
  task has missing values (or factors that are not encoded to numerics)
  and the learner can not handle them.

- factors_to_numeric:

  `logical(1)` \| `NULL`  
  Should (ordered and unordered) factors be encoded? Defaults to `NULL`:
  encodes if the task has factors (or character columns that get
  converted to factor) and the learner can not handle factors.

- max_cardinality:

  `integer(1)`  
  Maximum number of factor levels allowed. See above. Default: 1000.

- ordered_action:

  `character(1)`  
  How to handle `ordered` columns: `"factor"` (default) or `"factor!"`:
  convert to `factor` columns; `"numeric"` or `"numeric!"`: convert to
  `numeric` columns; `"integer"` or `"integer!"`: convert to `integer`
  columns; `"ignore"` or `"ignore!"`: ignore. When `task` is given and
  has no `ordered` columns, or when `learner` is given and can handle
  `ordered`, then `"factor"`, `"numeric"` and `"integer"` are treated
  like `"ignore"`. This means it is necessary to add the exclamation
  point to override
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) or
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)
  properties when given. `"ignore"` and `"ignore!"` therefore behave
  completely identically, `"ignore!"` is only present for consistency.  
  When `ordered` features are converted to `factor`, then they are
  treated like `factor` features further down in the pipeline, and are
  possibly eventually converted to `numeric`s, but in a different way:
  `factor`s get one-hot encoded, `ordered_action` = `"numeric"` converts
  ordered using `as.numeric` to their integer-valued rank.

- character_action:

  `character(1)`  
  How to handle `character` columns: `"factor"` (default) or
  `"factor!"`: convert to `factor` columns; `"matrix"` or `"matrix!"`:
  Use
  [`PipeOpTextVectorizer`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_textvectorizer.md).
  `"ignore"` or `"ignore!"`: ignore. When `task` is given and has no
  `character` columns, or when `learner` is given and can handle
  `character`, then `"factor"` and `"matrix"` are treated like
  `"ignore"`. This means it is necessary to add the exclamation point to
  override [`Task`](https://mlr3.mlr-org.com/reference/Task.html) or
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)
  properties when given. `"ignore"` and `"ignore!"` therefore behave
  completely identically, `"ignore!"` is only present for consistency.  
  When `character` columns are converted to `factor`, then they are
  treated like `factor` further down in the pipeline, and are possibly
  eventually converted to `numeric`s, using one-hot encoding.

- POSIXct_action:

  `character(1)`  
  How to handle `POSIXct` columns: `"numeric"` (default) or
  `"numeric!"`: convert to `numeric` columns; `"datefeatures"` or
  `"datefeatures!"`: Use
  [`PipeOpDateFeatures`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_datefeatures.md).
  `"ignore"` or `"ignore!"`: ignore. When `task` is given and has no
  `POSIXct` columns, or when `learner` is given and can handle
  `POSIXct`, then `"numeric"` and `"datefeatures"` are treated like
  `"ignore"`. This means it is necessary to add the exclamation point to
  override [`Task`](https://mlr3.mlr-org.com/reference/Task.html) or
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)
  properties when given. `"ignore"` and `"ignore!"` therefore behave
  completely identically, `"ignore!"` is only present for consistency.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)

## Examples

``` r
# \donttest{
library(mlr3)
lrn = lrn("regr.rpart")
task = mlr_tasks$get("boston_housing")
gr = pipeline_robustify(task, lrn) %>>% po("learner", lrn)
resample(task, GraphLearner$new(gr), rsmp("holdout"))
#> 
#> ── <ResampleResult> with 1 resampling iterations ───────────────────────────────
#>         task_id
#>  boston_housing
#>                                                                        learner_id
#>  removeconstants_prerobustify.fixfactors.removeconstants_postrobustify.regr.rpart
#>  resampling_id iteration  prediction_test warnings errors
#>        holdout         1 <PredictionRegr>        0      0
# }
```
