# Create a bagging learner

Creates a
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) that
performs bagging for a supplied graph. This is done as follows:

- `Subsample` the data in each step using
  [`PipeOpSubsample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_subsample.md),
  afterwards apply `graph`

- Replicate this step `iterations` times (in parallel via
  [multiplicities](https://mlr3pipelines.mlr-org.com/dev/reference/Multiplicity.md))

- Average outputs of replicated `graph`s predictions using the
  `averager` (note that setting `collect_multiplicity = TRUE` is
  required)

All input arguments are cloned and have no references in common with the
returned
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md).

## Usage

``` r
pipeline_bag(graph, iterations = 10, frac = 1, averager = NULL, replace = TRUE)
```

## Arguments

- graph:

  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  \|
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)  
  A
  [`PipeOpLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner.md)
  or [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  to create a bagging pipeline for. Outputs from the replicated `graph`s
  are connected with the `averager`.

- iterations:

  `integer(1)`  
  Number of bagging iterations. Defaults to 10.

- frac:

  `numeric(1)`  
  Percentage of rows to keep during subsampling. See
  [`PipeOpSubsample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_subsample.md)
  for more information. Defaults to 1.

- averager:

  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  \|
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)  
  A
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  or [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  that averages the predictions from the replicated and subsampled
  graph's. In the simplest case, `po("classifavg")` and `po("regravg")`
  can be used in order to perform simple averaging of classification and
  regression predictions respectively. If `NULL` (default), no averager
  is added to the end of the graph. Note that setting
  `collect_multiplicity = TRUE` during construction of the averager is
  required.

- replace:

  `logical(1)`  
  Whether to sample with replacement. Default `TRUE`.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)

## Examples

``` r
# \donttest{
library(mlr3)
lrn_po = po("learner", lrn("regr.rpart"))
task = mlr_tasks$get("boston_housing")
gr = pipeline_bag(lrn_po, 3, averager = po("regravg", collect_multiplicity = TRUE))
resample(task, GraphLearner$new(gr), rsmp("holdout"))$aggregate()
#> regr.mse 
#> 17.32365 

# The original bagging method uses bootstrapping by sampling with replacement.
gr = ppl("bag", lrn_po,
  averager = po("regravg", collect_multiplicity = TRUE))
resample(task, GraphLearner$new(gr), rsmp("holdout"))$aggregate()
#> regr.mse 
#> 14.22536 
# }
```
