# Create A Graph to Perform "One vs. Rest" classification.

Create a new
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) for
a [classification
Task](https://mlr3.mlr-org.com/reference/TaskClassif.html) to perform
"One vs. Rest" classification.

All input arguments are cloned and have no references in common with the
returned
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md).

## Usage

``` r
pipeline_ovr(graph)
```

## Arguments

- graph:

  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)  
  Graph being wrapped between
  [`PipeOpOVRSplit`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrsplit.md)
  and
  [`PipeOpOVRUnite`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrunite.md).
  The Graph should return `NULL` during training and a [classification
  Prediction](https://mlr3.mlr-org.com/reference/PredictionClassif.html)
  during prediction.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)

## Examples

``` r
library("mlr3")

task = tsk("wine")

learner = lrn("classif.rpart")
learner$predict_type = "prob"

# Simple OVR
g1 = pipeline_ovr(learner)
g1$train(task)
#> $ovrunite.output
#> NULL
#> 
g1$predict(task)
#> $ovrunite.output
#> 
#> ── <PredictionClassif> for 178 observations: ───────────────────────────────────
#>  row_ids truth response     prob.1     prob.2     prob.3
#>        1     1        1 0.93301153 0.04437722 0.02261125
#>        2     1        1 0.93301153 0.04437722 0.02261125
#>        3     1        1 0.93301153 0.04437722 0.02261125
#>      ---   ---      ---        ---        ---        ---
#>      176     3        3 0.08896784 0.04157376 0.86945841
#>      177     3        3 0.08896784 0.04157376 0.86945841
#>      178     3        3 0.01729144 0.04484461 0.93786395
#> 

# Bagged Learners
gr = po("replicate", reps = 3) %>>%
  po("subsample") %>>%
  learner %>>%
  po("classifavg", collect_multiplicity = TRUE)
g2 = pipeline_ovr(gr)
g2$train(task)
#> $ovrunite.output
#> NULL
#> 
g2$predict(task)
#> $ovrunite.output
#> 
#> ── <PredictionClassif> for 178 observations: ───────────────────────────────────
#>  row_ids truth response     prob.1     prob.2     prob.3
#>        1     1        1 0.94621803 0.03347464 0.02030733
#>        2     1        1 0.94621803 0.03347464 0.02030733
#>        3     1        1 0.94621803 0.03347464 0.02030733
#>      ---   ---      ---        ---        ---        ---
#>      176     3        3 0.08543923 0.03170632 0.88285445
#>      177     3        3 0.08543923 0.03170632 0.88285445
#>      178     3        3 0.04848767 0.03298737 0.91852495
#> 

# Bagging outside OVR
g3 = po("replicate", reps = 3) %>>%
  pipeline_ovr(po("subsample") %>>% learner) %>>%
  po("classifavg", collect_multiplicity = TRUE)
g3$train(task)
#> $classifavg.output
#> NULL
#> 
g3$predict(task)
#> $classifavg.output
#> 
#> ── <PredictionClassif> for 178 observations: ───────────────────────────────────
#>  row_ids truth response     prob.1     prob.2     prob.3
#>        1     1        1 0.95314545 0.03101051 0.01584404
#>        2     1        1 0.95314545 0.03101051 0.01584404
#>        3     1        1 0.95314545 0.03101051 0.01584404
#>      ---   ---      ---        ---        ---        ---
#>      176     3        3 0.05676624 0.02879565 0.91443811
#>      177     3        3 0.05676624 0.02879565 0.91443811
#>      178     3        3 0.01486811 0.02998338 0.95514851
#> 
```
