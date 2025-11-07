# Create A Graph to Perform "One vs. Rest" classification.

Create a new
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) for a
[classification
Task](https://mlr3.mlr-org.com/reference/TaskClassif.html) to perform
"One vs. Rest" classification.

All input arguments are cloned and have no references in common with the
returned
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md).

## Usage

``` r
pipeline_ovr(graph)
```

## Arguments

- graph:

  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)  
  Graph being wrapped between
  [`PipeOpOVRSplit`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrsplit.md)
  and
  [`PipeOpOVRUnite`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrunite.md).
  The Graph should return `NULL` during training and a [classification
  Prediction](https://mlr3.mlr-org.com/reference/PredictionClassif.html)
  during prediction.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)

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
#>        1     1        1 0.93373174 0.05426314 0.01200512
#>        2     1        1 0.93373174 0.05426314 0.01200512
#>        3     1        1 0.93373174 0.05426314 0.01200512
#>      ---   ---      ---        ---        ---        ---
#>      176     3        3 0.26488350 0.04202947 0.69308703
#>      177     3        3 0.26488350 0.04202947 0.69308703
#>      178     3        3 0.03972996 0.05490238 0.90536767
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
#>  row_ids truth response    prob.1     prob.2      prob.3
#>        1     1        1 0.9472779 0.04835943 0.004362625
#>        2     1        1 0.9472779 0.04835943 0.004362625
#>        3     1        1 0.9472779 0.04835943 0.004362625
#>      ---   ---      ---       ---        ---         ---
#>      176     3        3 0.1513068 0.04032444 0.808368765
#>      177     3        3 0.1513068 0.04032444 0.808368765
#>      178     3        3 0.0132432 0.04666889 0.940087904
#> 
```
