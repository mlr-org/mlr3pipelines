# Create A Graph to Perform Stacking.

Create a new
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) for
stacking. A stacked learner uses predictions of several base learners
and fits a super learner using these predictions as features in order to
predict the outcome.

All input arguments are cloned and have no references in common with the
returned
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md).

## Usage

``` r
pipeline_stacking(
  base_learners,
  super_learner,
  method = "cv",
  folds = 3,
  use_features = TRUE
)
```

## Arguments

- base_learners:

  `list` of
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)  
  A list of base learners.

- super_learner:

  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)  
  The super learner that makes the final prediction based on the base
  learners.

- method:

  `character(1)`  
  `"cv"` (default) for building a super learner using cross-validated
  predictions of the base learners or `"insample"` for building a super
  learner using the predictions of the base learners trained on all
  training data.

- folds:

  `integer(1)`  
  Number of cross-validation folds. Only used for `method = "cv"`.
  Default 3.

- use_features:

  `logical(1)`  
  Whether the original features should also be passed to the super
  learner. Default `TRUE`.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)

## Examples

``` r
library(mlr3)
library(mlr3learners)

base_learners = list(
  lrn("classif.rpart", predict_type = "prob"),
  lrn("classif.nnet", predict_type = "prob")
)
super_learner = lrn("classif.log_reg")

graph_stack = pipeline_stacking(base_learners, super_learner)
graph_learner = as_learner(graph_stack)
graph_learner$train(tsk("german_credit"))
#> # weights:  172
#> initial  value 702.752783 
#> final  value 610.864302 
#> converged
#> # weights:  172
#> initial  value 417.427019 
#> iter  10 value 398.811803
#> iter  20 value 379.569823
#> iter  30 value 334.453177
#> iter  40 value 289.756050
#> iter  50 value 277.504280
#> iter  60 value 259.000169
#> iter  70 value 257.671612
#> iter  80 value 257.668808
#> iter  80 value 257.668807
#> iter  80 value 257.668807
#> final  value 257.668807 
#> converged
#> # weights:  172
#> initial  value 437.127855 
#> final  value 401.249484 
#> converged
#> # weights:  172
#> initial  value 466.609427 
#> iter  10 value 406.510158
#> final  value 406.510141 
#> converged
```
