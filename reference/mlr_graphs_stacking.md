# Create A Graph to Perform Stacking.

Create a new
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) for
stacking. A stacked learner uses predictions of several base learners
and fits a super learner using these predictions as features in order to
predict the outcome.

All input arguments are cloned and have no references in common with the
returned
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md).

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

[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)

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
#> initial  value 846.477289 
#> final  value 610.864302 
#> converged
#> # weights:  172
#> initial  value 417.567736 
#> final  value 407.847240 
#> converged
#> # weights:  172
#> initial  value 466.311391 
#> final  value 408.206169 
#> converged
#> # weights:  172
#> initial  value 498.245682 
#> final  value 405.651390 
#> converged
```
